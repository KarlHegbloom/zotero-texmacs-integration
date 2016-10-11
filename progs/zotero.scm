;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;;; MODULE      : zotero.scm
;;; DESCRIPTION : Zotero Connector Plugin
;;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;;
;;; This software falls under the GNU general public license version 3 or
;;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;;
;;;;

(texmacs-module (zotero)
  (:use (kernel texmacs tm-modes)
        (kernel library content)
        (kernel library list)
        (utils base environment)
        (utils edit selections)
        (utils library cursor)
        (generic document-edit)
        (text text-structure)
        (generic document-part)
        (generic generic-edit)
        (generic format-edit)
        (convert tools sxml)))



;;; From (generic document-part):
;;;
;;; Perhaps these should be exported from there?
;;;
(define buffer-body-paragraphs (@@ (generic document-part) buffer-body-paragraphs))
;;;
;;; (buffer-get-part-mode)
;;;   modes are:  :preamble :all :several :one
;;; (buffer-test-part-mode? mode)
;;;
(define buffer-get-part-mode (@@ (generic document-part) buffer-get-part-mode))
(define buffer-test-part-mode? (@@ (generic document-part) buffer-test-part-mode?))
;;;
;;; When mode is :several or :one, then
;;;   (tree-in? (car (buffer-body-paragraphs)) '(show-part hide-part))
;;;    => #t
;;; When mode is :all, that will => #f
;;;
;;; (buffer-go-to-part id) id is a natural number beginning at 1, counting each
;;;   document part from the start of the document to the end.
;;;
;;; (buffer-show-part id)
;;; (buffer-toggle-part id)
;;;
(define buffer-show-preamble (@@ (generic document-part) buffer-show-preamble))
(define buffer-hide-preamble (@@ (generic document-part) buffer-hide-preamble))


;;; When parts of the document are hidden, which is what we do when the
;;; document is too large to easily edit, since TeXmacs slows way down to the
;;; point of becoming unusable when the document is larger and especially as
;;; it's structure becomes more complex... It defeats the purpose of hiding
;;; sections if the zcite or zbibiliography fields that are in those hidden
;;; sections are updated along with the rest of the document. It will be faster
;;; and easier to use when there are fewer for it to keep track of at a
;;; time... and so narrowing the view to only a single or a few sections will
;;; speed up the zcite turnaround time by reducing the amount of work that it
;;; has to do each time. For final document production, you must display all of
;;; the sections and then Zotero refresh it.
;;;
;;;
(define (zt-shown-buffer-body-paragraphs)
  (let ((l (buffer-body-paragraphs))) ;; list of tree
    (if (buffer-test-part-mode? :all)
        l
        (list-filter l (cut tree-is? <> 'show-part)))))
    


;;; With a very large bibliography, I had it stop with a Guile stack overflow. The manual for Guile-2.2 says that they've fixed the
;;; problem by making the stack dynamically extendable... but I think that this may still be required then because it's a setting
;;; designed more for checking programs that recurse "too deeply" rather than to guard against actual stack overflow.
;;;
;;; When it happened, it was not a crash, but instead was something inside of Guile-1.8 counting the stack depth, and throwing an
;;; error when the depth went past some default limit. Setting this to 0 removes the limit, and so if it runs out of stack this
;;; time, expect an operating system level crash or something... It depends on how the Scheme stack is allocated, and perhaps on
;;; per-user ulimit settings. (On Ubuntu, see: /etc/security/limits.conf owned by the libpam-modules package.) I don't know if
;;; ulimit settings affect available stack depth in this program. If you have a very large bibliography and it crashes TeXmacs, try
;;; extending your ulimit stack or heap limits.
;;;
(debug-set! stack 0)

(cond-expand
 (guile-2
  ;; In guile2 I think I'll need to set the port encoding.  There's still the problem where the part of TeXmacs that converts LaTeX
  ;; into TeXmacs assumes cork encoding rather than UTF-8. That is solved by using string-convert.
  )
 (else
   ;; My personal locale was already set to a UTF-8 one, and everything worked fine. The socket read and write routines are using u8
   ;; vectors, and so it ought to be fine with any encoding, since it's not trying to do any conversions at that layer. Just to be
   ;; sure though, I'm setting this here. If it causes problems for anyone, send me a github issue ticket.
   (define orig-locale-LC_ALL (setlocale LC_ALL))
   (define orig-locale-LC_CTYPE (setlocale LC_CTYPE))
   (unless (string-suffix? ".UTF-8" orig-locale-LC_CTYPE)
     (setlocale LC_CTYPE (string-append (substring orig-locale-LC_CTYPE
                                                   0
                                                   (string-index orig-locale-LC_CTYPE
                                                                 (string->char-set ".")))
                                        ".UTF-8")))))

;;; This copy of json was ported from Guile 2.0 to Guile 1.8 by Karl M. Hegbloom.
(use-modules (json))
(use-modules (ice-9 format))
(use-modules (ice-9 regex))
(use-modules (ice-9 common-list))

(use-modules (md5))
(define (md5-string str)
  (with-input-from-string str
    (md5)))


(tm-define (zt-format-error . args)
  (:secure)
  (apply format (cons (current-error-port) args)))


(define-public zt-debug-trace? #f)
;;; (define-public zt-debug-trace? #t)

(tm-define (zt-format-debug . args)
  (:secure)
  (when zt-debug-trace?
    (apply format (cons (current-output-port) args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Helper functions
;;;

(define (as-string obj)
  "Some arguments in this protocol can be number or string. Coerce to string."
  (cond
    ((number? obj)
     (number->string obj))
    ((tree? obj)
     (tree->string obj))
    ((string? obj) obj)
    (#t
     (zt-format-error "Error in zotero:as-string: ~s\n" obj)
     obj)))


;;; The documentID; this is not stable from one run to the next since it's value
;;; depends on whether this is the first document buffer upon launching TeXmacs
;;; or some subsequently loaded one. It does not have to be stable from one run
;;; to the next, but unique for each document being processed during this run.
;;;
;;; An alternative would be to assign it a stable value and store that in the
;;; init-env. It could be a UUID or a string formed just like the fieldID's are.
;;;
;;; I'm pretty sure that the LibreOffice document ID's are just sequential
;;; numbers counting from 0 each time the editor is launched. They identify it's
;;; internal document state management objects, which may actually be
;;; transcient, existing only during the duration of an "integration command"
;;; sequence.
;;;
;;; This should not be stored within the document since it changes each time
;;; TeXmacs is restarted and depending on whether the document is loaded first,
;;; second, etc.
;;;
(tm-define (zt-get-DocumentID)
  (string-append
   (if (defined? 'getpid)
       (as-string (getpid)) ;; ephemeral - don't store in document.
       (random 32768)) ;; ditto
   "-"
   (format #f "~a" (buffer-path))))

;;; There can be only 1 new field pending per document. For now I assume that we
;;; are editting only 1 document per instance of TeXmacs. If that changes, then
;;; this can become a hash table, or part of a per-document state management
;;; object.
;;;
(define-public zt-new-fieldID #f)

(tm-define (zt-get-new-fieldID)
  
  (as-string (create-unique-id)))

(tm-define (zt-field-refbinding-key fieldID)
  (string-append "zotero" fieldID "-noteIndex"))

;;; The set-binding call happens inside of the macro that renders the
;;; citation. I spent half a day figuring out how to write a glue-exported
;;; accessor function... then discovered this trick:
;;;
(tm-define (zt-get-refbinding key)
  (texmacs-exec `(get-binding ,key)))

;;; Modes
;;;
(texmacs-modes
    (in-tm-zotero-style% (style-has? "tm-zotero-dtd"))
    (in-zcite% (tree-func? (focus-tree) 'zcite)
               in-text% in-tm-zotero-style%)
    (in-zbibliography% (tree-func? (focus-tree) 'zbibliography)
                       in-text% in-tm-zotero-style%)
    (in-zfield% (or (in-zcite?) (in-zbibliography?)))
    (zt-can-edit-field% (and (in-zfield?)
                             (not
                              (and zt-new-fieldID
                                   (string=?
                                    zt-new-fieldID
                                    (as-string
                                     (zt-zfield-ID
                                      (focus-tree))))))))
    (in-ztHref% (tree-func? (focus-tree) 'ztHref)
                in-text% in-tm-zotero-style%))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Style sheet helper functions and tag accessors
;;;
(define zt-zfield-tags '(zcite zbibliography))


(tm-define (zt-insert-new-field tag placeholder)
  (if (not (in-zfield?))
      (let ((id-str (zt-get-new-fieldID)))
        (set! zt-new-fieldID id-str)
        (insert `(,tag ,id-str "" ,placeholder))
        ;; Perhaps add it to the cache here
        )
      (begin
        (zt-format-error "ERR: zt-insert-new-field ~s : focus-tree is a ~s\n"
                         tag (tree-label (focus-tree)))
        #f)))



;;XXX(define (

;;; This young code says that it wants to be a GOOPS object someday. I'm not
;;; sure if that's right for it yet.

;;; Operations on zcite fields.
;;;
;;;

;;; tm-find returns an incorrect result! Use tm-search.
;;;
(tm-define (zt-find-zfield fieldID)
  (car
   (tm-search
    (buffer-tree)
    (lambda (t)
      (and (tree-in? t zt-zfield-tags)
           (string=? fieldID
                     (as-string (zt-zfield-ID t))))))))


(tm-define (zt-go-to-zfield documentID fieldID)
  ;;
  ;; Ignore documentID for now, assuming work with focused document only.
  ;;
  ;; for now just tree-go-to, but perhaps look up the position from the cache
  ;; and jump to it without a tree-search having to happen first. (think "large
  ;; documents")
  ;;
  (tree-go-to (zt-find-zfield fieldID) 1))


;;; These must match the definitions in tm-zotero.ts;
;;;
;;;  L     0         1           2
;;; (zcite "fieldID" "fieldCode" "fieldText")
;;;
;;; fieldNoteIndex is gotten via a reference binding.
;;;
(tm-define (zt-zfield-ID t)
  (tree-ref t 0))


;;; the raw-data wrapper hopefully helps with the unicode conversion problems
(tm-define (zt-zfield-Code t)
  ;; upgrade old tags, also fixup hand-entered ones?
  (let ((code (tree-ref t 1)))
    (cond
      ((tm-func? code 'raw-data)
       (tree-ref code 0))
      ((and (not (tm-func? code 'raw-data))
            (tm-atomic? code))
       (tree-set! code (stree->tree `(raw-data ,(tree->stree code))))
       (tree-ref code 0))
      ((not (tm-func? code 'raw-data))
       (tree-set! code (stree->tree '(raw-data "")))
       (tree-ref code 0)))))

;; (tm-define (zt-zfield-Code t)
;;   (tree-ref t 1))


;;; For "note" styles, this reference binding links a citation field with
;;; the footnote number that it appears in.
;;;
(tm-define (zt-zfield-NoteIndex field)
  (zt-get-refbinding
   (zt-field-refbinding-key
    (as-string (zt-zfield-ID field)))))


;;; This next field is set automatically, below, with the result of converting
;;; the rich-text that Zotero sends back into a TeXmacs tree.
;;; 
;;;
;;; Todo: But what if I use drd-props to make it accessible so I can edit it,
;;;       and then do edit it? OpenOffice lets you edit them, but prompts you
;;;       that it's been editted before replacing it.
;;;
;;; Idea: When it's editted, perhaps a diff could be kept? Or some kind of
;;;       mechanism that finds out what is changed and sends it to Zotero?
;;;
;;;    A: I think that's not easy to do and more trouble than it's worth.
;;;       It's easier to just curate your reference collection to make it
;;;       produce what you want, right?
;;;
(tm-define (zt-zfield-Text t)
  (tree-ref t 2))


;;; I wanted to try and use the tm-select functionality to accomplish this, but
;;; found that it does not return the list in document-order!
;;;
;;; Scheme]  (define the-zfields-by-select (select (buffer-tree) '(:* (:or
;;; zcite zbibliography))))
;;;
;;; Scheme]  (map (lambda (t) (list (tree-label t) (tree->string (zt-zfield-ID
;;; t)))) the-zfields-by-select)
;;;
;;; ((zcite "+GHqJJmyDQVHhaO") (zcite "+28SL8xD6MxDCZI") (zcite
;;; "+JGeR0gQNwL2AKU") (zbibliography "+hoOIoDn3p6FB0H") (zcite
;;; "+6jApItmTysx1SJ") (zcite "+hoOIoDn3p6FB0G") (zcite "+JGeR0gQNwL2AKT")
;;; (zcite "+KUpBSz33QCflpE") (zcite "+KUpBSz33QCflpD") (zcite
;;; "+KUpBSz33QCflpC") (zcite "+KUpBSz33QCflpB") (zcite "+hoOIoDn3p6FB0J")
;;; (zcite "+hoOIoDn3p6FB0I"))
;;;
;;; Scheme]  (define the-zfields-by-tm-search (zt-get-zfields-list
;;; (zt-get-DocumentID) "ReferenceMark"))
;;;
;;; Scheme]  (map (lambda (t) (list (tree-label t) (tree->string (zt-zfield-ID
;;; t)))) the-zfields-by-tm-search)
;;;
;;; ((zcite "+6jApItmTysx1SJ") (zcite "+hoOIoDn3p6FB0G") (zcite
;;; "+JGeR0gQNwL2AKT") (zcite "+GHqJJmyDQVHhaO") (zcite "+KUpBSz33QCflpE")
;;; (zcite "+KUpBSz33QCflpD") (zcite "+28SL8xD6MxDCZI") (zcite
;;; "+KUpBSz33QCflpC") (zcite "+KUpBSz33QCflpB") (zcite "+hoOIoDn3p6FB0I")
;;; (zcite "+hoOIoDn3p6FB0J") (zcite "+JGeR0gQNwL2AKU") (zbibliography
;;; "+hoOIoDn3p6FB0H"))
;;;

(define (zt-zfield-search subtree)
  (tm-search
   subtree
   (lambda (t)
     (and (tree-in? t zt-zfield-tags)
          (not
           (and zt-new-fieldID
                (string=? (as-string
                           (zt-zfield-ID t))
                          zt-new-fieldID)))))))

(tm-define (zt-get-zfields-list documentID fieldType)
  ;;
  ;; Maybe ensure active document is documentID? For now assume it is.
  ;; Also for now assume fieldType is always "ReferenceMark", so ignore it.
  ;;
  ;; Q: What about "save-excursion" or "save-buffer-excursion"?
  ;; A: This is searching the document tree, not moving the cursor.
  ;;
  ;; What if I copy and paste a zcite from one location to another? The
  ;; zfield-ID of the second one will need to be updated.
  ;;
  (let* ((fields-tmp-ht (make-hash-table))
         (l (zt-shown-buffer-body-paragraphs))
         (all-fields (append-map zt-zfield-search l)))
      (set! zt-zfield-Code-cache (make-ahash-table))
      (let loop ((in all-fields)
                 (out '()))
        (cond
          ((null? in)
           (hash-for-each (lambda (key val)
                            (when (not (hash-ref fields-tmp-ht key #f))
                              (hash-remove! zt-zfield-Code-cache key)))
                          zt-zfield-Code-cache)
           (reverse! out))
          (else
            ;; fixup in case of copy + paste of zcite by tracking each ID and when one
            ;; is seen twice, change the second one.
            (let ((id-t (zt-zfield-ID (car in)))
                  (new-id ""))
              (if (hash-ref fields-tmp-ht (as-string id-t) #f)
                  (begin
                    (set! new-id (zt-get-new-fieldID))
                    (tree-set! id-t (stree->tree new-id))
                    (hash-set! fields-tmp-ht new-id #t)
                    (zt-get-zfield-Code-string (car in)));; caches zfield-Code
                  (hash-set! fields-tmp-ht (as-string id-t) #t))
              (loop (cdr in) (cons
                              (begin
                                (zt-get-zfield-Code-string (car in));; caches zfield-Code
                                (car in))
                              out))))))))
            



;;; The fieldCode is a JSON string. Among other things, it is how Zotero keeps
;;; track of whether or not the user has editted the automatically formatted
;;; citation text in the fieldText. When it has been editted, Zotero prompts
;;; first before overwriting it. By parsing that JSON and accessing that
;;; information ourself, we can render a red flag when it has been modified, to
;;; create a visual signal to the user. In order to make that happen, all
;;; setting and getting of the fieldCode must happen via these functions.
;;;
(define zt-zfield-Code-cache (make-ahash-table))

(tm-define (zt-get-zfield-Code-string field)
  (let ((id (as-string (zt-zfield-ID field)))
        (str_code (as-string (zt-zfield-Code field))))
    ;; So that Document_getFields causes this to happen.
    (when (and (not (hash-ref zt-zfield-Code-cache id #f))
               (not (and zt-new-fieldID
                         (string=? zt-new-fieldID id))))
      (zt-parse-and-cache-zfield-Code field str_code))
    str_code))

;;; Must handle empty string for zotero-Field_delete. Since it does not
;;; actually delete the tag from the document, it does not need to delete it
;;; from the cache.
;;;
;;; Also: What happens when I manually delete a zcite tag? How do I maintain the
;;; fieldCode and field positions cache?
;;;
;;; It is unlikely that a zcite will be manually deleted during the course of an
;;; integration command / editor-integration command sequence... Zotero is
;;; already designed to handle the case where you've manually removed a citation
;;; field... So these routines simply need to check that the field is really
;;; there before returning any cached information...
;;;
;;; What if I cut and paste a zcite from one location to another, and so the
;;; cached document position is no longer valid, but the zcite really is still
;;; in the document? For that case, I must fall back on a search of the document
;;; tree for a field with the sought-for fieldID, then update the cache.
;;;
(tm-define (zt-set-zfield-Code-from-string field str_code)
  (let ((code (zt-zfield-Code field))
        )
    (zt-parse-and-cache-zfield-Code field str_code)
    (tree-set! code (stree->tree str_code))))
    

;;; It goes through here so that this can also be called from the
;;; Document_getFields...
;;;
;;; Also handle empty string for zotero-Field_removeCode.
;;;
(tm-define (zt-parse-and-cache-zfield-Code field str_code)
  (let* ((id (as-string (zt-zfield-ID field)))
         (code (zt-zfield-Code field))
         (brace-idx (string-index str_code #\{))
         (str_json (and brace-idx
                        (substring str_code brace-idx)))
         (scm-code (and str_json
                        (safe-json-string->scm str_json))))
    (if (and (pair? scm-code)
             (string? (car scm-code))
             (>= (string-length (car scm-code)) 4)
             (string=? (string-take (car scm-code) 4) "ERR:"))
      (begin
        (zt-format-error
         "ERR:zt-parse-and-cache-zfield-Code: Invalid JSON? : ~s\n"
         (car scm-code))
        (noop)) ;; silent error?
      (cond
       ((or (string=? "" str_code); str_code => ""
            (not str_code)); str_code => #f
        (hash-remove! zt-zfield-Code-cache id))
       (scm-code
        (hash-set! zt-zfield-Code-cache id scm-code))))))


(tm-define (zt-get-zfield-Code-cache-ht-by-fieldID fieldIDstr)
  (hash-ref zt-zfield-Code-cache fieldIDstr #f))

;;(tm-define (zt-get-zfield-Code-cache-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; zt-ztbibItemRefs, \ztHrefFromCiteToBib{#zbibSysID1234}{text}
;;;
;;; List of refs / pagerefs to referring citations for the end of each
;;; bibliography entry. Compute them once, memoized, and so when the
;;; in-document tag is actually expanded, the operation is a fast hashtable
;;; lookup returning the pre-computed 'concat tree. The typesetter is run very
;;; often while using TeXmacs, and so if the full computation had to be run
;;; each time the tag is re-typeset (e.g. the user is typing on the page just
;;; above the zbibliography) it would be very slow.
;;;
;;;
(define zt-ztbibItemRefs-ht (make-ahash-table))

(define (zt-ztbibItemRefs-ht-reset!)
  (set! zt-ztbibItemRefs-ht (make-ahash-table)))



;;;
;;; Returns list of trees that are like:
;;;                        "zciteBibLabel" "displayed text"
;;;  '(ztHrefFromCiteToBib "#zbibSysID696" "text")
;;;
(define (zt-ztbibItemRefs-get-all-refs)
  (let ((refs (tm-search-tag (buffer-tree) 'ztHrefFromCiteToBib)))
    ;;(zt-format-debug "zt-ztbibItemRefs-get-all-refs:refs: ~S\n" (map tree->stree refs))
    refs))



;;;
;;; In each of the following, t is an element of the list returned by
;;; zt-ztbibItemRefs-get-all-refs and so it is a tree like
;;; '(ztHrefFromCiteToBib "#zbibSysID696" "text").
;;;
(define (zt-ztbibItemRefs-get-zciteBibLabel t)
  (as-string (tree-ref t 0)))


;;;
;;; Typically, this will be 1 to 4 characters of text without any special
;;; formatting inside of it. (Formatting may surround this chunk, but inside of
;;; it, there's not anything expected but an atomic string.
;;;
(define (zt-ztbibItemRefs-get-ztHrefFromCiteToBib-text t)
  (as-string (tree-ref t 1)))



(define zt-ztbibItemRefs-prefix-len (string-length "#zbibSysID"))
;;;
;;; This will be the hash key since the sysID is what's known to the macro
;;; being expanded after the end of each bibliography entry.
;;;
(define (zt-ztbibItemRefs-get-subcite-sysID t)
  (substring (zt-ztbibItemRefs-get-zciteBibLabel t)
             zt-ztbibItemRefs-prefix-len))


(define (zt-ztbibItemRefs-get-zfieldID t)
  (let loop ((t t))
    (cond
      ((eqv? t #f) "")
      ((tree-func? t 'zcite) (as-string (zt-zfield-ID t)))
      (else (loop (tree-outer t))))))



(define (zt-ztbibItemRefs-get-target-label t)
  (string-concatenate/shared
   (list "zciteID"
         (zt-ztbibItemRefs-get-zfieldID t)
         (zt-ztbibItemRefs-get-zciteBibLabel t))))

;;;
;;; For some reason there can be more than one the same in a citation cluster,
;;; probably only for parallel citations. Just for that, make sure the lists
;;; are uniq-equal? (since uniq uses memq, and this uses member, and we need to
;;; compare using equal? to make it recurse through list structure.
;;;
(define (uniq-equal? l)
  (let loop ((acc '())
             (l l))
    (if (null? l)
        (reverse! acc)
        (loop (if (member (car l) acc)
                  acc
                  (cons (car l) acc))
              (cdr l)))))

(define (zt-ztbibItemRefs-cache-1-zbibItemRef t)
  (let* ((key (zt-ztbibItemRefs-get-subcite-sysID t))
         (lst (and key (hash-ref zt-ztbibItemRefs-ht key '())))
         (new (and key `((hlink
                          ,(list 'zbibItemRef (zt-ztbibItemRefs-get-target-label t))
                          ,(string-concatenate/shared
                            (list "#" (zt-ztbibItemRefs-get-target-label t))))))))
    (hash-set! zt-ztbibItemRefs-ht key (append lst new))))



(define-public (zt-ztbibItemRefs-to-tree key)
  (let* ((lst1 (hash-ref zt-ztbibItemRefs-ht key #f))
         (lst (and lst1 (uniq-equal? lst1)))
         (first-item #t)
         (comma-like-sep (and lst
                              (apply append
                                     (map (lambda (elt)
                                            (if first-item
                                                (begin
                                                  (set! first-item #f)
                                                  (list elt))
                                                (begin
                                                  (list (list 'zbibItemRefsList-sep) elt))))
                                          lst))))
         (t (stree->tree (or (and comma-like-sep
                                  `(concat (zbibItemRefsList-left)
                                           ,@comma-like-sep
                                           (zbibItemRefsList-right)))
                             '(concat "")))))
    ;; (zt-format-debug "zt-ztbibItemRefs-to-tree:lst: ~S\n" lst)
    ;; (zt-format-debug "zt-ztbibItemRefs-to-tree:comma-sep: ~S\n" lst)
    ;; (zt-format-debug "zt-ztbibItemRefs-to-tree:t: ~S\n" (tree->stree t))
    t))



(define (zt-ztbibItemRefs-parse-all)
  ;; find all citations that reference sysID, list their pagerefs here.
  (zt-ztbibItemRefs-ht-reset!)
  (map zt-ztbibItemRefs-cache-1-zbibItemRef (zt-ztbibItemRefs-get-all-refs))
  (let ((keys '()))
    (hash-for-each (lambda (key val)
                     (when (not (string-suffix? "-t" (as-string key)))
                       (set! keys (append keys (list (as-string key))))))
                   zt-ztbibItemRefs-ht)
    ;;(zt-format-debug "zt-ztbibItemRefs-parse-all:keys: ~S\n" keys)
    (let loop ((keys keys))
      (cond
        ((null? keys) #t)
        (else
          (hash-set! zt-ztbibItemRefs-ht
                     (string-concatenate/shared (list (car keys) "-t"))
                     (zt-ztbibItemRefs-to-tree (car keys)))
          (loop (cdr keys)))))))




(tm-define (zt-ext-ztbibItemRefsList sysID)
  (:secure)
  (let* ((sysID (as-string sysID))
         (key-t (string-concatenate/shared (list sysID "-t"))))
    (cond
      ((hash-ref zt-ztbibItemRefs-ht key-t #f) => identity)
    (else
      (zt-ztbibItemRefs-parse-all)
      (hash-ref zt-ztbibItemRefs-ht key-t (stree->tree '(concat "")))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Memoization cache for zt-ext-flag-if-modified, fieldID -> boolean-modified?
;;;
(define zt-zfield-modified?-cache (make-hash-table))
(define zt-zfield-disactivated? (make-hash-table))

(tm-define (zt-get-orig-zfield-Text fieldID-str)
  (let ((scm-code (hash-ref zt-zfield-Code-cache fieldID-str #f)))
    (let* ((scm-code (or scm-code
                         ;; trigger parsing and caching of zfield-Code data.
                         (and (zt-get-zfield-Code-string (zt-find-zfield fieldID-str))
                              ;; access it.
                              (hash-ref zt-zfield-Code-cache fieldID-str #f))))
           (props (and scm-code
                       (hash-ref scm-code "properties" #f)))
           (plainCitation (and props
                               (hash-ref props "plainCitation" #f))))
      ;; (zt-format-debug
      ;;  "Debug:zt-get-orig-zfield-Text:fieldID-str:~s\n\nscm-code:~s\n\nprops:~s\n\nplainCitation:~s\n"
      ;;  fieldID-str scm-code props plainCitation)
      plainCitation)))

(define (zt-set-zfield-modified?! fieldID-str)
  (let* ((field (or (zt-find-zfield fieldID-str) #f))
         (text (and field (format #f "~s" (tree->stree (zt-zfield-Text field)))))
         (orig-text (and text (zt-get-orig-zfield-Text fieldID-str)))
         (zfield-modified? (and text
                                orig-text
                                (not (string=? text orig-text)))))
    ;; (zt-format-debug
    ;;  "Debug:zt-set-zfield-modified?!:fieldID-str:~s\n\ntext:~s\n\norig-text:~s\n\nzfield-modified?:~s\n"
    ;;  fieldID-str text orig-text zfield-modified?)
    (hash-set! zt-zfield-modified?-cache fieldID-str zfield-modified?)
    zfield-modified?))

(define (zt-zfield-modified?-or-undef fieldID-str)
  (hash-ref zt-zfield-modified?-cache fieldID-str 'undef))

;;; When the debug print statements are enabled and debugging is on, whenever I
;;; type anything in the same paragraph right after a citation, this function
;;; is run every time I press a key. So instead of it having to do all the work
;;; every time, it needs to memoize the answer... The field can not be modified
;;; unless the tag is disactivated first. This is good, since then the
;;; deactivation of the tag can trigger clearing the memoized status for this
;;; flag. So, see: notify-disactivated in this file.
;;;
;;; fieldID is handed in from the tm-zotero.ts and will be a tree. Also accept
;;; a string.
;;;
(tm-define (zt-ext-flag-if-modified fieldID)
  (:secure)
  (let ((fieldID-str (as-string fieldID)))
    (when (not (hash-ref zt-zfield-disactivated? fieldID-str #f))
      (case (zt-zfield-modified?-or-undef fieldID-str)
        ((undef)
         ;; (zt-format-debug "Debug:zt-ext-flag-if-modified:undef:~s\n" fieldID)
         (zt-set-zfield-modified?! fieldID-str)
         (zt-ext-flag-if-modified fieldID-str)) ;; tail-call
        ((#t)
         ;; (zt-format-debug "Debug: zt-ext-flag-if-modified: Field is modified: ~s\n" fieldID)
         '(concat (flag "Modified!" "red")))
        ((#f)
         ;; (zt-format-debug "Debug: zt-ext-flag-if-modified: Field is NOT modified: ~s\n" fieldID)
         '(concat (flag "Not Modified." "green")))))))


;;; Todo: Read the Google Keep note I made to myself regarding having it not include obtaining the rendering of the zbibliography
;;; when that is inside of a hidden section.

(tm-define (zt-ext-document-has-zbibliography?)
  (:secure)
  (let ((zbibs (tm-search-tag (buffer-tree) 'zbibliography)))
    (if (null? zbibs)
        "false"
        "true")))
    

;;; ztShowID
;;;
;;; I don't think this one will ever really show up, but just in case, I've
;;; defined it, so it can at least be observed when it occurs.
;;;
;;; "<span class=\"" + state.opt.nodenames[cslid] + "\" cslid=\"" + cslid + "\">" + str + "</span>"
;;;
;;; "\\ztShowID{#{state.opt.nodenames[cslid]}}{#{cslid}}{#{str}}"
;;;
(tm-define (zt-ext-ztShowID node cslid body)
  (:secure)
  (zt-format-debug "Debug:zt-ext-ztShowID: ~s ~s ~s\n" node clsid body)
  '(concat ""))


;;; zbibCitationItemID
;;;
;;; This is sent right after the \bibitem{bibtex_id} as
;;; \zbibCitationItemID{itemID}, where the itemID corresponds to the id inside
;;; of the zcite fieldCode JSON object. So the bibtex_id can be used to
;;; correlate the bibliographic entry with a BibTeX database if you like, and
;;; the itemID from this macro can be used to correlate this bibliography entry
;;; with each point in the document where it was cited. I have a few ideas
;;; about how I want to use this information... the obvious use is to decorate
;;; the citations for hyperlinking within the document.
;;;
;;; I want each citation to hyperlink to the bibliography entry corresponding
;;; to it, and the bibliography entry to hyperlink to any on-line source or
;;; perhaps to the Zotero.org entry or whatever; for legal cases, it should
;;; link to either Google Scholar or Casetext. For journal articles, it should
;;; link to a freely available source, or to Heinonline or something. Trailing
;;; after the normal bibliography entry then will be a sequence of
;;; pageref-labelled but linking to on-the-spot-locus back-links to each point
;;; of citation withing the body of the document. When a document does not have
;;; a bibliography, the footnote, endnote, or in-text citations themselves
;;; should link to the on-line source... and perhaps all of those fancy
;;; features should be parameterized for toggling them on and off.
;;;
;;; It occurs to me that in order to select which part of the text to wrap with
;;; a locus for the hyperlink, I'll either have to arbitrarily select the first
;;; word or two, or obtain semantic information from either the fieldCode JSON
;;; object (with the 
;;;
(tm-define (zt-ext-zbibCitationItemID sysID)
  (:secure)
  (zt-format-debug "Debug:STUB:zt-ext-zbibCitationItemID: ~s\n\n" sysID)
  '(concat ""))

(tm-define (zt-ext-bibitem key)
  (:secure)
  (zt-format-debug "Debug:STUB:zt-ext-bibitem: ~s\n" key)
  '(concat ""))



;;; DocumentData
;;;
;;; AFAIK the only pref that this program needs access to is noteType, and that
;;; access is read-only. The noteType is a document-wide setting, since it goes
;;; with the CSL stylesheet chosen. But it is also passed to
;;; Document_insertField, Document_convert (?), and Field_convert, so really
;;; it could be a per-field setting. I choose to make it document-wide.
;;;
;;; enum noteType
;;;
(define-public zotero-NOTE_IN_TEXT  0)
(define-public zotero-NOTE_FOOTNOTE 1)
(define-public zotero-NOTE_ENDNOTE  2)

;;;
;;; The rest of the DocumentData settings are "opaque" from the viewpoint of
;;; this interface. They control Zotero, not TeXmacs.
;;;
;;; All of them are set via the zotero controlled dialog. That dialog is
;;; displayed automatically when the document does not yet have
;;; zoteroDocumentData set, because at the start of the transaction, Zotero will
;;; call zotero-Document_getDocumentData, which returns null to Zotero unless
;;; it's been set. After setting it, the next thing Zotero sends is a
;;; zotero-Document_setDocumentData message. It can also be invoked by sending a
;;; zotero-setDocPrefs message, which will call zotero-Document_getDocumentData,
;;; then let you edit that in Zotero's dialog, and send it back with
;;; zotero-Document_setDocumentData. So from here, we never need to write the
;;; prefs by any means other than having Zotero set it.
;;;
;;; Perhaps a future iteration could provide initial hints based on the language
;;; of the document being editted? But that's sort of a global thing anyway, and
;;; setting the language takes only a few clicks.
;;;
;;; Access it from Guile with: (get-env "zotero-pref-noteType")
;;; Access it from TeXmacs with: <value|zotero-pref-noteType>


;;; Here's what the typical DocumentData looks like, parsed to sxml:
;;;
;;; (define zotero-sample-DocumentData-sxml
;;;   '(*TOP*
;;;     (data (@ (data-version "3") (zotero-version "4.0.29.9m75"))
;;;      (session (@ (id "gk3doRA9")))
;;;      (style (@ (id "http://juris-m.github.io/styles/jm-indigobook-in-text")
;;;                (locale "en-US")
;;;                (hasBibliography "1")
;;;                (bibliographyStyleHasBeenSet "0")))
;;;      (prefs
;;;       (pref (@ (name "citationTransliteration")       (value "en")))
;;;       (pref (@ (name "citationTranslation")           (value "en")))
;;;       (pref (@ (name "citationSort")                  (value "en")))
;;;       (pref (@ (name "citationLangPrefsPersons")      (value "orig")))
;;;       (pref (@ (name "citationLangPrefsInstitutions") (value "orig")))
;;;       (pref (@ (name "citationLangPrefsTitles")       (value "orig")))
;;;       (pref (@ (name "citationLangPrefsJournals")     (value "orig")))
;;;       (pref (@ (name "citationLangPrefsPublishers")   (value "orig")))
;;;       (pref (@ (name "citationLangPrefsPlaces")       (value "orig")))
;;;       (pref (@ (name "citationAffixes")
;;;                  (value "|||||||||||||||||||||||||||||||||||||||||||||||")))
;;;       (pref (@ (name "projectName")
;;;                  (value "Project:TeXmacsTesting")))
;;;       (pref (@ (name "extractingLibraryID")           (value "0")))
;;;       (pref (@ (name "extractingLibraryName")
;;;                  (value "No group selected")))
;;;       (pref (@ (name "fieldType")                   (value "ReferenceMark")))
;;;       (pref (@ (name "storeReferences")               (value "true")))
;;;       (pref (@ (name "automaticJournalAbbreviations") (value "true")))
;;;       (pref (@ (name "noteType")                      (value "0")))
;;;       (pref (@ (name "suppressTrailingPunctuation")   (value "true")))))))


;;; For now ignore documentID; assume it's always the active document anyway.  I
;;; think it's really just meant for a key to a table of document objects for
;;; keeping local state. For this application that state is in the actual
;;; document itself. Depending upon the way the documentID is formed, it could
;;; be used to obtain the buffer file name, document title, etc.
;;;
(define (zt-get-DocumentData documentID)
  (get-env "zoteroDocumentData"))

(define (zt-set-DocumentData documentID str_dataString)
  (set-init-env "zoteroDocumentData" str_dataString)
  (zt-set-init-env-zotero-prefs documentID str_dataString))


(define (zt-set-init-env-zotero-prefs documentID str_dataString)
  ;; (write (parse-xml (zt-get-DocumentData)))
  ;; (newline)
  (let ((zt-set-init-env-zotero-prefs-sub
         (lambda (prefix attr-list)
           (let loop ((attr-list attr-list))
                (cond
                  ((null? attr-list) #t)
                  (#t (set-init-env (string-append prefix (symbol->string
                                                           (caar attr-list)))
                                    (cadar attr-list))
                   (loop (cdr attr-list))))))))
    (let loop ((sxml (cdr (parse-xml str_dataString))))
         (cond
           ((null? sxml) #t)
           ((eq? 'data (sxml-name (car sxml)))
            (zt-set-init-env-zotero-prefs-sub "zotero-data-" (sxml-attr-list
                                                              (car sxml)))
            (loop (sxml-content (car sxml))))
           ((eq? 'session (sxml-name (car sxml)))
            (zt-set-init-env-zotero-prefs-sub "zotero-session-" (sxml-attr-list
                                                                 (car sxml)))
            (loop (cdr sxml)))
           ((eq? 'style (sxml-name (car sxml)))
            (zt-set-init-env-zotero-prefs-sub "zotero-style-" (sxml-attr-list
                                                               (car sxml)))
            (loop (cdr sxml)))
           ((eq? 'prefs (sxml-name (car sxml)))
            (loop (sxml-content (car sxml))))
           ((eq? 'pref (sxml-name (car sxml)))
            (set-init-env (string-append "zotero-pref-" (sxml-attr (car sxml) 'name))
                          (sxml-attr (car sxml) 'value))
            (when (string=? "noteType" (sxml-attr (car sxml) 'name))
              ;; The TeXmacs style language case statements can not test an
              ;; environment variable that is a string against any other
              ;; string... the string it's set to has to be "true" or "false"
              ;; to make boolean tests work. It can not check for "equals 0",
              ;; "equals 1", etc.
              (set-init-env "zotero-pref-noteType0" "false")
              (set-init-env "zotero-pref-noteType1" "false")
              (set-init-env "zotero-pref-noteType2" "false")
              (set-init-env (string-append "zotero-pref-noteType"
                                           (sxml-attr (car sxml) 'value)) "true"))
            (loop (cdr sxml)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Protocol between tm_zotero and ZoteroTeXmacsIntegration.js
;;;
;;; https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol
;;;
;;; The Firefox or Zotero Standalone process operates a server on port 23116,
;;; which the extension residing within TeXmacs connects to. All frames consist
;;; of a 32 bits specifying the transaction ID, a big-endian 32-bit integer
;;; specifying the length of the payload, and the payload itself, which is
;;; either UTF-8 encoded JSON or an unescaped string beginning with “ERR:”.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
(define (close-zt-zotero-socket-port!)
  (if (and (port? zt-zotero-socket-port)
           (not (port-closed? zt-zotero-socket-port)))
      (begin
        (close-port zt-zotero-socket-port)
        (set! zt-zotero-socket-port #f))))

;;; Idempotency: If this is reloaded while TeXmacs is running, close the port on reload.
;;; I often reload this file during development by having developer-mode turned on:
;;; (set! developer-mode? #t) is in ~/.TeXmacs/progs/my-init-texmacs.scm
;;; and then using the Debug -> Execute -> Evaluate scheme expression... menu to execute:
;;; (load-from-path "zotero.scm")
;;;
(when (and (defined? 'zt-zotero-socket-port)
           (port? zt-zotero-socket-port)
           (not (port-closed? zt-zotero-socket-port)))
  (close-port zt-zotero-socket-port) ;; free the IP port for re-use
  (set! zt-zotero-socket-port #f))   ;; should allow gc of the port object now.

(define zt-zotero-socket-port #f)
(define zt-zotero-socket-inet-texmacs-port-number 23117)
(define zt-zotero-socket-inet-zotero-port-number 23116)

(define (set-nonblocking sock)
  (fcntl sock F_SETFL (logior O_NONBLOCK
                              (fcntl sock F_GETFL))))

(define (set-blocking sock)
  (fcntl sock F_SETFL (logand (lognot O_NONBLOCK)
                              (fcntl sock F_GETFL))))



(define-public (get-logname)
  (or (getenv "LOGNAME")
      (getenv "USER")))


;;; From /usr/include/linux/tcp.h
(define TCP_NODELAY 1)


;;; Looking at the LibreOffice Integration plugin, I see that it's what opens up the TCP port that this talks to on Linux. That code
;;; does not check what OS it's running on first, and so I think that it opens the same TCP port on both Mac OS-X and Windows and so
;;; on those platforms, this program may already just work with no further programming required.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Todo: Support Mac OS-X.
;;;
;;; Notes: for Mac OS-X, they use a Unix domain pipe. Look first in:
;;;
;;;  /Users/Shared/.zoteroIntegrationPipe_$(logname)
;;;
;;; and then fall back on ${HOME}/.zoteroIntegrationPipe
;;;
;;; Start the Zotero first since it will remove the pipe file then recreate
;;; it... Handle that in case of Zotero restart. SIGPIPE.
;;;
;;; Just before it actually deletes the pipe file, it writes "Zotero
;;; shutdown\n" to it.
;;;
;;; It speaks exactly the same protocol over that pipe as Linux does over the
;;; TCP socket.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Todo: Support Windows
;;;
;;; On Windows, the Word plugin calls on Juris-M / Zotero by invoking firefox
;;; "via WM_COPYDATA rather than the command line".
;;;
;;; See: zotero/components/zotero-service.js:401
;;;
;;; -ZoteroIntegrationAgent
;;;
;;; -ZoteroIntegrationCommand
;;;
;;; -ZoteroIntegrationDocument
;;;

(define zt-os-x-integration-pipe-locations
  (list
   (string-concatenate `("/Users/Shared/.zoteroIntegrationPipe_" ,(get-logname)))
   (string-concatenate `(,(getenv "HOME") "/.zoteroIntegrationPipe"))))

(define (get-zt-zotero-socket-port!)
  (catch 'system-error
    (lambda ()
      (if (and (port? zt-zotero-socket-port)
               (not (port-closed? zt-zotero-socket-port)))
          zt-zotero-socket-port
          ;; (cond
          ;;   ((os-macos?)		;; Mac OS-X
          ;;    (set! zt-zotero-socket-port (socket PF_UNIX SOCK_STREAM 0))
          ;;    (cond
          ;;      ((or (and (file-exists? (first zt-os-x-integration-pipe-locations))
          ;;                (first zt-os-x-integration-pipe-locations))
          ;;           (and (file-exists? (second zt-os-x-integration-pipe-locations))
          ;;                (second zt-os-x-integration-pipe-locations)))
          ;;       => (lambda (p)
          ;;            (bind zt-zotero-socket-port AF_UNIX p)
          ;;            (connect zt-zotero-socket-port AF_UNIX p))
          ;;      (else
          ;;        (throw 'system-error "OS-X integration pipe not present")))) ;; Firefox not started yet?
          ;;    (setvbuf zt-zotero-socket-port _IOFBF)
          ;;    (set-blocking zt-zotero-socket-port)
          ;;    zt-zotero-socket-port
          ;;    )
          ;;   ((os-mingw?)                ;; Windows
          ;;    (throw 'unsupported-os "Unsupported OS - Need to implement support for Windows Zotero Integration.")
          ;;    )
          ;;   (else			;; Linux / Posix
          ;;
          ;;
          ;; I think that this IP port is open no matter what OS as long as the Zotero OpenOffice Integration plugin is installed. I
          ;; also think that it will work fine no matter what OS you are using. Needs to be tested on Windows and Mac OS-X.
          ;;
          (begin
            (set! zt-zotero-socket-port (socket PF_INET SOCK_STREAM 0))
            (setsockopt zt-zotero-socket-port SOL_SOCKET SO_REUSEADDR 1)
            (bind    zt-zotero-socket-port AF_INET INADDR_LOOPBACK 
                     zt-zotero-socket-inet-texmacs-port-number)
            (connect zt-zotero-socket-port AF_INET INADDR_LOOPBACK
                     zt-zotero-socket-inet-zotero-port-number)
            (setvbuf zt-zotero-socket-port _IOFBF)
            (setsockopt zt-zotero-socket-port IPPROTO_TCP TCP_NODELAY 1)
            (set-blocking zt-zotero-socket-port)
            zt-zotero-socket-port
            )))
    (lambda args
      (zt-format-error "ERR: Exception caught in get-zt-zotero-socket-port!: ~s\n" args)
      (close-port zt-zotero-socket-port)
      (set! zt-zotero-socket-port #f)
      (set! zotero-active? #f)
      (dialogue-window
       (zotero-display-alert
        (zt-get-DocumentID)
        (string-append "\\begin{center}\n"
                       "Exception caught in: "
                       "\\texttt{get-zt-zotero-socket-port!}\n\n"
                       "\\textbf{System Error:} " (caar (cdddr args)) "\n\n"
                       "Is Zotero running?\n\n"
                       "If so, then you may need to {\\em restart} Firefox\\\\\n"
                       "or Zotero Standalone.\n"
                       "\\end{center}\n")
        DIALOG_ICON_STOP
        DIALOG_BUTTONS_OK)
       (lambda (val)
         (noop))
       "System Error in get-zt-zotero-socket-port!")
      #f)))



(sigaction SIGPIPE (lambda (sig)
                     (set! zotero-active? #f)
                     (close-zt-zotero-socket-port!)))


(define (write-network-u32 value port)
  (let ((v (make-u32vector 1 0)))
    (u32vector-set! v 0 (htonl value))
    (uniform-vector-write v port)))

(define (read-network-u32 port)
  (let ((v (make-u32vector 1 0)))
    (uniform-vector-read! v port)
    (ntohl (u32vector-ref v 0))))


(define (zotero-write tid cmd)
  ;; (zt-format-debug "Debug: zotero-write: ~s ~s\n" tid cmd)
  (let ((zp (get-zt-zotero-socket-port!)))
    (catch 'system-error
      ;;; This writes raw bytes. The string can be UTF-8.
      (lambda ()
        (let* ((cmdv (list->u8vector (map char->integer
                                          (string->list cmd))))
               (len (u8vector-length cmdv)))
          (write-network-u32 tid zp)
          (write-network-u32 len zp)
          (uniform-vector-write cmdv zp)
          (force-output zp)))
      (lambda args
        (zt-format-error "ERR: System error in zotero-write: ~s ~s\n" tid cmd)
        (zt-format-error "ERR: Exception caught: ~s\n" args)
        (zt-format-error "ERR: Closing Zotero port!\n")
        (close-zt-zotero-socket-port!)
        (set! zotero-active #f)
        (dialogue-window
         (zotero-display-alert 
          (zt-get-DocumentID)
          (string-append "\\begin{center}\n"
                         "Exception caught in: "
                         "\\texttt{zotero-write}\n\n"
                         "\\textbf{System Error:} Is Zotero running?\n"
                         "\n"
                         "If so, then you may need to {\\em restart}"
                         "Firefox\\\\\n"
                         "or Zotero Standalone.\n\n"
                         "\\textbf{Closing Zotero port.}\n"
                         "\\end{center}\n")
          DIALOG_ICON_STOP
          DIALOG_BUTTONS_OK)
         (lambda (val)
           (noop)))
        #f))))


(define (zotero-read)
  (let ((zp (get-zt-zotero-socket-port!)))
    (catch 'system-error
      ;; This reads raw bytes. The string can be UTF-8.
      (lambda ()
        (let* ((tid (read-network-u32 zp))
               (len (read-network-u32 zp))
               (cmdv (make-u8vector len 0)))
          (uniform-vector-read! cmdv zp)
          (list tid len (list->string (map integer->char 
                                           (u8vector->list cmdv))))))
      (lambda args
        (zt-format-error "ERR: Exception caught in zotero-read: ~s\n" args)
        (list (or tid 0) (or len 666) (format #f "ERR: System error in zotero-read: ~s" args)))))) ;; return to zotero-listen


(define (safe-json-string->scm str)
  (catch 'json-invalid
    (lambda ()
      (json-string->scm str))
    (lambda args
      (zt-format-error "ERR: Exception caught from json-string->scm: ~s\n" args)
      ;; return to zotero-listen
      (list (format #f "ERR: Invalid JSON: ~s\n" str) '()))))


(define (safe-scm->json-string scm)
  (catch #t
    (lambda ()
      (scm->json-string scm))
    (lambda args
      (zt-format-error "ERR: Exception caught from scm->json-string: ~s\n" args)
      (zt-format-error "ERR: scm: ~s\n" scm)
      ;;
      ;; Return ERR: to caller, usually zotero-write, so send to Zotero.  That
      ;; will cause Zotero to initiate an error dialog and reset back to
      ;; Document_complete state.
      ;;
      (format #f (string-append "ERR: Error! "
                                "Exception caught from scm->json-string \n\n"
                                "Exception args: ~s\n\n"
                                "scm: ~s\n")
              args scm))))


(define zotero-active? #f)

;;;
;;; It's sort of a state machine; protocol is essentially synchronous, and user
;;; expects to wait while it finishes before doing anything else anyhow.
;;;
;;; When this is entered, one of the Integration commands has just been sent to
;;; Juris-M / Zotero. Zotero will call back and begin a word processing command
;;; sequence, culminating with Document_complete.
;;;
;;;
(define (zotero-listen)
  (set! zotero-active? #t)
  (with (counter wait) '(40 10)
    (delayed
      (:while zotero-active?)
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      ;; Only run when data is ready to be read...
      (when (char-ready? zt-zotero-socket-port)
        (with (tid len cmdstr) (zotero-read)
          ;; (zt-format-debug "Debug: tid:~s len:~s cmdstr:~s\n" tid len cmdstr)
          (if (> len 0)
              (with (editCommand args) (safe-json-string->scm cmdstr)
                ;; (zt-format-debug "Debug: ~s\n" (list editCommand (cons tid args)))
                (cond
                  ((and (>= (string-length editCommand) 4)
                        (string=? (string-take editCommand 4) "ERR:"))
                   (zotero-write tid editCommand) ;; send the error to Zotero
                   (set! counter 40)
                   (set! wait 10)) ;; keep listening
                  ((string=? editCommand "Document_complete")
                   (set-message "Zotero: Document complete." "Zotero integration")
                   (zotero-write tid (scm->json-string '()))
                   ;;(close-zt-zotero-socket-port!)
                   (set! wait 0)
                   (set! zotero-active? #f))
                  (#t
                   ;; Todo: This traps the event where there's a syntax or other error in the zotero.scm program itself, and send
                   ;; the ERR: message back to Zotero, and set! zotero-active? #f, etc. in an attempt to make it more robust, so
                   ;; that Firefox and TeXmacs don't both have to be restarted when this program doesn't work right?
                   ;;
                   ;; It did not work right. It just sits there and never prints the backtrace from the error to the terminal the
                   ;; way I expect, and so I can't debug it. Also, sending that ERR did not cause Juris-M to put up a dialog or
                   ;; anything so there's no indication of the error and the network protocol does not reset to the starting state
                   ;; anyway. Maybe the error condition needs to be noted and then handled with the next start of a command, so
                   ;; noted but zotero-active? left #t until after the error handling?
                   ;;
                   ;; JavaScript error: file:///home/karlheg/.mozilla/firefox/yj3luajv.default/extensions/jurismOpenOfficeIntegration@juris-m.github.io/components/zoteroOpenOfficeIntegration.js, line 323: TypeError: can't access dead object
                   ;;
                   ;(catch #t
                     ;(lambda ()
                       (apply (eval ;; to get the function itself
                               (string->symbol 
                                (string-append "zotero-"
                                               editCommand)))
                              (cons tid args))
                       ;)
                     ;(lambda args
                       ;(zotero-write tid (scm->json-string "ERR: TODO: Unspecified Error Caught."))
                       ;(set! zotero-active? #f)))
                   (set! counter 40)
                   (set! wait 10))))
              (begin
                ;; Sometimes when Firefox is stopped in the middle of it,
                ;; char-ready? returns #t but zotero-read does not read
                ;; anything... Perhaps look for eof-object?
                (set! counter (- counter 1))
                (when (<= counter 0)
                  (close-zt-zotero-socket-port!)
                  (set! wait 0) 
                  (set! zotero-active? #f)))))))))


;;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;;; Editor commands.
;;;
;;; See: zotero-menu.scm
;;; See: zotero-kbd.scm
;;;
(define (zt-call-zotero-integration-command cmd)
  (when (not zotero-active?) ;; one at a time only
    (set-message (string-append "Calling Zotero integration command: " cmd)
                 "Zotero Integration")
    (let ((zp (get-zt-zotero-socket-port!)))
      (if (and (port? zp)
               (catch 'system-error
                 (lambda ()
                   (zotero-write 0 (safe-scm->json-string cmd))
                   #t)
                 (lambda arg
                   #f))) ;; Firefox or Zotero Standalone not running?
          (begin
            (zotero-listen) ;; delayed, returns immediately.
            #t) ;; report successful initiation of integration command sequence
          (begin
            #f)))))



;;; ---------

(tm-define (zotero-addCitation)
  (unless zt-new-fieldID ;; one at a time only
    (try-modification
      (if (and (zt-insert-new-field 'zcite "{Citation}")
               (zt-call-zotero-integration-command "addCitation"))
          #t
          (begin
            (set! zt-new-fieldID #f)
            #f)))))
                

(tm-define (zotero-editCitation)
  (zt-call-zotero-integration-command "editCitation"))

;;; ---------

(tm-define (zotero-addBibliography)
  (unless zt-new-fieldID ;; one at a time only
    (try-modification
      ;; It is what Zotero expects. I'd expect to put {Bibliography} there.
      (if (and (zt-insert-new-field 'zbibliography "{Bibliography}")
               (zt-call-zotero-integration-command "addBibliography"))
          #t
          (begin
            (set! zt-new-fieldID #f)
            #f)))))


(tm-define (zotero-editBibliography)
  (zt-call-zotero-integration-command "editBibliography"))


;;; ---------


(tm-define (zotero-refresh)
  (zt-call-zotero-integration-command "refresh"))


;;; (tm-define (zotero-removeCodes)
;;;   (zt-call-zotero-integration-command "removeCodes"))


;;; ---------


(tm-define (zotero-setDocPrefs)
  (zt-call-zotero-integration-command "setDocPrefs"))



;;; The definition of "latex" style command shortcuts for "\zcite" (aliased
;;; also as "\zc") makes it easy to enter them with the keyboard only, not
;;; needing the menu. But when you kill and yank zcite fields, it does not
;;; automatically update them... but running "zotero-refresh" from the menu
;;; causes the update to happen. So for example, create a citation to several
;;; sources, then just below it, create another one containing at least one of
;;; the same sources as the first one. Now kill the second one and yank it back
;;; just above the first one. Now use the Zotero menu to "refresh", and you'll
;;; see that Zotero updates the "id" or "supra", switching them appropriately.
;;; I want it to do that automatically when I kill and yank. I know it requires
;;; using observers etc. but I'm not far enough along in my understanding of
;;; TeXmacs internals to do it just yet. I'm sure it's possible.

;;; notify-activated is probably not the exactly method I need for that... but
;;; it's close. This makes it refresh every time you disactivate and reactivate
;;; a zcite tag.

(tm-define (notify-activated t)
  (:require (and (in-tm-zotero-style?)
                 (in-zcite?)))
  (set-message "zcite activated." "Zotero Integration")
  ;;
  ;; When activating a zcite tag, call the same zotero-refresh called from the
  ;; Zotero menu. This does not happen when the tag is initially inserted,
  ;; since the LaTeX style hybrid shortcut command activates an insertion of
  ;; the entire tag in already activated state. So this routine is only called
  ;; on when the user has pressed Backspace or used the toolbar to disactivate
  ;; the tag, potentially editted it's accessible fields (zcite-Text), and then
  ;; re-activated it by pressing Enter or using the toolbar.
  ;;
  ;; If this routine is ever extended to do anything else special, consider
  ;; whether that initial insertion of the citation should do that special
  ;; thing as well.
  ;;
  ;; We only need to run the zotero-refresh when the contents of the zcite-Text
  ;; have been hand-modified while the tag was in the disactive state.
  ;;
  (let ((fieldID-str (as-string (zt-zfield-ID t))))
    (hash-remove! zt-zfield-disactivated? fieldID-str)
    (when (case (zt-zfield-modified?-or-undef t)
            ((undef)
             (zt-set-zfield-modified?! fieldID-str)) ;; returns boolean
            ((#t) #t)
            ((#f) #f))
      (zotero-refresh))))
                 

(tm-define (notify-disactivated t)
  (:require (and (in-tm-zotero-style?)
                 (in-zcite?)))
  (set-message "zcite disactivated." "Zotero integration")
  (let ((fieldID-str (as-string (zt-zfield-ID t))))
    (hash-set! zt-zfield-disactivated? fieldID-str #t)
    ;;
    ;; When the tag is disactivated, and the zcite-Text has not been modified
    ;; from the original text that was set by Juris-M / Zotero, then this
    ;; refresh will catch any modifications to the reference database made
    ;; there. So if you modify the reference database item for the citation
    ;; cluster of this zcite tag and disactivate the tag, you'll see the
    ;; zcite-Text update.
    ;;
    (when (not (eq? #t (zt-zfield-modified?-or-undef fieldID-str)));; might be 'undef
      (zotero-refresh))
    ;;
    ;; When the tag is disactivated, the user might hand-modify the
    ;; zfield-Text. In that case, the flag must be turned red to make it
    ;; visually apparent. The comparison is more expensive than the quick
    ;; lookup of a boolean, so that status is cached, but cleared here when the
    ;; tag is disactivated. It is done after the zotero-refresh since that will
    ;; update the contents of unmodified zfield-Text when the reference
    ;; database items for a zcite citation cluster have changed.
    ;;
    (hash-remove! zt-zfield-modified?-cache fieldID-str)))



;;; Preferences and Settings
;;;
;;; Some CSL styles define in-text citations, and others define note style ones
;;; that create either a footnote or an endnote, depending on which of those
;;; you select from the Zotero document preferences dialogue.  When you enter a
;;; citation while already inside of a footnote or an endnote when in either
;;; style, it's designed so that it won't create a footnote of a footnote or a
;;; footnote of an endnote; that is, that particular citation will be rendered
;;; as an in-text citation, but the noteIndex reference binding will be set
;;; appropriately since it really is inside of a footnote or endnote.
;;;
;;; This in-text or note style is a global setting, but when a note style is
;;; active, any individual citation can be forced to be in-text by the
;;; user. Zotero sends the noteType with every field update, but this program
;;; is not really using that for anything. My guess is that it's designed to
;;; cause it to perform lazy update of the field types for the LibreOffice
;;; integration.
;;;
;;; While learning about TeXmacs internals in order to setup the configurable
;;; settings here, I learned that: "standard-options" is about style packages
;;; loaded or not, and "parameter-show-in-menu?" is about parameters I might
;;; test for in "if" or "case", and set locally using a "with" wrapping a tag.
;;;
;;; Whether citations appear in-text or in footnotes or endnotes is not an
;;; option set by changing what style package is loaded, since it's necessary
;;; to allow in-text citations when the CSL style is for footnote or endnotes,
;;; in case the writer wants to override one, or in case the citation is being
;;; made while already inside of a manually-created footnote or endnote.
;;;
(tm-define (parameter-show-in-menu? l)
  (:require
   (and (or (in-zfield?)
            (in-ztHref?))
        ;; Never show these.
        (or (in? l (list "zotero-pref-noteType0"  ;; set by Zotero, in-text style
                         "zotero-pref-noteType1"  ;; set by Zotero, footnote style
                         "zotero-pref-noteType2"  ;; set by Zotero, endnote style
                         "zt-not-inside-note" ;; tm-zotero.ts internal only
                         "zt-in-footnote"
                         "zt-in-endnote"
                         "zt-not-inside-zbibliography"
                         "zt-option-this-zcite-in-text"
                         "zt-extra-surround-before"
                         "endnote-nr" "footnote-nr"
                         "zt-endnote" "zt-footnote"))
            ;; Sometimes the footnote related items belong here.
            (and (or (== (get-env "zotero-pref-noteType0") "true")
                     (and (or (== (get-env "zotero-pref-noteType1") "true")
                              (== (get-env "zotero-pref-noteType2") "true"))
                          (== (get-env "zt-option-this-zcite-in-text") "true")))
                 (in? l (list "footnote-sep" "page-fnote-barlen" "page-fnote-sep"))))))
  #f)


(tm-define (parameter-show-in-menu? l)
  (:require
   (and (in-zbibliography?)
        (in? l (list "zt-option-zbib-font-size"
                     "zt-bibliography-two-columns"
                     "ztbibSubHeadingVspace*"
                     "zt-link-BibToURL"
                     "zt-render-bibItemRefsLists"
                     "zbibItemRefsList-sep"
                     "zbibItemRefsList-left"
                     "zbibItemRefsList-right"))))
  #t)


(tm-define (parameter-choice-list var)
  (:require (and (in-zbibliography?)
                 (== var "zbibColumns")))
  (list "1" "2"))

(tm-define (parameter-choice-list var)
  (:require (and (in-zbibliography?)
                 (== var "zbibPageBefore")))
  (list "0" "1" "2"))


(tm-define (focus-tag-name l)
  (:require (in-zfield?))
  (case l
    (("zt-option-zbib-font-size")   "Bibliography font size")
    (("zbibColumns")                "Number of columns")
    (("zbibPageBefore")             "Page break or double page before?")
    (("ztbibSubHeadingVspace*")     "Vspace before ztbibSubHeading")
    (("zt-link-BibToURL")           "Link bibitem to URL?")
    (("zt-link-FromCiteToBib")      "Link from citation to bib item?")
    (("zt-render-bibItemRefsLists") "Render bib item refs lists?")
    (("zbibItemRefsList-sep")       "Refs list sep")
    (("zbibItemRefsList-left")      "Refs list surround left")
    (("zbibItemRefsList-right")     "Refs list surround right")
    (else
      (former l))))


(tm-define (customizable-parameters t)
  (:require (and (in-zcite?)
                 (!= (get-env "zotero-pref-noteType0") "true")
                 (or (== (get-env "zotero-pref-noteType1") "true")
                     (== (get-env "zotero-pref-noteType2") "true"))
                 (!= (get-env "zt-in-footnote") "true")
                 (!= (get-env "zt-in-endnote") "true")))
  (list (list "zt-option-this-zcite-in-text" "Force in-text?")
        ))


(tm-define (parameter-choice-list var)
  (:require (and (in-zcite?)
                 (== var "zt-option-this-zcite-in-text")))
  (list "true" "false"))


(tm-define (hidden-child? t i)
  (:require (in-zcite?))
  #f)
        

;;; Todo: go to next similar tag does not work right with zcite. Why?
;;; The following seems to have no effect...

;;; Ok, it might not be zcite; it might be everything. Tried with a \strong text block and got the same error.  Fails when there's
;;; only 1 \paragraph, but works when there's 2, but trying to go past last one gives same error.  I think this used to work, but
;;; now it does not. I can't fix it today.

;; (tm-define (similar-to lab)
;;   (:require (in-zcite?))
;;   (list 'zcite))

;; (tm-define (similar-to lab)
;;   (:require (in-zbibliography?))
;;   (list 'zbibliography))



(define (zt-notify-debug-trace var val)
  (set-message (string-append "zt-debug-trace? set to " val)
               "Zotero integration")
  (set! zt-debug-trace? (== val "on")))


(define-preferences
  ("zt-debug-trace?" "off" zt-notify-debug-trace)
  ("zt-pref-in-text-hrefs-as-footnotes"         "on"  ignore)
  ("zt-pref-in-text-hlinks-have-href-footnotes" "on"  ignore))



;;; Todo: See  update-document  at generic/document-edit.scm:341
;;;
;;; Maybe this should only happen from the Zotero menu?
;;;
(tm-define (update-document what)
  (:require (in-tm-zotero-style?))
  (delayed
    (:idle 1)
    (cursor-after
     (when (or (== what "all")
               (== what "bibliography"))
       (zotero-refresh)
       (zt-ztbibItemRefs-parse-all))
     (unless (== what "bibliography")
       (former what)))))



;;;
;;; Word Processor commands: Zotero -> TeXmacs
;;;
;;; Each sends: [CommandName, [Parameters,...]].
;;;
;;; The response is expected to be a JSON encoded payload, or the unquoted and
;;; unescaped string: ERR: Error string goes here
;;;
;;; Gets information about the client and the currently active
;;; document. documentID can be an integer or a string.
;;;
;;; ["Application_getActiveDocument", [int_protocolVersion]] -> [int_protocolVersion, documentID]
;;;
(tm-define (zotero-Application_getActiveDocument tid pv)
  (zotero-write tid (safe-scm->json-string (list pv (zt-get-DocumentID)))))



(define DIALOG_ICON_STOP 0)
(define DIALOG_ICON_NOTICE 1)
(define DIALOG_ICON_CAUTION 2)

(define DIALOG_BUTTONS_OK 0)
(define DIALOG_BUTTONS_OK_OK_PRESSED 1)

(define DIALOG_BUTTONS_OK_CANCEL 1)
(define DIALOG_BUTTONS_OK_CANCEL_OK_PRESSED 1)
(define DIALOG_BUTTONS_OK_CANCEL_CANCEL_PRESSED 0)

(define DIALOG_BUTTONS_YES_NO 2)
(define DIALOG_BUTTONS_YES_NO_YES_PRESSED 1)
(define DIALOG_BUTTONS_YES_NO_NO_PRESSED 0)

(define DIALOG_BUTTONS_YES_NO_CANCEL 3)
(define DIALOG_BUTTONS_YES_NO_CANCEL_YES_PRESSED 2)
(define DIALOG_BUTTONS_YES_NO_CANCEL_NO_PRESSED 1)
(define DIALOG_BUTTONS_YES_NO_CANCEL_CANCEL_PRESSED 0)


(tm-widget ((zotero-display-alert documentID str_Text int_Icon int_Buttons) cmd)
  (let ((text (tree->stree (latex->texmacs (parse-latex str_Text)))))
    (centered
      (hlist ((icon (list-ref (map %search-load-path
                                   '("icon-stop.png"
                                     "icon-notice.png"
                                     "icon-caution.png"))
                              int_Icon)) (noop))
             >> (texmacs-output `(document (very-large ,text))
                                `(style (tuple "generic"))))))
  (bottom-buttons >>> (cond
                        ((= int_Buttons DIALOG_BUTTONS_OK)
                         ("Ok"     (cmd DIALOG_BUTTONS_OK_OK_PRESSED)))
                        ((= int_Buttons DIALOG_BUTTONS_OK_CANCEL)
                         ("Ok"     (cmd DIALOG_BUTTONS_OK_CANCEL_OK_PRESSED))
                         ("Cancel" (cmd DIALOG_BUTTONS_OK_CANCEL_CANCEL_PRESSED)))
                        ((= int_Buttons DIALOG_BUTTONS_YES_NO)
                         ("Yes"    (cmd DIALOG_BUTTONS_YES_NO_YES_PRESSED))
                         ("No"     (cmd DIALOG_BUTTONS_YES_NO_NO_PRESSED)))
                        ((= int_Buttons DIALOG_BUTTONS_YES_NO_CANCEL)
                         ("Yes"    (cmd DIALOG_BUTTONS_YES_NO_CANCEL_YES_PRESSED))
                         ("No"     (cmd DIALOG_BUTTONS_YES_NO_CANCEL_NO_PRESSED))
                         ("Cancel" (cmd DIALOG_BUTTONS_YES_NO_CANCEL_CANCEL_PRESSED))))))


;;; Shows an alert.
;;;
;;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;;
(tm-define (zotero-Document_displayAlert tid documentID str_dialogText int_icon int_buttons)
  (dialogue-window (zotero-display-alert documentID str_dialogText int_icon int_buttons)
                   (lambda (val)
                     (zotero-write tid (safe-scm->json-string val)))
                   "Zotero Alert!"))


;;; Brings the document to the foreground. (For OpenOffice, this is a no-op on non-Mac systems.)
;;;
;;; ["Document_activate", [documentID]] -> null
;;;
(tm-define (zotero-Document_activate tid documentID)
  (zotero-write tid (safe-scm->json-string '())))


;;; Indicates whether a field can be inserted at the current cursor position.
;;;
;;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;;
(tm-define (zotero-Document_canInsertField tid documentID str_fieldType)
  (let ((ret (not
              (not
               (and (in-text?)
                    (not (in-math?))
                    (if (in-zfield?)
                        (let ((t (focus-tree)))
                          ;; (zt-format-debug "Debug:zotero-Document_canInsertField:in-zfield? => #t, (focus-tree) => ~s\n" t)
                          (or (and zt-new-fieldID
                                   (string=? zt-new-fieldID
                                             (as-string (zt-zfield-ID t))))
                              #f))
                        #t))))))
    (zotero-write tid (safe-scm->json-string ret))))



;;; Retrieves data string set by setDocumentData.
;;;
;;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;;
(tm-define (zotero-Document_getDocumentData tid documentID)
  (zotero-write tid (safe-scm->json-string (zt-get-DocumentData documentID))))



;;; Stores a document-specific persistent data string. This data
;;; contains the style ID and other user preferences.
;;;
;;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;;
(tm-define (zotero-Document_setDocumentData tid documentID str_dataString)
  (zt-set-DocumentData documentID str_dataString)
  (zotero-write tid (safe-scm->json-string '())))



;;; Indicates whether the cursor is in a given field. If it is, returns
;;; information about that field. Returns null, indicating that the cursor isn't
;;; in a field of this fieldType, or a 3 element array containing:
;;;
;;; fieldID, int or string, A unique identifier corresponding to this field.
;;;
;;; fieldCode, UTF-8 string, The code stored within this field.
;;;
;;; noteIndex, int, The number of the footnote in which this field resides, or 0
;;;                 if the field is not in a footnote.
;;;
;;; ["Document_cursorInField", [documentID, str_fieldType]] -> null || [fieldID, fieldCode, int_noteIndex]
;;;
(tm-define (zotero-Document_cursorInField tid documentID str_fieldType)
  (let ((ret
         (if (in-zfield?)
             (begin
               ;; (zt-format-debug "Debug:zotero-Document_cursorInField: in-zfield? => #t\n")
               (let* ((t (focus-tree))
                      (id (as-string (zt-zfield-ID t))))
                 (if (not (and zt-new-fieldID
                               (string=? zt-new-fieldID id)))
                     (begin
                       (list id
                             (zt-get-zfield-Code-string t)
                             (as-string (zt-zfield-NoteIndex t))))
                     '()))) ;; is the new field not finalized by Document_insertField
             '()))) ;; not tree-in? zt-zfield-tags
    (zotero-write tid (safe-scm->json-string ret))))



;;; Inserts a new field at the current cursor position. Because there has to be
;;; time for the typesetting to run in order for it to create the footnote
;;; number and set the reference bindings for the noteIndex, by the time this
;;; routine is being called by Zotero, TeXmacs must have already inserted the
;;; new field, but in a pending state, finalized by this.
;;;
;;; str_fieldType, either "ReferenceMark" or "Bookmark"
;;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;;
;;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]
;;;
;;; Ignore: str_fieldType
;;;
(tm-define (zotero-Document_insertField tid documentID str_fieldType int_noteType)
  (let ((field (zt-find-zfield zt-new-fieldID))
        (id zt-new-fieldID))
    (set! zt-new-fieldID #f)
    ;; (tree-go-to field 1)
    ;; Q: Is it useful to initialize the fieldCode to anything here?
    (zotero-write tid (safe-scm->json-string
                       (list id "" (as-string (zt-zfield-NoteIndex field)))))))

  

;;; Get all fields present in the document, in document order.
;;;
;;; str_fieldType is the type of field used by the document, either ReferenceMark or Bookmark
;;;
;;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]
;;;
;;;
;;; Todo: Perhaps use a hash table to memoize buffer positions for each field so
;;;       that after this, access is more like O(1) rather than O(n), assuming
;;;       hash lookup is faster than short list traversal with string
;;;       compare... but this is more than list traversal; it's buffer-tree
;;;       traversal; that's not the slow part though; typing is slow when the
;;;       document is complicated because of the O(n^2) box-tree to
;;;       document-tree ip (inverse path) search algorithm. Finding these fields
;;;       in the source document is really just straightforward recursive DAG
;;;       traversal, right?
;;;
;;; Lets get it working first, then option setting features next, then see if it
;;; needs this.
;;;
;;; A protocol trace watching the traffic between Libreoffice and Zotero shows
;;; that the BIBL field is also sent as one of the fields in this list.
;;;
(tm-define (zotero-Document_getFields tid documentID str_fieldType)
  (let ((ret
         (let loop ((zcite-fields (zt-get-zfields-list
                                   documentID str_fieldType))
                    (ids '()) (codes '()) (indx '()))
              (cond
                ((null? zcite-fields) (if (nnull? ids)
                                          (list (reverse! ids)
                                                (reverse! codes)
                                                (reverse! indx))
                                          '((0) ("TEMP") (0))))
                (#t
                 (let ((field (car zcite-fields)))
                   (loop (cdr zcite-fields)
                         (cons (as-string (zt-zfield-ID field)) ids)
                         (cons (zt-get-zfield-Code-string field) codes)
                         (cons (as-string (zt-zfield-NoteIndex
                                                  field)) indx))))))))
    (zotero-write tid (safe-scm->json-string ret))))



;;; ["Document_convert" ??? (TODO in documentation.)
;;;
;;; public void convert(ReferenceMark mark, String fieldType, int noteType)
;;;
;;; I think this is for OpenOffice to convert a document from using ReferenceMark fields to Bookmark ones.
;;; Maybe we could repurpose this for TeXmacs?  Better to make a new flag; and just ignore this one.
;;;
(tm-define (zotero-Document_convert tid . args)
  (zotero-write tid (safe-scm->json-string '())))



;;;
;;; ["Document_setBibliographyStyle", [documentID,
;;;                                    firstLineIndent, bodyIndent,
;;;                                    lineSpacing, entrySpacing,
;;;                                    arrayList, tabStopCount]]
;;;
;;; public void setBibliographyStyle(int firstLineIndent,
;;;                                  int bodyIndent,
;;;                                  int lineSpacing,
;;;    		                     int entrySpacing,
;;;                                  ArrayList<Number> arrayList,
;;;                                  int tabStopCount) {...}
;;;
;;; Sample: ["Document_setBibliographyStyle", [2,0,0,240,240,[],0]]
;;;
;;; The first argument is documentID. After that, they match up to the above
;;; Java method signature.
;;;
;;;
;;; From the Java program that extends LibreOffice for this integration:
;;;
;;; static final double MM_PER_100_TWIP = 25.4/1440*100;
;;;
;;;    1 twip = 1/20 * postscript point
;;;    1 twip = 0.05 point
;;;  100 twip = 1.76388888889 mm
;;;
;;; // first line indent
;;; styleProps.setPropertyValue("ParaFirstLineIndent",
;;;                             (int) (firstLineIndent*MM_PER_100_TWIP));
;;;
;;; // indent
;;; styleProps.setPropertyValue("ParaLeftMargin",
;;;                             (int) (bodyIndent*MM_PER_100_TWIP));
;;;
;;; // line spacing
;;; LineSpacing lineSpacingStruct = new LineSpacing();
;;; lineSpacingStruct.Mode = LineSpacingMode.MINIMUM;
;;; lineSpacingStruct.Height = (short) (lineSpacing*MM_PER_100_TWIP);
;;; styleProps.setPropertyValue("ParaLineSpacing", lineSpacingStruct);
;;;
;;; // entry spacing
;;; styleProps.setPropertyValue("ParaBottomMargin",
;;;                             (int) (entrySpacing*MM_PER_100_TWIP));
;;;
;;;
;;; I don't like this use of non-font-size-relative measurements. I wonder
;;; what font size they assume as the default?  I think that LibreOffice uses
;;; 12pt font as the default, and so I will assume that for the calculations
;;; here... That turns out to work perfectly.
;;;
;;;
;;; The default interline space in TeXmacs is 0.2fn.
;;;
;;; 12 texpt * 0.2 = 2.4 texpt. Multiply that times 100 gives 240, which appears
;;; to be the default line spacing (par-sep) and entry spacing.
;;;
;;; So 240 meas == 0.2 fn ?
;;;
;;; 240 twip => 1.00375 fn (fn in terms of texpt)
;;; 240 twip => 1 fn       (fn in terms of postscript point) !
;;;
;;; Hmmm... if I defined twip in terms of texpt, then 240 twip would be 1 fn
;;; with fn in terms of texpt.
;;;
;;; So 240 twip is single spaced, but we want to set the par-sep.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; From the CSL 1.0.1-Dev Specification Document, Options,
;;; Bibliography-specific options:
;;;
;;;   hanging-indent
;;;
;;;     If set to “true” (“false” is the default), bibliographic entries are
;;;     rendered with hanging-indents.
;;;
;;;   second-field-align
;;;
;;;     If set, subsequent lines of bibliographic entries are aligned along the
;;;     second field. With “flush”, the first field is flush with the
;;;     margin. With “margin”, the first field is put in the margin, and
;;;     subsequent lines are aligned with the margin. An example, where the
;;;     first field is <text variable="citation-number" suffix=". "/>:
;;;
;;;       9.  Adams, D. (2002). The Ultimate Hitchhiker's Guide to the
;;;           Galaxy (1st ed.).
;;;       10. Asimov, I. (1951). Foundation.
;;;
;;;   line-spacing
;;;
;;;     Specifies vertical line distance. Defaults to “1” (single-spacing), and
;;;     can be set to any positive integer to specify a multiple of the
;;;     standard unit of line height (e.g. “2” for double-spacing).
;;;
;;;   entry-spacing
;;;
;;;     Specifies vertical distance between bibliographic entries. By default
;;;     (with a value of “1”), entries are separated by a single additional
;;;     line-height (as set by the line-spacing attribute). Can be set to any
;;;     non-negative integer to specify a multiple of this amount.
;;;
;;;
;;; Display
;;;
;;;   The display attribute (similar the “display” property in CSS) may be used
;;;   to structure individual bibliographic entries into one or more text
;;;   blocks. If used, all rendering elements should be under the control of a
;;;   display attribute. The allowed values:
;;;
;;;     “block” - block stretching from margin to margin.
;;;
;;;     “left-margin” - block starting at the left margin. If followed by a
;;;     “right-inline” block, the “left-margin” blocks of all bibliographic
;;;     entries are set to a fixed width to accommodate the longest content
;;;     string found among these “left-margin” blocks. In the absence of a
;;;     “right-inline” block the “left-margin” block extends to the right
;;;     margin.
;;;
;;;     “right-inline” - block starting to the right of a preceding
;;;     “left-margin” block (behaves as “block” in the absence of such a
;;;     “left-margin” block). Extends to the right margin.
;;;
;;;     “indent” - block indented to the right by a standard amount. Extends to
;;;     the right margin.
;;;
;;;   Examples
;;;
;;;     Instead of using second-field-align (see Whitespace), a similar layout
;;;     can be achieved with a “left-margin” and “right-inline” block. A
;;;     potential benefit is that the styling of blocks can be further
;;;     controlled in the final output (e.g. using CSS for HTML, styles for
;;;     Word, etc.).
;;;
;;;       <bibliography>
;;;         <layout>
;;;           <text display="left-margin" variable="citation-number"
;;;               prefix="[" suffix="]"/>
;;;           <group display="right-inline">
;;;             <!-- rendering elements -->
;;;           </group>
;;;         </layout>
;;;       </bibliography>
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The American Anthropological Association style uses a display="block" for
;;; the first line, contributors, followed by a display="left-margin" group for
;;; the date, and then a display="right-inline" for the rest. It uses no
;;; special settings for margins or anything in the bibliography tag. (AAA has
;;; since dropped their special style and is now going with Chicago
;;; Author-Date.)
;;;
;;; The APA annotated bibliography and the Chicago annotated bibliography use
;;; display="block" for the text variables "abstract" and "note",
;;; respectively. Those are the last items of each bibliography entry... empty
;;; and not emitted when that variable has no value for the items expansion.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Trying various bibliography formats by changing CSL styles:
;;;
;;; Open University (numeric), hanging-indent="true"
;;; second-field-align="flush", 'display' attribute not used.
;;;
;;; Labels in the document are in-text [1] numbered in citation-order, and the
;;; bibliography is presented in citation-order, with the citation-number, a
;;; space, and then the bibliographic entry. The HTML looks like this:
;;;
;;; <div class="csl-entry">
;;;   <div class="csl-left-margin">1 </div><div class="csl-right-inline">Galloway Jr, Russell W. (1989) ‘Basic Equal Protection Analysis’. <i>Santa Clara Law Review</i>, 29, pp. 121–170. [online] Available from: http://heinonline.org/HOL/Page?handle=hein.journals/saclr29&#38;id=139&#38;div=&#38;collection=journals</div>
;;; </div>
;;;
;;; That "csl-left-margin" followed by "csl-right-inline" thing is what I see
;;; for every style of this kind, where there's a label in front of the
;;; bibliography entry.
;;;
;;; <associate|zotero-BibliographyStyle_arrayList|()>
;;; <associate|zotero-BibliographyStyle_bodyIndent|2.0000tab>
;;; <associate|zotero-BibliographyStyle_entrySpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_firstLineIndent|-2.0000tab>
;;; <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_tabStopCount|0>
;;;
;;; -------------------------------------------------------------------------
;;;
;;; APA: hanging-indent="true" entry-spacing="0" line-spacing="2"
;;;
;;; HTML:
;;;
;;; <div class="csl-entry">Crouse v. Crouse, 817P. 2d 836 (Court of Appeals September 11, 1991). Retrieved from http://scholar.google.com/scholar_case?q=Crouse+v+Crouse&#38;hl=en&#38;as_sdt=4,45&#38;case=2646370866214680565&#38;scilh=0</div>
;;;
;;; <associate|zotero-BibliographyStyle_arrayList|()>
;;; <associate|zotero-BibliographyStyle_bodyIndent|2.0000tab>
;;; <associate|zotero-BibliographyStyle_entrySpacing|0.0000>
;;; <associate|zotero-BibliographyStyle_firstLineIndent|-2.0000tab>
;;; <associate|zotero-BibliographyStyle_lineSpacing|2.0000>
;;; <associate|zotero-BibliographyStyle_tabStopCount|0>
;;;
;;; -------------------------------------------------------------------------
;;;
;;; I'm pretty sure it's an array of tab stops.
;;;
;;; It is set by Elsevier (numeric, with titles, sorted alphabetically).
;;;   entry-spacing="0" second-field-align="flush", 'display' attribute not used.
;;;
;;; <div class="csl-entry">
;;;     <div class="csl-left-margin">[1]</div><div class="csl-right-inline">R.W. Galloway Jr, Basic Constitutional Analysis, Santa Clara L. Rev. 28 (1988) 775.</div>
;;; </div>
;;;
;;; The bodyIndent is the same as that tab-stop.
;;;
;;; <associate|zotero-BibliographyStyle_arrayList|<tuple|1.4000tab>>
;;; <associate|zotero-BibliographyStyle_bodyIndent|1.4000tab>
;;; <associate|zotero-BibliographyStyle_entrySpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_firstLineIndent|-1.4000tab>
;;; <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_tabStopCount|1>
;;;
;;; -------------------------------------------------------------------------
;;;
;;; iso690-numeric-en
;;;
;;; The bibliography section of the CSL for this style does not set any of the
;;; attribute variables for hanging indent. It does use 'display="left-margin"
;;; for the text of variable="citation-number", and 'display="right-inline"'
;;; for each group in the bibliography.
;;;
;;; The bbl sent back has a very long string in the location that's supposed to
;;; set the width of the labels in the-bibliography's biblist. So something is
;;; wrong with the way that it forms the maxoffset value. Thus, I can not use
;;; that, and must find that value myself using tm-select or ice-9 match.
;;;
;;; It sends HTML like this:
;;;
;;; <div class="csl-entry">
;;;   <div class="csl-left-margin">1. </div><div class="csl-right-inline">GALLOWAY JR, Russell W. Basic Equal Protection Analysis. <i>Santa Clara Law Review</i> [online]. 1989. Vol. 29, p. 121–170. Available from: http://heinonline.org/HOL/Page?handle=hein.journals/saclr29&#38;id=139&#38;div=&#38;collection=journals</div>
;;;   <div class="csl-right-inline">00044</div>
;;; </div>
;;;
;;; <associate|zotero-BibliographyStyle_arrayList|()>
;;; <associate|zotero-BibliographyStyle_bodyIndent|0.0000tab>
;;; <associate|zotero-BibliographyStyle_entrySpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_firstLineIndent|0.0000tab>
;;; <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_tabStopCount|0>
;;;
;;;
;;; JM Indigo Book
;;;
;;; <associate|zotero-BibliographyStyle_arrayList|()>
;;; <associate|zotero-BibliographyStyle_bodyIndent|0.0000tab>
;;; <associate|zotero-BibliographyStyle_entrySpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_firstLineIndent|0.0000tab>
;;; <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_tabStopCount|0>
;;;
;;;
;;; JM Chicago Manual of Style (full note)
;;;
;;; <associate|zotero-BibliographyStyle_arrayList|()>
;;; <associate|zotero-BibliographyStyle_bodyIndent|2.0000tab>
;;; <associate|zotero-BibliographyStyle_entrySpacing|0.0000>
;;; <associate|zotero-BibliographyStyle_firstLineIndent|-2.0000tab>
;;; <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
;;; <associate|zotero-BibliographyStyle_tabStopCount|0>
;;;

(define tmpt-per-inch 153600); tmpt

(define (inch->tmpt inch)
  (* tmpt-per-inch inch)); tmpt

(define (tmpt->inch tmpt)
  (/ tmpt tmpt-per-inch)); in


(define tmpt-per-bp 6400/3); tmpt

(define (bp->tmpt bp)
  (* tmpt-per-bp bp)); tmpt

(define (tmpt->bp tmpt)
  (/ tmpt tmpt-per-bp)); bp


(define twip-per-inch 1440); twip

(define (inch->twip inch)
  (* twip-per-inch inch)); twip

(define (twip->inch twip)
  (/ twip twip-per-inch)); in


(define twip-per-bp 20); twip

(define (bp->twip bp)
  (* twip-per-bp bp)); twip

(define (twip->bp twip)
  (/ twip twip-per-bp)); bp


(define tmpt-per-twip 3/320); tmpt

(define (twip->tmpt twip)
  (* tmpt-per-twip twip)); tmpt

(define (tmpt->twip tmpt)
  (/ tmpt tmpt-per-twip)); twip



(define bp-per-inch 72); bp

(define (inch->bp inch)
  (* bp-per-inch inch)); bp

(define (bp->inch bp)
  (/ bp bp-per-inch)); in



(define tmpt-per-12bp 25600); tmpt
(define tmpt-per-10bp 64000/3); tmpt
(define 1em-in-10bp-cmr 21327); tmpt
(define 1em-in-12bp-cmr 25060); tmpt

;;;          min     def     max
;;; <tmlen|10666.7|21333.3|32000.0>
;;;
(define fn-min-10pt 64000/6); tmpt
(define fn-def-10pt 64000/3); tmpt, ==> 200 twip
(define fn-max-10pt 64000/2); tmpt

(define fn-min-12pt 76800/6); tmpt
(define fn-def-12pt 76800/3); tmpt, ==> 240 twip
(define fn-max-12pt 76800/2); tmpt

(define tab-min-10pt 96000/6); tmpt
(define tab-def-10pt 96000/3); tmpt, ==> 300 twip
(define tab-max-10pt 96000/2); tmpt

(define tab-min-12pt 115200/6); tmpt
(define tab-def-12pt 115200/3); tmpt, ==> 360 twip
(define tab-max-12pt 115200/2); tmpt

;;; 12 bp    == 240 twip
;;; 720 twip == 2 tab

(define (zt-zotero-lineSpacing->tmlen meas)
  (let ((sep-mult (/ (if (= meas 0) 240 meas)
                     240)))
    (format #f "~,4f" (exact->inexact sep-mult)))) ;; times par-sep

(define (zt-zotero-entrySpacing->tmlen meas)
  (let ((sep-mult (/ (if (= meas 0) 240 meas)
                     240)))
    (format #f "~,4f" (exact->inexact sep-mult)))) ;; times item-vsep

(define (zt-zotero-firstLineIndent->tmlen meas)
  (let ((indent-tabs (/ meas 360))) ; can be zero
    (format #f "~,4ftab" (exact->inexact indent-tabs))))

(define (zt-zotero-bodyIndent->tmlen meas)
  (let ((indent-tabs (/ meas 360))) ; can be zero
    (format #f "~,4ftab" (exact->inexact indent-tabs))))

(define (zt-zotero-tabstop-arrayList->tmlen-ls tab-ls)
  (let loop ((tab-ls tab-ls)
             (ret '()))
    (cond
     ((null? tab-ls)
      (stree->tree `(tuple ,@(reverse! ret))))
       ;;(format #f "~s" (reverse! ret))) ; scheme reader syntax
      (#t (loop (cdr tab-ls)
                (cons (format #f "~,4ftab"
                              (exact->inexact
                               (/ (car tab-ls) 360)))
                      ret))))))

(define (zt-read-zotero-tabstop-arrayList)
  (with-input-from-string
      (get-env "zotero-BibliographyStyle_arrayList")
    (lambda () (read (current-input-port)))))

(define (zt-read-zotero-tabStopCount)
  (string->number
   (get-env "ztbibItemIndentTabN")))


(tm-define (zotero-Document_setBibliographyStyle tid documentID
            firstLineIndent bodyIndent
            lineSpacing entrySpacing
            arrayList tabStopCount)
  (set-init-env "zotero-BibliographyStyle_firstLineIndent"
                (zt-zotero-firstLineIndent->tmlen firstLineIndent))
  (set-init-env "zotero-BibliographyStyle_bodyIndent"
                (zt-zotero-bodyIndent->tmlen bodyIndent))
  (set-init-env "zotero-BibliographyStyle_lineSpacing"
                (zt-zotero-lineSpacing->tmlen lineSpacing))
  (set-init-env "zotero-BibliographyStyle_entrySpacing"
                (zt-zotero-entrySpacing->tmlen entrySpacing))
  (set-init-env "zotero-BibliographyStyle_arrayList"
                (zt-zotero-tabstop-arrayList->tmlen-ls arrayList))
  (set-init-env "zotero-BibliographyStyle_tabStopCount"
                (format #f "~s" tabStopCount))
  ;;
  (zotero-write tid (safe-scm->json-string '())))



;;; Not documented, but exists in CommMessage.java in LibreOffice side of the
;;; connector. It appears to do nothing there either.
;;;
(tm-define (zotero-Document_cleanup tid documentID)
  (zt-format-debug "Debug:STUB:zotero-Document_cleanup: ~s\n" documentID)
  (zotero-write tid (safe-scm->json-string '())))
  


;;; Indicates that the given documentID will no longer be used and
;;; associated resources may be freed.
;;;
;;; ["Document_complete", [documentID]] -> null
;;;
;;; See: zotero-listen, where this is checked for inline... but also enable it here since I might need to use it during
;;; development, at least. It's never called at all by zotero-listen, so can just be commented off here.
;;;
;;; (tm-define (zotero-Document_complete tid documentID)
;;;   (zotero-write tid (safe-scm->json-string '()) )
;;;   (set! zotero-active? #f)
;;;   ;; (close-zt-zotero-socket-port!)
;;;   )



;;; Deletes a field from the document (both its code and its contents).
;;;
;;; When I choose addCitation and then cancel without selecting one, it returns
;;; and immediately calls this function.
;;;
;;; fieldID as originally returned by Document_cursorInField,
;;; Document_insertField, or Document_getFields.
;;;
;;; ["Field_delete", [documentID, fieldID]] -> null
;;;
(tm-define (zotero-Field_delete tid documentID fieldID)
  (let* ((field (zt-find-zfield fieldID))
         (code (zt-zfield-Code field))
         (text (zt-zfield-Text field)))
    ;; clear from zt-zfield-Code-cache via the function in case it needs to
    ;; anything special later on.
    (zt-set-zfield-Code-from-string field "")
    (tree-set! field ""))
  (zotero-write tid (safe-scm->json-string '())))



;;; Moves the current cursor position to encompass a field.
;;;
;;; ["Field_select", [documentID, fieldID]] -> null
;;;
;;; I think that whether or not this works as expected depends on settings made
;;; by the drd-props macro. I think that I want the cursor to be inside of it's
;;; light blue box, after it.... (writing this comment prior to testing. FLW.)
;;;
(tm-define (zotero-Field_select tid documentID fieldID)
  (zt-go-to-zfield documentID fieldID)
  (zotero-write tid (safe-scm->json-string '())))



;;;
;;; ["Field_removeCode", [documentID, fieldID]] -> null
;;;
(tm-define (zotero-Field_removeCode tid documentID fieldID)
  (let* ((field (zt-find-zfield fieldID))
         (code (zt-zfield-Code field)))
    (tree-set! code ""))
  (zotero-write tid (safe-scm->json-string '())))



;;;;;;;;;
;;;
;;; This could also do some processing of either the text prior to parsing and
;;; conversion to a tree, or of the tree after that phase.
;;;
;;; Todo: Here is where to implement client-side munging of the fieldText prior
;;;       to setting that argument of the zcite tag.
;;;
;;; Ideas include:
;;;
;;;  * For styles that include an href or hlink, ensure proper formatting when
;;;    displayed as an in-text or as a note style citation. That means that the
;;;    hlink should become an href where the label is the URL, and that it must
;;;    be placed on it's own line with a spring on the end of the line above it
;;;    so that the remainder of the citation is filled properly and not
;;;    displayed with inch-wide spaces between words.
;;;
;;;  * Turn in-text hlinks into hlinks with footnote hrefs.
;;;
;;;  * Turn hlinks that display the URL in the textual part into hrefs instead,
;;;    also moved to a footnote, unless already in a footnote.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Notes: Some styles display "doi: DOI-NUMBER-AS-HLINK", others display
;;;        "http://doi.org/DOI-NUMBER-IN-HREF". That's why the outputFormat in
;;;        citeproc.js for HTML does not write in the http://doi.org part in
;;;        front of str, so don't change that.
;;;
;;;        Some styles write: <http://www.online.com/location/document.pdf>
;;;        href links inside of <less> and <gtr>, others write it bare without
;;;        the <less> and <gtr>. The TeXmacs tree has <less> and <gtr>, not "<"
;;;        and ">", so a 6 character string and a 5 character string, not two
;;;        one-character strings.
;;;
;;;        There's not always the same thing preceding or following the URL or
;;;        DOI, and so it does not work right to put the next-line markup
;;;        there. Also, it runs the same outputFormat template for a footnote,
;;;        endnote, or bibliography entry as for an in-text citation where the
;;;        next-line markup doesn't belong.
;;;
;;;        The #<00A0> or &nbsp; character (" ") should be used from within
;;;        Juris-M or Zotero for it's intended purpose, rather than inserting
;;;        \hspace{} markup there.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Use cases, hlink:
;;;
;;;  * In running text:
;;;
;;;    * Option set true, also create footnote with href.
;;;
;;;  * In footnote or endnote:
;;;
;;;    * hlink is not a DOI (http://dx.doi.org/.* or http://doi.org/.*)
;;;    
;;;      * Option set true, create href on line by itself after text of
;;;        footnote?  When there is more than one hlink inside of a footnote,
;;;        then each related href must go on it's own line, like footnotes to
;;;        the footnote, with a letter for each of them; but when there's only
;;;        one hlink, the href needs no letter.
;;;
;;;  * In bibliography:
;;;
;;;    * Same as for footnote or endnote, after the entry, not in a footnote of
;;;      the bibliography.
;;;
;;;
;;; Use cases, href:
;;;
;;;  * In running text (as for in-text citation, e.g., jm-indigobook)
;;;
;;;    * Options: Move the href to a footnote (or endnote?) by itself, elide it
;;;      entirely, or leave it like it is, in-text. No 'next-line' around it
;;;      in-text.
;;;
;;;  * In footnote, as for citations in note styles, don't move the href but
;;;    put it on it's own line. Don't forget that some styles wrap the href
;;;    with <less> and <gtr> (not "<" and ">" in TeXmacs!).
;;;
;;;  * For each citation, footnote, hlink, href: Options (with-wrapped).
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; === hlink and href ===
;;;
;;; * Inside footnote, endnote, or bibliography,
;;;
;;;   * href moves to it's own line, without changing position relative to the
;;;     rest of the hand-written footnote or endnote, or automatically
;;;     generated citation or bibliography entry.
;;;
;;;   * hlink that is not a doi link makes an href on it's own line at the end
;;;     of the present footnote, endnote, or ztbibItemText entry, collecting
;;;     them, and when there is more than one, listing them each with a letter,
;;;     superscripted next to the corresponding hlink text. They are like
;;;     footnotes of the footnote or endnote, but not smaller text than
;;;     already... but footnotesize is normally the same size as "small" so
;;;     even when the link is wrapped with "small" it is the same size as the
;;;     rest of the footnote, so it doesn't need a special case around it. In
;;;     my legal-brief.ts style, there's an option to make the footnotes be the
;;;     same font-size as the rest of the text. It's still good to make the
;;;     URL's be small, so they fit on the page.
;;;
;;;
;;; * Inside running text
;;;
;;;   * href moves to a footnote (or endnote).
;;;
;;;   * hlink makes an href in it's own footnote (or endnote).
;;;
;;;;;;;;;
;;;
;;; These changes need to be made before the zfield Text is set and before
;;; Zotero asks for it back and then stores the original text into the zfield
;;; Code... so the transformation must be done with the fresh string handed to
;;; this program by Zotero before tree-set! text.
;;;;
;;; But when typing a document, entering an hlink or href, either in running
;;; text or in a footnote or endnote, there needs to be special behaviour that
;;; happens when the tag is activated and that can be run via a document scan,
;;; so in update-document or zotero-refresh. (one or the other, don't recurse!)
;;;;
;;; update-document runs zotero-refresh, and really is where this hlink and
;;; href munging belongs, as well as on the notify-activated or (?)
;;; notify-disactivated mode/context methods, for in-href? and in-hlink?.
;;;;
;;; Since the hlink and href munging is done from more than one location, it
;;; must be pulled out into a subroutine. It needs only the block it's
;;; operating in, so the sentence, zbibItemText, footnote, or endnote the hlink
;;; or href being operated on is located in.
;;;;;;;;


(tm-define (zt-move-link-to-own-line lnk)
  (:synopsis "Move links to their own line, in smaller text, so that long links
will not overflow into the page margins. Keep punctuation before and after,
including parentheses and <less> <gtr> around the link put there by some styles.")
  (let* ((pre-lnk-txt (tree-ref (tree-up lnk) (- (tree-index lnk) 1)))
         (pre-lnk-str (and pre-lnk-txt (tree->stree pre-lnk-txt)))
         (post-lnk-txt (tree-ref (tree-up lnk) (+ (tree-index lnk) 1)))
         (post-lnk-str (and post-lnk-txt (tree->stree post-lnk-txt)))
         (is-doi? (and (string? pre-lnk-str)
                       (or (string-suffix? "doi:" pre-lnk-str)
                           (string-suffix? "doi: " pre-lnk-str)
                           (string-suffix? "doi: " pre-lnk-str)))))
    ;; (zt-format-debug "Debug:lnk before: ~s\n" lnk)
    ;; (zt-format-debug "Debug:pre-lnk-str: ~s\n" pre-lnk-str)
    ;; (zt-format-debug "Debug:post-lnk-str: ~s\n" post-lnk-str)
    (unless is-doi?
      (when (string? pre-lnk-str)
        (cond
          ((and (string? post-lnk-str)
                (string-suffix? "<less>" pre-lnk-str)
                (string-prefix? "<gtr>" post-lnk-str))
           ;; Keep link wrapped in <less> <gtr> and put on it's own line
           (set! pre-lnk-str (substring pre-lnk-str
                                        0
                                        (- (string-length pre-lnk-str)
                                           (string-length "<less>"))))
           (tree-set! pre-lnk-txt (stree->tree pre-lnk-str))
           (set! post-lnk-str (substring post-lnk-str
                                         (string-length "<gtr>")
                                         (string-length post-lnk-str)))
           (tree-set! post-lnk-txt (stree->tree post-lnk-str))
           (tree-set! lnk (stree->tree
                           `(concat (next-line)
                                    (small (concat "<less>" ,lnk "<gtr>"))))))
          ((and (string? post-lnk-str)  ;; translation error hack hack hack
                (string-suffix? "<less>less<gtr>" pre-lnk-str)
                (string-prefix? "<less>gtr<gtr>" post-lnk-str))
           ;; Keep link wrapped in <less> <gtr> and put on it's own line
           (set! pre-lnk-str (substring pre-lnk-str
                                        0
                                        (- (string-length pre-lnk-str)
                                           (string-length "<less>less<gtr>"))))
           (tree-set! pre-lnk-txt (stree->tree pre-lnk-str))
           (set! post-lnk-str (substring post-lnk-str
                                         (string-length "<less>gtr<gtr>")
                                         (string-length post-lnk-str)))
           (tree-set! post-lnk-txt (stree->tree post-lnk-str))
           (tree-set! lnk (stree->tree
                           `(concat (next-line)
                                    (small (concat "<less>" ,lnk "<gtr>"))))))
          ((or (and (string-suffix? "http://doi.org/"    pre-lnk-str) "http://doi.org/")
               (and (string-suffix? "http://dx.doi.org/" pre-lnk-str) "http://dx.doi.org/"))
           => (lambda (lnstr)
                ;; Keep link next to the prefix text.
                (set! pre-lnk-str (substring pre-lnk-str
                                             0
                                             (- (string-length pre-lnk-str)
                                                (string-length lnstr))))
                (tree-set! pre-lnk-txt (stree->tree pre-lnk-str))
                (tree-set! lnk (stree->tree
                                `(concat (next-line)
                                         (small (concat ,lnstr ,lnk)))))))
          (#t
           (tree-set! lnk (stree->tree `(concat (next-line) (small ,lnk))))))
        (when (or (string-suffix? " " pre-lnk-str)
                  (string-suffix? " " pre-lnk-str))
          (set! pre-lnk-str (substring pre-lnk-str
                                       0
                                       (- (string-length pre-lnk-str)
                                          1)))
          (tree-set! pre-lnk-txt (stree->tree pre-lnk-str))))
      (when (string? post-lnk-str)
        (let pls ((strs (list "." ")." "," ";" ":")))
          (cond
           ((null? strs) #t)
           ((string-prefix? (car strs) post-lnk-str)
            (tree-set! lnk (stree->tree
                            `(concat ,lnk
                                     ,(substring post-lnk-str
                                                 0
                                                 (string-length (car strs))))))
            (set! post-lnk-str (substring post-lnk-str
                                          (string-length (car strs))
                                          (string-length post-lnk-str)))
            (tree-set! post-lnk-txt (stree->tree post-lnk-str))) ; Fall out of loop.
           (#t (pls (cdr strs)))))
        (when (and (> (string-length post-lnk-str) 1)
                   (string? pre-lnk-str))
          (tree-set! lnk (stree->tree `(concat ,lnk (next-line))))
          (when (or (string-prefix? " " post-lnk-str)
                    (string-prefix? " " post-lnk-str))
            (set! post-lnk-str (substring post-lnk-str 1 (string-length post-lnk-str)))
            (tree-set! post-lnk-txt (stree->tree post-lnk-str))))))
    ;; (zt-format-debug "Debug:lnk after: ~s\n" lnk))
    )
  lnk)


(tm-define (zt-delete-one-space-to-left-of lnk)
  (let* ((pre-lnk-txt (tree-ref (tree-up lnk) (- (tree-index lnk) 1)))
         (pre-lnk-str (and pre-lnk-txt (tree->stree pre-lnk-txt))))
    (when (or (string-suffix? " " pre-lnk-str)
              (string-suffix? " " pre-lnk-str))
      (set! pre-lnk-str (substring pre-lnk-str
                                   0
                                   (- (string-length pre-lnk-str)
                                      1)))
      (tree-set! pre-lnk-txt (stree->tree pre-lnk-str)))))



(tm-define (zt-fixup-embedded-slink-as-url lnk)
  (cond
    ((and (tree-in? lnk '(ztHrefFromBibToURL ztHrefFromCiteToBib))
          (tree-in? (tree-ref lnk 1) '(slink verbatim)))
     (let ((slink-or-verbatim (tree-ref lnk 1)))
       (tree-set! slink-or-verbatim (tree-ref slink-or-verbatim 0)))))
  lnk)

;;; Debug: tid:10 len:190 cmdstr:"[\"Field_setText\",[\"10724-(1)\",\"+3LuhRbmY22me9N\",\"\\\\textit{Statutes in derogation of
;;; common law not strictly construed --- Rules of equity prevail.}, Title 68, Chapter 3 § 2 (2014).\",false]]"
;;;
;;; Debug: ("Field_setText" (10 "10724-(1)" "+3LuhRbmY22me9N" "\\textit{Statutes in derogation of common law not strictly construed
;;; --- Rules of equity prevail.}, Title 68, Chapter 3 § 2 (2014)." #f))
;;;
;;; Debug:zt-zotero-str_text->texmacs:t before: <tree <with|font-shape|italic|Statutes in derogation of common law not strictly
;;; construed \V Rules of equity prevail.>, Title 68, Chapter 3 � 2 (2014).>
;;;
;;; Debug:zt-zotero-str_text->texmacs:select lt: ()
;;;
;;; Debug:zt-zotero-str_text->texmacs:t after: <tree <with|font-shape|italic|Statutes in derogation of common law not strictly
;;; construed \V Rules of equity prevail.>, Title 68, Chapter 3 � 2 (2014).>
;;;
;;; Debug: zotero-write: 10 "null"
;;;
;;; Debug: tid:11 len:49 cmdstr:"[\"Field_getText\",[\"10724-(1)\",\"+3LuhRbmY22me9N\"]]"
;;;
;;; Debug: ("Field_getText" (11 "10724-(1)" "+3LuhRbmY22me9N"))
;;;
;;; Debug: zotero-write: 11 "\"(concat (with \\\"font-shape\\\" \\\"italic\\\" \\\"Statutes in derogation of common law not strictly
;;; construed \\\\x16 Rules of equity prevail.\\\") \\\", Title 68, Chapter 3 � 2 (2014).\\\")\""
;;;
;;; JavaScript error:
;;; file:///home/karlheg/.mozilla/firefox/yj3luajv.default/extensions/jurismOpenOfficeIntegration@juris-m.github.io/components/zoteroOpenOfficeIntegration.js,
;;; line 257: NS_ERROR_ILLEGAL_INPUT: Component returned failure code: 0x8050000e (NS_ERROR_ILLEGAL_INPUT)
;;; [nsIScriptableUnicodeConverter.ConvertToUnicode
;;;
;;;
;;; The only way I could fix this, for now, was to add:
;;;
;;;    .replace(/\u00B6/g, "\\ParagraphSignGlyph")
;;;    .replace(/\u00A7/g, "\\SectionSignGlyph")
;;;
;;; ... to the text_escape function for the bbl outputFormat in schomd.coffee, and then add matching macros to the tm-zotero.ts
;;; style. I don't know where the problem occurs. Guile-2 has the ability to set the encoding of specific ports, and perhaps that
;;; will fix it; but it might be another problem to do with how the text sent by Zotero is converted to TeXmacs and back.
;;;

;;; Todo: Perhaps this ought to be configurable, by making it possible for the
;;; user to put their own ones into a separate configuration file?
;;;
(define zt-zotero-regex-replace-clauses
  (map (lambda (elt)
         (cons (apply make-regexp `,(car elt))
               (cdr elt)))
       ;;
       ;; Remember that these execute one after the next, and are applied using regexp-substitute/global, so they must contain
       ;; 'post' as an element in order to have them work on the entire string.
       ;;
       `((("(\r\n)")
          pre "\n" post);; The standard "integration.js" sends RTF, which uses \r\n pairs. Turn them to \n only.
         ;;
         ;; Template
         ;;
         ;;(("")
         ;; pre "" post);; comment
         ;;
         ;;
         ;; Categorized sort hack utilizing Juris-M abbrevs mechanism. 03USC#@18#@00241#@
         ;; (for Title 18 U.S.C. §241, where federal laws are the 03'd category in the larger category of items of type "statute")
         ;;
         ;; For: Privacy and civil liberties officers, Title 42 U.S.C. §2000ee-1
         ;; Title: 03USC#@42#@02000ee1#@Privacy and civil liberties officers.
         ;;
         ;; Notice that using the prefix 03USC#@, I get sorting to 3rd category, and the string USC to search with for finding
         ;; it. This stripping of the prefix must happen prior to the abbrev substitutions below or the USC will get replaced in the
         ;; sorting prefix, leaving 03\abbrev{U.S.C.}#@ there, which is not what I want, obviously.
         ;;
         ;; Perhaps ideally the CSL should sort them according to a special sort macro designed for sorting the USC laws into the
         ;; correct order, and then the Juris-M / Zotero user interface ought to be able to sort them in the same order. But for
         ;; now, it doesn't do that, but this makes sorting them by title group them together and in the expected (defined) order.
         ;;
         ;; All this does is strip the prefix off of the title of the item, so the prefix is used for sorting, in both the
         ;; user-interface and bibliography, but not for rendering the citation. It of course assumes that normally titles don't
         ;; contain strings that match this pattern.
         ;;
         (("(([0-9][0-9a-zA-Z.]+#@)+)")
          pre post)
         (("((.*)\\2X-X-X([  ]?|\\hspace.[^}+].))") ;; RepeatRepeatX-X-X to delete. Hopefully won't affect sort-order much.
          pre post)
         (("(X-X-X([  ]?|\\hspace.[^}]+.))")
          pre post)
         (("(([  ]?|\\hspace.[^}+].)\\(\\))") ;; empty parentheses and space before them (but NOT period or space after).
          pre post)
         (("(.*000000000@#(.ztbib[A-Za-z]+.*})}.*\\.?}%?)" ,regexp/newline)
          pre 2 post) ;; Category heading dummy entries.
         ;;
         ;; Unless you use UTF-8 encoded fonts (TeX Gyre are very good UTF-8 encoded fonts; the standard TeX fonts are Cork
         ;; encoded) these characters won't work right for some reason. The macros I'm replacing them with below expand to the same
         ;; glyphs, but wrapped in a `with' so that the font is for certain set to a UTF-8 encoded one there. They can, of course,
         ;; be redefined... Perhaps when the document's main font is already a UTF-8 encoded font, these should be redefined too, so
         ;; they expand without the `with' wrapper that changes the font the glyph is rendered from.
         ;;
         ;; By "won't work right", I mean that the wrong glyph is shown, or, in the pdf outlines, the paragraph sign does not show
         ;; up as such, but instead as a ü... So first, these must be sent as UTF-8 encoded characters, to get the right glyph in
         ;; the pdf outlines and in the running text.
         ;;
         (("(¶)")
          pre "\\ParagraphSignGlyph{}" post)
         (("(§)")
          pre "\\SectionSignGlyph{}" post)
         ;;
         ;; Todo: Fix this in citeproc.js (bibliography for collapsed parallel citation) When a legal case is cited twice in a row
         ;; in a citation cluster, they are collapsed into a parallel citation. With Indigobook, the in-text citation looks perfect,
         ;; but for some reason the one in the bibliography has a ., between the two different reporters, rather than only a , so
         ;; this hack cleans that up.
         ;;
         (("(\\.,)")
          pre "," post)
         ;;
         ;; Using the ibus mathwriter input method, I can type -> and get →. I can put that at the end of the suffix text, when I
         ;; want the following semicolon or period to be deleted. For example:
         ;;
         ;; Giglio v. United States, 405 U. S. 150, 153 (1972), quoting→; Napue v. Illinois, 360 U. S. 264, 269 (1959).
         ;;
         ;; In the first citation of the citation cluster, the one to Giglio, the suffix text is ", quoting→". The processor returns
         ;; the suffix text unchanged, and places the semicolon between the two citations in the citation cluster. Because of the
         ;; arrow there, this hack removes that semicolon:
         ;;
         (("(→}*[;.])")
          pre post)
         ;;
         ;; use \abbr{v.} to make the space after the period be a small sized one.
         ((" (v\\.?s?\\.?) ")
          pre " \\abbr{v.} " post)
         (("(U\\.?S\\.?C\\.?)")
          pre "\\abbr{U.S.C.}" post)
         (("(Jan\\.|Feb\\.|Mar\\.|Apr\\.|May\\.|Jun\\.|Jul\\.|Aug\\.|Sep\\.|Sept\\.|Oct\\.|Nov\\.|Dec\\.)")
          pre "\\abbr{" 1 "}" post)
         (("(Dr\\.|Mr\\.|Mrs\\.|Jr\\.|PhD\\.|Jd\\.|Md\\.|Inc\\.|Envtl\\.|Cir\\.|Sup\\.|Ct\\.|App\\.|U\\.|Mass\\.|Const\\.|art\\.|Art\\.|sec\\.|Sec\\.|ch\\.|Ch\\.|para\\.|Para\\.)")
          pre "\\abbr{" 1 "}" post)
         (("(L\\. Rev\\.)")
          pre "\\abbr{L.} \\abbr{Rev.}" post)
         (("([A-Z]\\.)([  ])")
          pre "\\abbr{" 1 "}" 2 post)
         )))

         ;; ("<abbr>([^<]+)</abbr>"
         ;;  pre "\\abbr{" 1 "}" post)

;;; What this suggests the need for is a way to add new ones to it on-the-fly,
;;; with no need to reload the editor extension. It might also be useful to
;;; have something like the regexp-opt that there is in GNU Emacs.

(define (zt-zotero-regex-transform str_text)
  (set-message "Zotero: regex transform..." "Zotero integration")
  ;; (zt-format-debug "zt-zotero-regex-transform:before:str_text: ~S\n" str_text)
  (let loop ((text str_text)
             (rc zt-zotero-regex-replace-clauses))
    ;; each is applied in turn, so later ones can modify results of earlier
    ;; ones if you like.
    (cond
      ((null? rc)
       (recall-message)
       ;; (zt-format-debug "zt-zotero-regex-transform:after:text: ~S\n" text)
       text)
      (else
        ;; (zt-format-debug "zt-zotero-regex-transform:during:text: ~S\n" text)
        (loop (apply regexp-substitute/global `(#f ,(caar rc) ,text ,@(cdar rc)))
              (cdr rc))))))



;;;
;;; This runs for both in-text or note citations as well as for the bibliography.
;;;
(tm-define (zt-zotero-str_text->texmacs str_text is-note? is-bib?)
  ;; With a monkey-patched Juris-M / Zotero, even when the real outputFormat is
  ;; bbl rather than rtf, the integration.js doesn't know that, and wraps
  ;; strings in {\rtf ,Body}. This removes it when it has done that.
  (let* ((str_text (string-convert
                    (zt-zotero-regex-transform
                     (if (string-prefix? "{\\rtf " str_text)
                         (substring str_text 6 (1- (string-length str_text)))
                         str_text))
                    "UTF-8" "Cork"))
         (t (latex->texmacs (parse-latex str_text)))
         (b (buffer-new)))
    (set-message "Zotero: str_text->texmacs..." "Zotero integration")
    (buffer-set-body b t) ;; This is magical.
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    ;;
    ;; Used from inside tm-zotero.ts
    ;;
    (let ((lt (select t '(:* (:or ztHref hlink href)))))
      ;; It turns out that tm-select will return these not in tree or document 
      ;; order.  For this function, that's alright.
      ;; (zt-format-debug "Debug:zt-zotero-str_text->texmacs:t before: ~s\n" t)
      ;; (zt-format-debug "Debug:zt-zotero-str_text->texmacs:select lt: ~s\n" lt)
      (let loop ((lt2 lt))
        (let ((lnk (and (pair? lt2) (car lt2)))) ; lnk will be bool or tree
          (cond
            ((null? lt2) #t)
            ((or is-note? is-bib?)
             (zt-move-link-to-own-line lnk)
             (loop (cdr lt2)))
            (else
              (loop (cdr lt2)))))))
    ;;
    ;; from propachi-texmacs/bootstrap.js monkeypatch VariableWrapper
    ;;
    (let ((lt (select t '(:* (:or ztHrefFromBibToURL ztHrefFromCiteToBib)))))
      (let loop ((lt2 lt))
        (let ((lnk (and (pair? lt2) (car lt2))))
          (cond
            ((null? lt2) #t)
            (else
              ;;
              ;; juris-m citeproc.js propachi-texmacs monkeypatch
              ;; VariableWrapper sends text of a URL inside of a \path{} tag so
              ;; that the conversion inside of TeXmacs into a texmacs tree does
              ;; not modify the URL. It creates an slink tag in TeXmacs, and
              ;; that's unwrapped here to make the links function
              ;; correctly. They don't like having their URL be an slink.
              ;;
              (zt-fixup-embedded-slink-as-url lnk))))))
    ;;
    (tree-simplify t)
    ;; (zt-format-debug "Debug:zt-zotero-str_text->texmacs:t after: ~s\n" t)
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    (buffer-close b)
    (recall-message)
    t))

;;;
;;; Remember that there is a difference between the source document tree and
;;; the typeset tree, and that it is not always the case that the cursor focus
;;; is on the field when it's being tested. These two don't require the cursor
;;; focus to be there, and should not, and they work on the source document
;;; where the typesetting environment has not necessarily been formed at the
;;; point in time where these are run! That's why it can not simply use
;;; in-zcite? or in-zbibliography?. Those are for the cursor-focus tree while
;;; editting. These are for the zotero integration for seeing how to format the
;;; final result of translating LaTeX bbl to TeXmacs.
;;;
;;; In particular, it can not rely on zt-not-inside-note, zt-in-footnote, or
;;; zt-in-endnote, since those are part of the dynamic typesetting tree
;;; environment, not the static source document tree environment. Only the
;;; init-env, knowledge of the defaults, and the "with" surrounding can be seen
;;; by these predicates.
;;;

;;;
;;; From: generic/format-edit.scm, not exported or tm-define'd there either.
;;;
(define (with-like-search t)
  (if (with-like? t) t
      (and (or (tree-atomic? t) (tree-in? t '(concat document)))
	   (and-with p (tree-ref t :up)
	     (with-like-search p)))))


;;;
;;; Input is a field tree, already found.
;;;
(tm-define (zt-zfield-IsBib? field)
  (string-prefix? "BIBL" (as-string (zt-zfield-Code field))))


;;;
;;; Input is a field tree, already found.
;;;
(tm-define (zt-zfield-IsNote? field)
  ;; Inside a "with" context that has zt-option-this-zcite-in-text true?
  (let* ((with-t (with-like-search (tree-ref field :up)))
         (in-text-opt (and with-t                             
                           (with-ref with-t
                                     "zt-option-this-zcite-in-text")))
         (forced-in-text? (and in-text-opt
                               (== (tree->string in-text-opt) "true")))
         (fn-t (tree-search-upwards (tree-ref field :up)
                                    '(zt-footnote footnote)))
         (in-footnote? (not (not fn-t)))
         )
    (and (not (zt-zfield-IsBib? field))
         (or
          (and (not forced-in-text?)
               ;; Global pref is set due to a CSL "note" style: (default)
               (and (test-env? "zotero-pref-noteType0" "false") ;; Overrides
                    (or (test-env? "zotero-pref-noteType1" "true")
                        (test-env? "zotero-pref-noteType2" "true"))))
          ;; Inside of a footnote?
          in-footnote?))))
   


;;; Sets the (visible) text of a field.
;;;
;;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;;
;;; Let's assume that for this, it's always "isRich", so ignore that arg.
;;;
(tm-define (zotero-Field_setText tid documentID fieldID str_text isRich)
  (let* ((field   (zt-find-zfield fieldID)) ;; zcite tree
         (text    (zt-zfield-Text field)) ;; string
         (is-note? (zt-zfield-IsNote? field))
         (is-bib? (zt-zfield-IsBib? field))
         (tmtext
          (zt-zotero-str_text->texmacs str_text is-note? is-bib?)))
    (hash-remove! zt-zfield-modified?-cache fieldID)
    (tree-set! text tmtext))
  (zotero-write tid (safe-scm->json-string '())))



;;; Gets the (visible) text of a field.
;;;
;;; ["Field_getText", [documentID, fieldID]] -> str_text
;;;
(tm-define (zotero-Field_getText tid documentID fieldID)
  (let* ((field (zt-find-zfield fieldID))
         (str_text (format #f "~s" (tree->stree
                                    (zt-zfield-Text field))))
         (str_utf8 (string-convert str_text "Cork" "UTF-8"))
         ;;(str_md5 (md5-string str_utf8))
         )
    (zotero-write tid (safe-scm->json-string str_utf8))));str_md5))))



;;; Sets the (hidden, persistent) code of a field.
;;;
;;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;;
(tm-define (zotero-Field_setCode tid documentID fieldID str_code)
  (let* ((field (zt-find-zfield fieldID)))
    (zt-set-zfield-Code-from-string field str_code))
  (zotero-write tid (safe-scm->json-string '())))



;;; Gets the code of a field.
;;;
;;; ["Field_getCode", [documentID, fieldID]] -> str_code
;;;
(tm-define (zotero-Field_getCode tid documentID fieldID)
  (let* ((field (zt-find-zfield fieldID)))
    (zotero-write tid (zt-get-zfield-Code-string field))))



;;; Converts a field from one type to another.
;;;
;;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;;; null
;;;
(tm-define (zotero-Field_convert tid documentID
                                 fieldID str_fieldType int_noteType)
  (zt-format-debug "Debug:STUB:zotero-Field_convert: ~s ~s ~s ~s\n"
                   documentID fieldID
                   str_fieldType int_noteType)
  (zotero-write tid (safe-scm->json-string '())))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local Variables:
;;; fill-column: 132
;;; truncate-lines: t
;;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
