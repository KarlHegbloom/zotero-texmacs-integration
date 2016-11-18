;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;;; MODULE      : tm-zotero.scm
;;; DESCRIPTION : Zotero Connector Plugin
;;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;;
;;{{{ This software falls under the GNU general public license version 3 or

;;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>

;;}}}
;;;;

;;{{{ Module definition and uses

(texmacs-module (tm-zotero)
  (:use (kernel texmacs tm-modes)
        (kernel library content)
        (kernel library list)
        (utils base environment)
        (utils edit selections)
        (utils library cursor)
        (generic document-edit)
        (text text-structure)
        (generic document-part)
        (generic document-style)
        (generic generic-edit)
        (generic format-edit)
        (convert tools sxml)))


;;; Just to be sure that (a) these libraries are actually installed where this
;;; program is being used, and (b) that known working versions of them are
;;; installed, I have bundled them with tm-zotero. Within TeXmacs, these ones
;;; will shadow any that are also installed in Guile's own %load-path
;;; directories.

;;;
;;; This copy of json was ported from Guile 2.0 to Guile 1.8
;;; by Karl M. Hegbloom.
;;;
;;; Todo: When TeXmacs is ported to Guile 2.n, I will need to do something to
;;; ensure that the correct version of this json library is loaded.
;;;
(use-modules (tm-zotero json))

;;; This will print a warning about replacing current-time in module zotero.
;;; Overriding current-time is intentional. It only affects this module's
;;; namespace.
;;;
(use-modules (srfi srfi-19))            ; Time

(define cformat format)
(use-modules (ice-9 format))

;; (use-modules (md5))
;; (define (string->md5 str)
;;   (with-input-from-string str
;;     (md5)))

;;; I really want pcre in Guile for this.
;;;
(use-modules (ice-9 regex))

(use-modules (ice-9 common-list))

;;; This produces:
;;;
;;;   Warning: `make' imported from both (guile-user) and (oop goops).
;;;
;;; The :renamer renames all of them. I don't want that. I'll do it by hand,
;;; until Guile 2.n, where the use-modules form has been extended with features
;;; not present in 1.8, which for the time being, we must work with.
;;;
(define guile-user:make (@ (guile-user) make))
(define goops:make (@ (oop goops) make))
(use-modules (oop goops))
(define make goops:make)                ; only affects this module.
;; (use-modules (oop goops describe))

(use-modules (compat guile-2))
(use-modules (term ansi-color))

;;}}}

;;{{{ Misc. global setting overrides for Guile

;;; With a very large bibliography, I had it stop with a Guile stack
;;; overflow. The manual for Guile-2.2 says that they've fixed the problem by
;;; making the stack dynamically extendable... but I think that this may still
;;; be required then because it's a setting designed more for checking programs
;;; that recurse "too deeply" rather than to guard against actual stack
;;; overflow.
;;;
;;; When it happened, it was not a crash, but instead was something inside of
;;; Guile-1.8 counting the stack depth, and throwing an error when the depth
;;; went past some default limit. Setting this to 0 removes the limit, and so
;;; if it runs out of stack this time, expect an operating system level crash
;;; or something... It depends on how the Scheme stack is allocated, and
;;; perhaps on per-user ulimit settings. (On Ubuntu, see:
;;; /etc/security/limits.conf owned by the libpam-modules package.) I don't
;;; know if ulimit settings affect available stack depth in this program. If
;;; you have a very large bibliography and it crashes TeXmacs, try extending
;;; your ulimit stack or heap limits.
;;;
(debug-set! stack 0)


;;; If it ain't broke, don't fix it. Unless disabling this breaks something, it
;;; will disappear soon. I don't think the locale needs to be set like this to
;;; get 8-bit clean links and UTF-8 across the link between TeXmacs and
;;; Firefox. That's done by the conversion functions called in the right
;;; places.
;;
;; (cond-expand
;;  (guile-2
;;   ;; In guile2 I think I'll need to set the port encoding.  There's still the
;;   ;; problem where the part of TeXmacs that converts LaTeX into TeXmacs assumes
;;   ;; cork encoding rather than UTF-8. That is solved by using string-convert.
;;   ;;
;;   )
;;  (else
;;    ;; My personal locale was already set to a UTF-8 one, and everything worked
;;    ;; fine. The socket read and write routines are using u8 vectors, and so it
;;    ;; ought to be fine with any encoding, since it's not trying to do any
;;    ;; conversions at that layer. Just to be sure though, I'm setting this
;;    ;; here. If it causes problems for anyone, send me a github issue ticket.
;;    ;;
;;    (define orig-locale-LC_ALL   (setlocale LC_ALL))
;;    (define orig-locale-LC_CTYPE (setlocale LC_CTYPE))
;;    (unless (string-suffix? ".UTF-8" orig-locale-LC_CTYPE)
;;      (setlocale LC_CTYPE (string-append (substring orig-locale-LC_CTYPE
;;                                                    0
;;                                                    (string-index orig-locale-LC_CTYPE
;;                                                                  (string->char-set ".")))
;;                                         ".UTF-8")))))

;;}}}

;;{{{ Error and debugging printouts to console with time differences for benchmarking

(define timestamp-format-string-colored
  (string-concatenate
   (list
    (colorize-string "~10,,,'0@s" 'GREEN) ;; seconds +
    ":"
    (colorize-string "~10,,,'0@s" 'GREEN) ;; nanoseconds
    ":("
    (colorize-string "~10,,,'0@s" 'CYAN)  ;; seconds since last timestamp +
    ":"
    (colorize-string "~10,,,'0@s" 'CYAN)  ;; nanoseconds since last timestamp
    "):")))

(define timestamp-format-string-plain
  "~10,,,'0@s:~10,,,'0@s:(~10,,,'0@s:~10,,,'0@s):")

(define timestamp-format-string timestamp-format-string-colored)


(define last-time (current-time))

(define (timestamp time)
  "@time is a time-utc as returned by srfi-19:current-time."
  (let* ((td (time-difference time last-time))
         (ret (format #f
                timestamp-format-string
                (time-second time)
                (time-nanosecond time)
                (time-second td)
                (time-nanosecond td))))
    (set! last-time time)
    ret))


(define (zt-format-error . args)
  ;;(:secure)
  (tm-errput
   (apply format (cons #f
                       (cons
                        (string-concatenate
                         (list
                          (timestamp (current-time))
                          (car args)))
                        (cdr args))))))


(define zt-debug-trace? #f)


(define (zt-format-debug . args)
  ;;(:secure)
  (when zt-debug-trace?
    (tm-output
     (apply format (cons #f
                         (cons
                          (string-concatenate
                           (list
                            (timestamp (current-time))
                            (car args)))
                          (cdr args)))))))

;;}}}


;;{{{ Access and define not-yet-exported functions from other parts of TeXmacs
;;;
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
;;;   document part from the start of the document to the end. A document part
;;;   is a XXXX 
;;;
;;; (buffer-show-part id)
;;; (buffer-toggle-part id)
;;;
(define buffer-show-preamble (@@ (generic document-part) buffer-show-preamble))
(define buffer-hide-preamble (@@ (generic document-part) buffer-hide-preamble))

;;;
;;; From: generic/format-edit.scm, not exported or tm-define'd there either.
;;;
(define (with-like-search t)
  (if (with-like? t) t
      (and (or (tree-atomic? t) (tree-in? t '(concat document)))
	   (and-with p (tree-ref t :up)
	     (with-like-search p)))))

;;}}}

;;{{{ Misc. functions used within this program
;;;
;;; When parts of the document are hidden, which is what we do when the
;;; document is too large to easily edit, since TeXmacs slows way down to the
;;; point of becoming unusable when the document is larger and especially as
;;; it's structure becomes more complex... It defeats the purpose of hiding
;;; sections if the zcite or zbibiliography fields that are in those hidden
;;; sections are updated along with the rest of the citations in the visible
;;; parts of the document. It will be faster and easier to use when there are
;;; fewer for it to keep track of at a time... and so narrowing the view to
;;; only a single or a few sections should speed up the zcite turnaround time
;;; by reducing the amount of work that it has to do each time.
;;;
;;; Of course, for final document production, you must display all of the
;;; sections and then let Zotero refresh it.
;;;
(define (shown-buffer-body-paragraphs)
  (let ((l (buffer-body-paragraphs))) ;; list of tree
    (if (buffer-test-part-mode? :all)
        l
        (list-filter l (cut tree-is? <> 'show-part)))))


(define (get-documentID)
  (url->string (current-buffer)))

(define (document-buffer documentID)
  (string->url documentID))


;;; Reference binding keys must have deterministic format so the program can
;;; build them from data provided by Juris-M / Zotero. If this is ever changed,
;;; older documents might not work right.
;;;
(define (get-zfield-noteIndex-refbinding-key zfieldID)
  (string-append "zotero" zfieldID "-noteIndex"))

;;; The set-binding call happens inside of the macro that renders the
;;; citation. I spent half a day figuring out how to write a glue-exported
;;; accessor function... then discovered this trick:
;;;
(define (get-refbinding key)
  (texmacs-exec `(get-binding ,key)))

;;}}}


;;{{{ zfield tag definitions, insert-new-zfield; a zfield is a tree.

(define-public zfield-tags '(zcite zbibliography))

;;; If any one of these is-*? => #t, then t is a zfield tree.

(define-public (is-zcite? t)
  (tm-is? t 'zcite))

(define-public (is-zbibliography? t)
  (tm-is? t 'zbibliography))

(define-public (is-zfield? t)
  (tm-in? t zfield-tags))



;;; Top-half of new zfield insertion. This always happens at the cursor
;;; position. After the insert, the cursor is at the right edge of the newly
;;; inserted zfield, just inside the light-blue box around it. focus-tree with
;;; the cursor there returns the zfield tree.
;;;
;;; The bottom-half is in tm-zotero-Document_insertField.
;;;
;;; There must be a "top-half" and a "bottom-half" for this because of the
;;; reasons given in the comment above tm-zotero-Document_insertField,
;;; pertaining to needing to be able to pass the noteIndex back to Zotero
;;; there. There has to be time for the typesetter to run in order for that
;;; noteIndex to exist. It runs during the "delay" form in tm-zotero-listen.
;;;
(define (insert-new-zfield tag placeholder)
  (if (not (focus-is-zfield?))
      (let ((documentID (get-documentID))
            (new-zfieldID (get-new-fieldID))
            (zfd (make <zfield-data>)))
        (set-document-new-zfieldID! documentID new-zfieldID)
        (insert `(,tag ,new-zfieldID (tuple "3" (raw-data "") "false") ,placeholder))
        (slot-set! zfd 'tree-pointer (tree->tree-pointer (focus-tree)))
        ;; This is put into the ht but not the ls until tm-zotero-Document_insertField.
        (hash-set! (get-document-zfield-ht documentID) new-zfieldID zfd))
      (begin ;; focus-is-zfield? => #t
        ;; Todo: This ought to be a dialog if it actually happens much...
        ;; Alternatively, perhaps it could move the cursor out of the zfield,
        ;; arbitrarily to the right or left of it, then proceed with inserting
        ;; the new zfield... Or perhaps it ought to convert it into an
        ;; editCitation rather than an insertCitation?
        (zt-format-error "ERR: insert-new-zfield ~s : focus-tree is a ~s\n"
                         tag (tree-label (focus-tree)))
        #f)))

;;; When the buffer has just opened, the list of zfields is not populated yet. XXXX
;;;
;;; tm-find returns an incorrect result! Use tm-search.
;;;
;; (define (zt-find-zfield zfieldID)
;;   (let ((ls (tm-search
;;             (buffer-tree)
;;             (lambda (t)
;;               (and (tree-in? t zfield-tags)
;;                    (string=? zfieldID
;;                              (object->string (zfield-ID t))))))))
;;     (if (pair? ls)
;;         (car ls)
;;         #f)))

;;}}}

;;;
;;; These are for accessing parts of the static source tree that are saved as
;;; part of the document. They deal with actual document trees.
;;;
;;{{{ zfield trees and tree-ref based accessors for them

;;{{{ Documentation Notes

;;;
;;; A zfield is a tree. Each part of it is a tree also.
;;;
;;; These must match the definitions in tm-zotero.ts;
;;;
;;;  L     0         1           2
;;; (zcite "fieldID" "fieldCode" "fieldText")
;;;
;;;   A zbibliography has the same arity and semantics for it's elements as the
;;;   zcite has.
;;;
;;; fieldID is a string
;;;
;;; fieldCode has undergone some revisions.
;;;
;;;   v.1 The raw UTF-8 string given to us by zotero was stored here.
;;;
;;;       There was problems with it due to the transcoding of UTF-8 into
;;;       TeXmacs internal representation and back.
;;;
;;;   v.2 That UTF-8 string is now wrapped with a raw-data, to avoid the
;;;       transcoding problem. That's fine since it's now hidden from view when
;;;       the tag is disactivated.
;;;
;;;   v.3 There's a need for more tag "attribute" or "property" information
;;;       stored in the tag itself, since that enables it to be saved and
;;;       loaded with the document, and makes it faster to access. So, in v.3,
;;;       the fieldData contains a tuple.
;;;
;;;       That tuple's first child (tree-ref fieldCode 0) is the fieldCode
;;;       layout version number, 3.
;;;
;;;       The second child (tree-ref fieldCode 1) is a raw-data containing the
;;;       UTF-8 fieldCode string sent by Juris-M / Zotero.
;;;
;;;       The third child (tree-ref fieldCode 2) is a boolean flag for whether
;;;       or not the fieldText has been modified. That can only happen if the
;;;       zcite is disactivated, the text editted, and then the zcite activated
;;;       again, and so it's initial value is "false".
;;;
;;;       <tuple|3|<raw-data|fieldCode>|false>
;;;
;;; fieldText is a TeXmacs tree, the result of taking the LaTeX-syntax UTF-8
;;;           string from Zotero, running the regexp transformer on it,
;;;           converting that from UTF-8 to Cork encoding, parsing that string
;;;           to obtain a LaTeX tree, and then converting that into a TeXmacs
;;;           tree.
;;;
;;; fieldNoteIndex is gotten via a reference binding.
;;;
;;}}}

(define (get-zfield-zfieldID-t zfield)
  (tree-ref zfield 0))

(define (get-zfield-zfieldID zfield)
  (object->string (get-zfield-zfieldID-t zfield)))


(define (get-zfield-Code-v zfield)
  (let ((code-t (tree-ref zfield 1)))
    (cond
      ((tm-func? code-t 'tuple)           ; >= v.3
       (string->integer (object->string (tree-ref code-t 0))))
      ((tm-func? code-t 'raw-data) 2)
      (else 1))))


(define (get-zfield-Code-code-t zfield)
  ;; Upgrade old tags.
  (let ((code (tree-ref zfield 1)))
    (cond
      ((tm-func? code 'tuple)           ; >= v.3
       (tree-ref code 1 0))             ; <tuple|3|<raw-data|THIS>|false>
      ((tm-func? code 'raw-data)        ; v.2
       (tree-set! code (stree->tree `(tuple "3" ,(tree->stree code) "false"))) ; update to v.3
       (get-zfield-Code-t zfield)       ; tail-call
       )
      ((tm-atomic? code)                ; v.1
       (tree-set! code (stree->tree `(tuple "3" (raw-data ,(tree->stree code)) "false"))) ; to v.3
       (get-zfield-Code-t zfield)       ; tail-call
       )
      (else ; ? I don't think this can really happen.
        (tree-set! code (stree->tree `(tuple "3" (raw-data "") "false"))) ; to v.3
       (get-zfield-Code-t zfield)       ; tail-call
      ))))

(define (get-zfield-Code-code zfield)
  (object->string (get-zfield-Code-code-t zfield)))


(define (get-zfield-Code-is-modified?-flag-t zfield) ; assume >= v.3
  (tree-ref zfield 1 2))

(define (get-zfield-Code-is-modified?-flag zfield)
  (object->string (get-zfield-is-modified?-flag-t zfield)))

(define (set-zfield-Code-is-modified?-flag! zfield str-bool) ; "false" or "true"
  (let (t (get-zfield-is-modified?-flag-t zfield))
    (tree-set! t (stree->tree str-bool))))


;;; For "note" styles, this reference binding links a citation field with
;;; the footnote number that it appears in.
;;;
;;; Used by tm-zotero-Document_insertField to form it's response. See note
;;; there regarding the necessity of letting the typesetter run in order for
;;; this reference binding to have a readable value.
;;;
(define (get-zfield-NoteIndex-str zfieldID)
  (if (and (not (string? zfieldID))
           (tree? zfieldID)
           (is-zcite? zfieldID))
      (set! zfieldID (get-zfield-zfieldID zfieldID)))
  (get-refbinding
   (get-zfield-noteindex-refbinding-key zfieldID)))



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
(define (get-zfield-Text zfield)
  (tree-ref zfield 2))

;;}}}

;;;
;;; This is for tm-zotero program state that is not saved with the
;;; document. These are scheme data structures, not in-document trees.
;;;
;;{{{ State data for document and zfields
;;{{{ R&D Notes

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; A document-order list of <zfield-data> is necessary for this. I will start
;;; with a simple sorted list maintained using @var{merge}.
;;;
;;; An rb-tree is a lot more complicated, right? I'm hoping that the sorted
;;; lists will be fast enough. If not, then port to an rb-tree after the
;;; TeXmacs port to Guile 2+.
;;;
;;; Alternatively, the wt-tree from SLIB might be easy enough to get working,
;;; and maybe that's better than a flat sorted list?
;;;
;;; sorted input lists => sorted merged output list
;;; merge  alist blist less? => list
;;; merge! alist blist less? => list
;;;
;;;
;;; A position is a tree observer. It is attached to the tree, and so moves as
;;; things are inserted ahead of it, etc. It can always be queried for it's
;;; current path within the document.
;;;
;;; position-new [path]    => position   (path defaults to (cursor-path))
;;; position-delete pos    => undefined
;;; position-set pos path  => undefined (?)
;;; position-get pos       => path
;;;
;;; Example use:  (go-to (position-get pos))
;;;
;;; Ok, so positions are for cursor positions. What I really want is
;;; tree-pointers.
;;;
;;; (tree->tree-pointer t)    => tree-pointer
;;; (tree-pointer->tree ptr)  => tree
;;; (tree-pointer-detach ptr) => undefined

;;}}}

(define-class <zfield-data> ()
  ;; TeXmacs tree-pointer
  (tree-pointer #:init-value #f)
  ;; String, original unmodified text for comparison
  (orig-text #:init-value "")
  ;; Boolean, set when tag is disactivated or reactivated.
  (disactivated? #:init-value #f)
  ;;
  ;; ? (code-cache #:init-value #f)
  ;; Extra data ht? Only if needed.
  ;; data keys: code-cache, ... ?
  ;; (data-ht #:init-thunk make-hash-table)
  )


(define-class <document-data> ()
  ;;
  ;; is a zotero command active?
  ;;
  (tm-zotero-active? #:init-value #f)
  ;;
  ;; one new zfield at a time per document
  ;;
  (new-zfieldID #:init-value #f)
  ;;
  ;; in-document-order list of <zfield-data>
  ;;
  (zfield-ls #:init-thunk list)
  ;;
  ;; hash-table of zfieldID => <zfield-data>
  ;;
  ;; The new-zfieldID will lead to the <zfield-data> for it here, but it's not
  ;; in the above list until it's finalized by tm-zotero-Document_insertField.
  ;;
  (zfield-ht #:init-thunk make-hash-table)
  ;;
  ;; List of refs / pagerefs to referring citations for the end of each
  ;; zbibliography entry. Compute them once, memoized, and so when the
  ;; in-document tag is actually expanded, the operation is a fast hashtable
  ;; lookup returning the pre-computed 'concat tree. The typesetter is run very
  ;; often while using TeXmacs, and so if the full computation had to be run
  ;; each time the tag is re-typeset (e.g. the user is typing on the page just
  ;; above the zbibliography) it would be very slow.
  ;;
  (ztbibItemRefs-ht #:init-thunk make-hash-table)
  ;;
  ;; Anything else?
  ;;
  )

;;; Deliberately not "idempotent" wrt re-loading of this program. Reloading the
;;; zotero.scm module will cause this to be reinitialized to an empty hash
;;; table.
;;;
(define <document-data>-ht (make-hash-table)) ;; documentID => <document-data>



(define (get-<document-data> documentID)
  (or (hash-ref <document-data>-ht documentID #f)
      (let ((dd (make <document-data>)))
        (set-<document-data>! documentID dd)
        dd)))

(define (set-<document-data>! documentID document-data)
  (hash-set! <document-data>-ht documentID document-data))



(define (get-document-tm-zotero-active? documentID)
  (slot-ref (get-<document-data> documentID) 'tm-zotero-active?))

(define (set-document-tm-zotero-active?! documentID val)
  (slot-set! (get-<document-data> documentID) 'tm-zotero-active? val))



(define get-new-zfieldID create-unique-id)


(define (get-document-new-zfieldID documentID)
  (slot-ref (get-<document-data> documentID) 'new-zfieldID))

(define (set-document-new-zfieldID! documentID zfieldID)
  (slot-set! (get-<document-data> documentID) 'new-zfieldID zfieldID))

(define (zfieldID-is-document-new-zfieldID? documentID zfieldID)
  (== zfieldID (get-document-new-fieldID documentID)))



(define (get-document-zfield-ls documentID)
  (slot-ref (get-<document-data> documentID) 'zfield-ls))

(define (set-document-zfield-ls! documentID ls)
  (slot-set! (get-<document-data> documentID) 'zfield-ls ls))



(define (get-document-zfield-ht documentID)
  (slot-ref (get-<document-data> documentID) 'zfield-ht))

(define (reset-document-zfield-ht! documentID)
  (slot-set! (get-<document-data> documentID) 'zfield-ht (make-hash-table)))



(define (get-document-ztbibItemRefs-ht documentID)
  (slot-ref (get-<document-data> documentID) 'ztbibItemRefs-ht))

(define (reset-ztbibItemRefs-ht! documentID)
  (slot-set! (get-<document-data> documentID) 'ztbibItemRefs-ht (make-hash-table)))



(define (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
  (hash-ref (get-document-zfield-ht documentID) zfieldID #f))



(define (get-document-zfield-tree-pointer-by-zfieldID documentID zfieldID)
  (slot-ref (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
            'tree-pointer))

(define (get-document-zfield-by-zfieldID documentID zfieldID)
  (tree-pointer->tree
   (get-document-zfield-tree-pointer-by-zfieldID documentID zfieldID)))

(define (go-to-document-zfield-by-zfieldID documentID zfieldID)
  (tree-go-to (get-document-zfield-by-zfieldID documentID zfieldID) 1))



(define (get-document-zfield-disactivated?-by-zfieldID documentID zfieldID)
  (slot-ref (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
            'disactivated?))

(define (set-document-zfield-disactivated?-by-zfieldID! documentID zfieldID val)
  (slot-set! (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
             'disactivated? val))



(define (get-document-zfield-orig-text-by-zfieldID documentID zfieldID)
  (slot-ref (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
            'orig-text))

(define (set-document-zfield-orig-text-by-zfieldID! documentID zfieldID text)
  (when (tree? text)
    (set! text (tmtext-t->string text)))
  (slot-set! (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
             'orig-text text))


(define (tmtext-t->string tmtext-t)
  (with-output-to-string (write (tree->stree tmtext-t))))


(define (document-zfield-text-user-modified? documentID zfieldID cmp-text)
  (when (tree? text)
    (set! text (tmtext-t->string text)))
  (not
   (string=? text (get-document-zfield-orig-text-by-zfieldID documentID zfieldID))))


;; (define (position-less? pos1 pos2)
;;   (path-less? (position-get pos1) (position-get pos2)))


(define (document-<zfield-data>-insert! documentID zfield)
  (let* ((zfieldID (XXXXX))
         (dd (get-<document-data> documentID))
         (zfl (slot-ref dd 'zfield-ls))
         (zfht (slot-ref dd 'zfield-ht))
         (zfd (make <zfield-data>)))
    (position-set (slot-ref zfd 'position) (tree->path zfield))
    (slot-set! zfd 'zfield zfield)
    (slot-set! dd 'zfield-ls
               (merge! zfl '(zfd)
                       (lambda (a b)
                         (position-less? (slot-ref a 'position)
                                         (slot-ref b 'position)))))
    (hash-set! zfht zfieldID zfd)))

(define (document-<zfield-data>-delete! documentID zfieldID)
  (let* ((dd (get-<document-data> documentID))
         (zfl (slot-ref dd 'zfield-ls))
         (zfht (slot-ref dd 'zfield-ht))
         (zfd (hash-ref zfht zfieldID #f)))
    (when zfd
      (slot-set! dd 'zfield-ls
                 (list-filter zfl (lambda (elt)
                                    (not (eq? zfd elt)))))
      (hash-remove! zfht zfieldID))))

;;}}}



;;{{{ Comment regarding tm-select vs tm-search

;;; I wanted to try and use the tm-select functionality to accomplish this, but
;;; found that it does not return the list in document-order!
;;;
;;; Scheme]  (define the-zfields-by-select (select (buffer-tree) '(:* (:or
;;; zcite zbibliography))))
;;;
;;; Scheme]  (map (lambda (t) (list (tree-label t) (tree->string (zfield-ID
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
;;; (get-documentID) "ReferenceMark"))
;;;
;;; Scheme]  (map (lambda (t) (list (tree-label t) (tree->string (zfield-ID
;;; t)))) the-zfields-by-tm-search)
;;;
;;; ((zcite "+6jApItmTysx1SJ") (zcite "+hoOIoDn3p6FB0G") (zcite
;;; "+JGeR0gQNwL2AKT") (zcite "+GHqJJmyDQVHhaO") (zcite "+KUpBSz33QCflpE")
;;; (zcite "+KUpBSz33QCflpD") (zcite "+28SL8xD6MxDCZI") (zcite
;;; "+KUpBSz33QCflpC") (zcite "+KUpBSz33QCflpB") (zcite "+hoOIoDn3p6FB0I")
;;; (zcite "+hoOIoDn3p6FB0J") (zcite "+JGeR0gQNwL2AKU") (zbibliography
;;; "+hoOIoDn3p6FB0H"))
;;;

;;}}}

;;{{{ Is this code still needed for other than reference during transition?

;;;
;;; Is this still needed to initialize the <document-data> when the document is
;;; first opened? I think this is what I was working to replace, so this will
;;; probably go away soon.
;;; 
(define (zt-zfield-search subtree)
  (let ((zt-new-fieldID (get-document-new-fieldID (get-documentID))))
    (tm-search
     subtree
     (lambda (t)
       (and (tree-in? t zfield-tags)
            (not
             (and zt-new-fieldID
                  (string=? zt-new-fieldID
                            (get-zfield-zfieldID t)))))))))

;;; Was: (tm-define (zt-get-zfields-list documentID fieldType)
;;;
;;; Maybe ensure active document is documentID? For now assume it is.
;;; Also for now assume fieldType is always "ReferenceMark", so ignore it.
;;;
;;; WIP: Nothing calls this yet... is it needed? (and it's not fixed up for new
;;; changes yet anyway...)
;;;
(define (init-document-zfields-list documentID)
  ;;
  ;; Maybe ensure active document is documentID? For now assume it is.
  ;;
  ;; Q: What about "save-excursion" or "save-buffer-excursion"?
  ;; A: This is searching the document tree, not moving the cursor.
  ;;
  ;; Todo: What if I copy and paste a zcite from one location to another? The
  ;; zfield-ID of the second one will need to be updated...
  ;; (WIP: See: buffer-notify)
  ;;
  (let* ((fields-tmp-ht (make-hash-table))
         (l (shown-buffer-body-paragraphs))
         (all-fields (append-map zt-zfield-search l)))
      (set! zt-zfield-Code-cache (make-hash-table))
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
            (let ((id-t (zfield-ID (car in)))
                  (new-id ""))
              (if (hash-ref fields-tmp-ht (object->string id-t) #f)
                  (begin
                    (set! new-id (get-new-fieldID))
                    (tree-set! id-t (stree->tree new-id))
                    (hash-set! fields-tmp-ht new-id #t)
                    (zt-get-zfield-Code-string (car in)));; caches zfield-Code
                  (hash-set! fields-tmp-ht (object->string id-t) #t))
              (loop (cdr in) (cons
                              (begin
                                (zt-get-zfield-Code-string (car in));; caches zfield-Code
                                (car in))
                              out))))))))

;;}}}

;;{{{ Dealing with fieldCode (transition in process to new way from this way)

;;; The fieldCode is a JSON string. Among other things, it is how Zotero keeps
;;; track of whether or not the user has editted the automatically formatted
;;; citation text in the fieldText. When it has been editted, Zotero prompts
;;; first before overwriting it. By parsing that JSON and accessing that
;;; information ourself, we can render a red flag when it has been modified, to
;;; create a visual signal to the user. In order to make that happen, all
;;; setting and getting of the fieldCode must happen via these functions.
;;;

;;;REMOVING (define zt-zfield-Code-cache (make-hash-table))
;;; moved to <zfield-data>:data

;;; XXX Todo
(define (zt-get-zfield-Code-string field)
  (let ((id (object->string (zfield-ID field)))
        (str_code (object->string (zt-zfield-Code field))))
    ;; So that Document_getFields causes this to happen.
    ;; (when the document is freshly opened, since these
    ;; ephemeral data structures are not saved with the document)
    (when (and (not (hash-ref zt-zfield-Code-cache id #f))
               (not (and zt-new-fieldID
                         (string=? zt-new-fieldID id))))
      (zt-parse-and-cache-zfield-Code field str_code)) ;; <== first...
    str_code))

;;; Must handle empty string for tm-zotero-Field_delete. Since it does not
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
;;; tree for a field with the sought-for zfieldID, then update the cache.
;;;
(define (zt-set-zfield-Code-from-string field str_code)
  (let ((code (zt-zfield-Code field))
        )
    (zt-parse-and-cache-zfield-Code field str_code)
    (tree-set! code (stree->tree str_code))))
    

;;; It goes through here so that this can also be called from the
;;; Document_getFields...
;;;
;;; Also handle empty string for tm-zotero-Field_removeCode.
;;;
(define (zt-parse-and-cache-zfield-Code field str_code)
  (let* ((id (object->string (zfield-ID field)))
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


(define (zt-get-zfield-Code-cache-ht-by-fieldID zfieldIDstr)
  (hash-ref zt-zfield-Code-cache zfieldIDstr #f))

;;(tm-define (zt-get-zfield-Code-cache-

;;}}}


;;{{{ ztbibItemRefs lists that follow bibliography items

;;;
;;; Returns list of trees that are like:
;;;                        "zciteBibLabel" "displayed text"
;;;  '(ztHrefFromCiteToBib "#zbibSysID696" "text")
;;;
;;; Todo: maintain the list, search only on startup.
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
  (object->string (tree-ref t 0)))


;;;
;;; Typically, this will be 1 to 4 characters of text without any special
;;; formatting inside of it. (Formatting may surround this chunk, but inside of
;;; it, there's not anything expected but an atomic string.
;;;
(define (zt-ztbibItemRefs-get-ztHrefFromCiteToBib-text t)
  (object->string (tree-ref t 1)))



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
      ((tree-func? t 'zcite) (object->string (zfield-ID t)))
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
                     (when (not (string-suffix? "-t" (object->string key)))
                       (set! keys (append keys (list (object->string key))))))
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
  (let* ((sysID (object->string sysID))
         (key-t (string-concatenate/shared (list sysID "-t"))))
    (cond
      ((hash-ref zt-ztbibItemRefs-ht key-t #f) => identity)
    (else
      (zt-ztbibItemRefs-parse-all)
      (hash-ref zt-ztbibItemRefs-ht key-t (stree->tree '(concat "")))))))

;;}}}


;;{{{ :secure ext functions called from tm-zotero.ts style
;;{{{ zt-ext-flag-if-modified (also being replaced)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Memoization cache for zt-ext-flag-if-modified, zfieldID -> boolean-modified?
;;;
;;(define zt-zfield-modified?-cache (make-hash-table))
;;(define zt-zfield-disactivated? (make-hash-table))
;;; Moved inside <zfield-data>.

;;; I'm in the middle of rewriting this stuff to make it faster. I'm thinking
;;; about the lazy initialization of the <zfield-data> and
;;; <document-data>... and finding that I don't have a clear picture of how it
;;; needs to operate.
;;;
;;; Open a document file, which contains 1 or more zfields.  Just in order to
;;; typeset it for display, in order to render the zfield-modified? flags, it
;;; will need this information. When the document contains 0 zfields, then
;;; inserting a new zfield will... But then there's 1 zfield to render, and so
;;; this gets called, so it only needs this one "entry point" to the lazy
;;; cacheing of this information. It doesn't need to initialize this when the
;;; new zfield is inserted...
;;;
;;; For the in-document-order list of zfields, etc., it does need to maintain
;;; that via the buffer-notify mechanism. That is because when a zfield is
;;; "killed", it might not get "yanked" back into the buffer, and so it needs
;;; to be removed from the zfield-ls and zfield-ht slots of the <document-data>
;;; but not reinserted into those, unless it does get "yanked" back in. When
;;; that happens, buffer-notify must ensure that there's not a duplicate
;;; zfieldID, in the case where it is "yanked" more than once, or where the
;;; user used M-w to copy the selected text containing a zfield, and then
;;; "yank" (paste) it in, potentially more than once.

;; (define (zt-get-orig-zfield-Text zfieldID)
;;   (let ((scm-code (hash-ref zt-zfield-Code-cache zfieldID #f)))
;;     (let* ((zft (zt-find-zfield zfieldID))
;;            (scm-code (or scm-code
;;                          ;; trigger parsing and caching of zfield-Code data.
;;                          (and zft
;;                               (zt-get-zfield-Code-string zft)
;;                               ;; access it.
;;                               (hash-ref zt-zfield-Code-cache zfieldID #f))))
;;            (props (and scm-code
;;                        (hash-ref scm-code "properties" #f)))
;;            (plainCitation (and props
;;                                (hash-ref props "plainCitation" #f))))
;;       ;; (zt-format-debug
;;       ;;  "zt-get-orig-zfield-Text:fieldID-str:~s\n\nscm-code:~s\n\nprops:~s\n\nplainCitation:~s\n"
;;       ;;  zfieldID scm-code props plainCitation)
;;       plainCitation)))


;; (define (zt-set-zfield-modified?! zfieldID)
;;   (let* ((field (zt-find-zfield zfieldID))
;;          (text (and field (format #f "~s" (tree->stree (get-zfield-Text field)))))
;;          (orig-text (and text (zt-get-orig-zfield-Text zfieldID)))
;;          (zfield-modified? (and text
;;                                 orig-text
;;                                 (not (string=? text orig-text)))))
;;     ;; (zt-format-debug
;;     ;;  "zt-set-zfield-modified?!:fieldID-str:~s\n\ntext:~s\n\norig-text:~s\n\nzfield-modified?:~s\n"
;;     ;;  zfieldID text orig-text zfield-modified?)
;;     (hash-set! zt-zfield-modified?-cache zfieldID zfield-modified?)
;;     zfield-modified?))

;; (define (zt-zfield-modified?-or-undef zfieldID)
;;   (hash-ref zt-zfield-modified?-cache zfieldID 'undef))

;;; When the debug print statements are enabled and debugging is on, whenever I
;;; type anything in the same paragraph right after a citation, this function
;;; is run every time I press a key. So instead of it having to do all the work
;;; every time, it needs to memoize the answer... The field can not be modified
;;; unless the tag is disactivated first. This is good, since then the
;;; deactivation of the tag can trigger clearing the memoized status for this
;;; flag. So, see: notify-disactivated in this file.
;;;
;;; zfieldID is handed in from the tm-zotero.ts and will be a tree. Also accept
;;; a string.
;;;
;; (tm-define (zt-ext-flag-if-modified zfieldID)
;;   (:secure)
;;   (let ((fieldID-str (object->string zfieldID)))
;;     (when (not (hash-ref zt-zfield-disactivated? zfieldID #f))
;;       (case (zt-zfield-modified?-or-undef zfieldID)
;;         ((undef)
;;          ;; (zt-format-debug "zt-ext-flag-if-modified:undef:~s\n" zfieldID)
;;          (zt-set-zfield-modified?! zfieldID)
;;          (zt-ext-flag-if-modified zfieldID)) ;; tail-call
;;         ((#t)
;;          ;; (zt-format-debug " zt-ext-flag-if-modified: Field is modified: ~s\n" zfieldID)
;;          '(concat (flag "Modified!" "red")))
;;         ((#f)
;;          ;; (zt-format-debug " zt-ext-flag-if-modified: Field is NOT modified: ~s\n" zfieldID)
;;          '(concat (flag "Not Modified." "green")))))))

;;}}}


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
  (zt-format-debug "zt-ext-ztShowID: ~s ~s ~s\n" node clsid body)
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
  (zt-format-debug "STUB:zt-ext-zbibCitationItemID: ~s\n\n" sysID)
  '(concat ""))

(tm-define (zt-ext-bibitem key)
  (:secure)
  (zt-format-debug "STUB:zt-ext-bibitem: ~s\n" key)
  '(concat ""))

;;}}}


;;{{{ DocumentData (from Zotero, saved, parsed -> document initial environment
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
;;; call tm-zotero-Document_getDocumentData, which returns null to Zotero unless
;;; it's been set. After setting it, the next thing Zotero sends is a
;;; tm-zotero-Document_setDocumentData message. It can also be invoked by sending a
;;; zotero-setDocPrefs message, which will call tm-zotero-Document_getDocumentData,
;;; then let you edit that in Zotero's dialog, and send it back with
;;; tm-zotero-Document_setDocumentData. So from here, we never need to write the
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
(define (get-env-zoteroDocumentData documentID)
  (get-env "zoteroDocumentData"))

(define (set-env-zoteroDocumentData! documentID str_dataString)
  (set-init-env "zoteroDocumentData" str_dataString)
  (set-init-env-for-zotero-document-prefs documentID str_dataString))


(define (set-init-env-for-zotero-document-prefs documentID str_dataString)
  (let ((set-init-env-for-zotero-document-prefs-sub
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
         (set-init-env-for-zotero-document-prefs-sub "zotero-data-" (sxml-attr-list
                                                                     (car sxml)))
         (loop (sxml-content (car sxml))))
        ((eq? 'session (sxml-name (car sxml)))
         (set-init-env-for-zotero-document-prefs-sub "zotero-session-" (sxml-attr-list
                                                                        (car sxml)))
         (loop (cdr sxml)))
        ((eq? 'style (sxml-name (car sxml)))
         (set-init-env-for-zotero-document-prefs-sub "zotero-style-" (sxml-attr-list
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
;;}}}

;;{{{ Wire protocol between TeXmacs and Zotero

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
(define (close-tm-zotero-socket-port!)
  (if (and (port? tm-zotero-socket-port)
           (not (port-closed? tm-zotero-socket-port)))
      (begin
        (close-port tm-zotero-socket-port)
        (set! tm-zotero-socket-port #f))))

;;; Idempotency: If this is reloaded while TeXmacs is running, close the port on reload.
;;; I often reload this file during development by having developer-mode turned on:
;;; (set! developer-mode? #t) is in ~/.TeXmacs/progs/my-init-texmacs.scm
;;; and then using the Debug -> Execute -> Evaluate scheme expression... menu to execute:
;;; (load-from-path "zotero.scm")
;;;
(eval-when 
(when (defined? 'tm-zotero-socket-port)
  (close-tm-zotero-socket-port!))       ;; free the IP port for re-use

(define tm-zotero-socket-port #f)

;;; Dynamically allocate in case of multiple instances of TeXmacs running at the same time!
;;;(define tm-zotero-socket-inet-texmacs-port-number 23117) 
(define tm-zotero-socket-inet-zotero-port-number 23116)


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
;;;
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

(define OS-X-integration-pipe-locations
  (list
   (string-concatenate `("/Users/Shared/.zoteroIntegrationPipe_" ,(get-logname)))
   (string-concatenate `(,(getenv "HOME") "/.zoteroIntegrationPipe"))))

(define (get-tm-zotero-socket-port)
  (catch 'system-error
    (lambda ()
      (if (and (port? tm-zotero-socket-port)
               (not (port-closed? tm-zotero-socket-port)))
          tm-zotero-socket-port
          ;; (cond
          ;;   ((os-macos?)		;; Mac OS-X
          ;;    (set! tm-zotero-socket-port (socket PF_UNIX SOCK_STREAM 0))
          ;;    (cond
          ;;      ((or (and (file-exists? (first OS-X-integration-pipe-locations))
          ;;                (first OS-X-integration-pipe-locations))
          ;;           (and (file-exists? (second OS-X-integration-pipe-locations))
          ;;                (second OS-X-integration-pipe-locations)))
          ;;       => (lambda (p)
          ;;            (bind tm-zotero-socket-port AF_UNIX p)
          ;;            (connect tm-zotero-socket-port AF_UNIX p))
          ;;      (else
          ;;        (throw 'system-error "OS-X integration pipe not present")))) ;; Firefox not started yet?
          ;;    (setvbuf tm-zotero-socket-port _IOFBF)
          ;;    (set-blocking tm-zotero-socket-port)
          ;;    tm-zotero-socket-port
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
            (set! tm-zotero-socket-port (socket PF_INET SOCK_STREAM 0))
            (setsockopt tm-zotero-socket-port SOL_SOCKET SO_REUSEADDR 1)
            ;; (bind    tm-zotero-socket-port AF_INET INADDR_LOOPBACK 
            ;;          tm-zotero-socket-inet-texmacs-port-number)
            (connect tm-zotero-socket-port AF_INET INADDR_LOOPBACK
                     tm-zotero-socket-inet-zotero-port-number)
            (setvbuf tm-zotero-socket-port _IOFBF)
            (setsockopt tm-zotero-socket-port IPPROTO_TCP TCP_NODELAY 1)
            (set-blocking tm-zotero-socket-port)
            tm-zotero-socket-port
            )))
    (lambda args
      (let ((documentID (get-documentID)))
        (zt-format-error "ERR: Exception caught in get-tm-zotero-socket-port: ~s\n" args)
        (close-port tm-zotero-socket-port)
        (set! tm-zotero-socket-port #f)
      (set-document-tm-zotero-active?! documentID #f)
      (dialogue-window
       (zotero-display-alert
        documentID
        (string-append "\\begin{center}\n"
                       "Exception caught in: "
                       "\\texttt{get-tm-zotero-socket-port}\n\n"
                       "\\textbf{System Error:} " (caar (cdddr args)) "\n\n"
                       "Is Zotero running?\n\n"
                       "If so, then you may need to {\\em restart} Firefox\\\\\n"
                       "or Zotero Standalone.\n"
                       "\\end{center}\n")
        DIALOG_ICON_STOP
        DIALOG_BUTTONS_OK)
       (lambda (val)
         (noop))
       "System Error in get-tm-zotero-socket-port")
      #f))))


;; (sigaction SIGPIPE
;;   (lambda (sig)
;;     (set-message "SIGPIPE on tm-zotero-socket-port!" "Zotero integration")
;;     (hash-for-each
;;      (lambda (key val)
;;        (set-document-tm-zotero-active?! key #f))
;;      <document-data>-ht)
;;     (close-tm-zotero-socket-port!)))



(define (write-network-u32 value port)
  (let ((v (make-u32vector 1 0)))
    (u32vector-set! v 0 (htonl value))
    (uniform-vector-write v port)))

(define (read-network-u32 port)
  (let ((v (make-u32vector 1 0)))
    (uniform-vector-read! v port)
    (ntohl (u32vector-ref v 0))))


(define (tm-zotero-write tid cmd)
  (zt-format-debug "tm-zotero-write:tid:~s:cmd:~s\n" tid cmd)
  (let ((zp (get-tm-zotero-socket-port)))
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
        (let ((documentID (get-documentID)))
          (zt-format-error "ERR: System error in tm-zotero-write: ~s ~s\n" tid cmd)
          (zt-format-error "ERR: Exception caught: ~s\n" args)
          (zt-format-error "ERR: Closing Zotero port!\n")
          (close-tm-zotero-socket-port!)
          (set-document-tm-zotero-active?! documentID #f)
          (dialogue-window
           (zotero-display-alert 
            documentID
            (string-append "\\begin{center}\n"
                           "Exception caught in: "
                           "\\texttt{tm-zotero-write}\n\n"
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
          #f)))))


(define (tm-zotero-read)
  (let ((zp (get-tm-zotero-socket-port)))
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
        (zt-format-error "ERR: Exception caught in tm-zotero-read: ~s\n" args)
        (list (or tid 0) (or len 666) (format #f "ERR: System error in tm-zotero-read: ~s" args)))))) ;; return to tm-zotero-listen


(define (safe-json-string->scm str)
  (catch 'json-invalid
    (lambda ()
      (json-string->scm str))
    (lambda args
      (zt-format-error "ERR: Exception caught from json-string->scm: ~s\n" args)
      ;; return to tm-zotero-listen
      (list (format #f "ERR: Invalid JSON: ~s\n" str) '()))))


(define (safe-scm->json-string scm)
  (catch #t
    (lambda ()
      (scm->json-string scm))
    (lambda args
      (zt-format-error "ERR: Exception caught from scm->json-string: ~s\n" args)
      (zt-format-error "ERR: scm: ~s\n" scm)
      ;;
      ;; Return ERR: to caller, usually tm-zotero-write, so send to Zotero.  That
      ;; will cause Zotero to initiate an error dialog and reset back to
      ;; Document_complete state.
      ;;
      (format #f (string-append "ERR: Error! "
                                "Exception caught from scm->json-string \n\n"
                                "Exception args: ~s\n\n"
                                "scm: ~s\n")
              args scm))))


;;;
;;; It's sort of a state machine; protocol is essentially synchronous, and user
;;; expects to wait while it finishes before doing anything else anyhow.
;;;
;;; When this is entered, one of the Integration commands has just been sent to
;;; Juris-M / Zotero. Zotero will call back and begin a word processing command
;;; sequence, culminating with Document_complete.
;;;
;;;
(define (tm-zotero-listen cmd)          ; cmd is only used for system-wait display.
  (let ((documentID (get-documentID)))
    (set-document-tm-zotero-active?! documentID #t)
    (system-wait "Zotero: " cmd)
    (with (counter wait) '(40 10)
      (delayed
        (:while (get-document-tm-zotero-active? documentID))
        (:pause ((lambda () (inexact->exact (round wait)))))
        (:do (set! wait (min (* 1.01 wait) 2500)))
        ;; Only run when data is ready to be read...
        (when (char-ready? tm-zotero-socket-port)
          (with (tid len cmdstr) (tm-zotero-read)
            (zt-format-debug "tm-zotero-listen:tid:~s:len:~s:cmdstr:~s\n" tid len cmdstr)
            (if (> len 0)
                (with (editCommand args) (safe-json-string->scm cmdstr)
                  (zt-format-debug "~s\n" (list editCommand (cons tid args)))
                  (cond
                    ((and (>= (string-length editCommand) 4)
                          (string=? (string-take editCommand 4) "ERR:"))
                     (zt-format-debug "tm-zotero-listen:~s\n" editCommand)
                     (tm-zotero-write tid editCommand) ;; send the error to Zotero
                     (set! counter 40)
                     (set! wait 10)) ;; keep listening
                    ((string=? editCommand "Document_complete")
                     (zt-format-debug "tm-zotero-Document_complete called.\n")
                     (set-message "Zotero: Document complete." "Zotero integration")
                     (system-wait "Zotero: Document complete." "(soon ready)")
                     (tm-zotero-write tid (scm->json-string '()))
                     ;;(close-tm-zotero-socket-port!)
                     (set! wait 0)
                     (set-document-tm-zotero-active?! documentID #f))
                    (#t
                     ;; Todo: This traps the event where there's a syntax or
                     ;; other error in the zotero.scm program itself, and send
                     ;; the ERR: message back to Zotero, and set!
                     ;; tm-zotero-active? #f, etc. in an attempt to make it
                     ;; more robust, so that Firefox and TeXmacs don't both
                     ;; have to be restarted when this program doesn't work
                     ;; right?
                     ;;
                     ;; It did not work right. It just sits there and never
                     ;; prints the backtrace from the error to the terminal the
                     ;; way I expect, and so I can't debug it. Also, sending
                     ;; that ERR did not cause Juris-M to put up a dialog or
                     ;; anything so there's no indication of the error and the
                     ;; network protocol does not reset to the starting state
                     ;; anyway. Maybe the error condition needs to be noted and
                     ;; then handled with the next start of a command, so noted
                     ;; but tm-zotero-active? left #t until after the error
                     ;; handling?
                     ;;
                     ;; JavaScript error: file:///home/karlheg/.mozilla/firefox/yj3luajv.default/extensions/jurismOpenOfficeIntegration@juris-m.github.io/components/zoteroOpenOfficeIntegration.js, line 323: TypeError: can't access dead object
                     ;;
                     (system-wait "Zotero: " editCommand)
                     ;;(catch #t
                     ;;(lambda ()
                     (apply (eval ;; to get the function itself
                             (string->symbol 
                              (string-append "tm-zotero-"
                                             editCommand)))
                            (cons tid args))
                     ;;)
                     ;;(lambda args
                       ;;(tm-zotero-write tid (scm->json-string "ERR: TODO: Unspecified Error Caught."))
                       ;;(set! tm-zotero-active? #f)))
                   (set! counter 40)
                   (set! wait 10))))
                (begin
                  ;; Sometimes when Firefox is stopped in the middle of it,
                  ;; char-ready? returns #t but tm-zotero-read does not read
                  ;; anything... Perhaps look for eof-object?
                  (set! counter (- counter 1))
                  (when (<= counter 0)
                    (close-tm-zotero-socket-port!)
                    (set! wait 0) 
                    (set-document-tm-zotero-active?! documentID #f))))))))))

;;}}}

;;{{{ Integration commands: TeXmacs -> Zotero

;;;
;;; These expect no immediate reply packet from Zotero. Zotero will connect
;;; back with Editor integration commands, while this is "in" tm-zotero-listen.
;;;
;;; See: zotero-menu.scm
;;; See: zotero-kbd.scm
;;;
(define (call-zotero-integration-command cmd)
  (let ((documentID (get-documentID)))
    (when (not (get-document-tm-zotero-active? documentID)) ;; one at a time only
      (set-message (string-append "Calling Zotero integration command: " cmd)
                   "Zotero Integration")
      (system-wait (string-append "Calling Zotero integration command: " cmd) "")
      (let ((zp (get-tm-zotero-socket-port)))
        (if (and (port? zp)
                 (catch 'system-error
                   (lambda ()
                     (tm-zotero-write 0 (safe-scm->json-string cmd))
                     #t)
                   (lambda arg
                     #f))) ;; Firefox or Zotero Standalone not running?
            (begin
              (tm-zotero-listen cmd) ;; delayed, returns immediately.
              #t) ;; report successful initiation of integration command sequence
            (begin
              #f))))))



(define (tm-zotero-add str-kind)
  (if (not (or (== str-kind "citation")
               (== str-kind "bibliography")))
      ;; then
      (begin
        (texmacs-error "tm-zotero-add" "unknown kind ~S" str-kind)
        #f)
      ;; else
      (let* ((documentID (get-documentID))
             (new-zfieldID (get-document-new-zfieldID documentID)))
        (unless new-zfieldID ;; one at a time only
          (try-modification
            (if (and (insert-new-zfield 'zcite "{Citation}")
                     (call-zotero-integration-command "addCitation"))
                (begin ; then successful modification, so keep it.
                  #t) 
                (begin ; else
                  ;;
                  ;; The problem here is that call-zotero-integration-command
                  ;; is going to call tm-zotero-listen, which is going to
                  ;; return immediately, since most of tm-zotero-listen runs
                  ;; inside a "delay" form, thus, gives up control to the main
                  ;; GUI event loop, which eventually runs the "delay" job
                  ;; queue...
                  ;;
                  ;; So if the integration command sequence is not ultimately
                  ;; successful, in order to have the try-modification here do
                  ;; the right thing---undo the tentative insertion of a zfield
                  ;; when the TeXmacs<-->Zotero interaction is not successfully
                  ;; completed---I think I'd need to pass a continuation
                  ;; through call-zotero-integration-command then through
                  ;; tm-zotero-listen, and use some kind of dynamic-wind thing
                  ;; here that I'm not very familiar with using yet... so that
                  ;; it can return here to do this error cleanup.
                  ;;
                  ;; ... and I'm unsure whether TeXmacs can deal with if I do
                  ;; that. Can I use a continuation? Is that too "heavy" since
                  ;; it copies the C stack? Any ideas?
                  ;;
                  ;; Clear the new-zfieldID
                  (set-document-new-zfieldID! documentID #f)
                  ;; Clear the <zfield-data> for it.
                  (hash-set! (get-document-zfield-ht documentID) new-zfieldID #f)
                  #f))))))) ; unsuccessful modification, so undo it.



;;; ---------

(define (tm-zotero-addCitation)
  (let* ((documentID (get-documentID))
         (new-zfieldID (get-document-new-zfieldID documentID)))
  (unless new-zfieldID ;; one at a time only
    (try-modification
      (if (and (insert-new-zfield 'zcite "{Citation}")
               (call-zotero-integration-command "addCitation"))
          (begin ; then
            #t)  ; successful modification, so keep it.
          (begin ; else
            ;; Clear the new-zfieldID
            (set-document-new-zfieldID! documentID #f)
            ;; Clear the <zfield-data> for it.
            (hash-set! (get-document-zfield-ht documentID) new-zfieldID #f)
            #f))))) ; unsuccessful modification, so undo it.
                

(define (tm-zotero-editCitation)
  (call-zotero-integration-command "editCitation"))

;;; ---------

(define (tm-zotero-addBibliography)
  (unless zt-new-fieldID ;; one at a time only
    (try-modification
      ;; It is what Zotero expects. I'd expect to put {Bibliography} there.
      (if (and (insert-new-zfield 'zbibliography "{Bibliography}")
               (call-zotero-integration-command "addBibliography"))
          #t
          (begin
            (set! zt-new-fieldID #f)
            #f)))))


(define (tm-zotero-editBibliography)
  (call-zotero-integration-command "editBibliography"))


;;; ---------


(define (tm-zotero-refresh)
  (call-zotero-integration-command "refresh"))


;;; (define (tm-zotero-removeCodes)
;;;   (call-zotero-integration-command "removeCodes"))


;;; ---------


(define (tm-zotero-setDocPrefs)
  (call-zotero-integration-command "setDocPrefs"))

;;}}}

;;{{{ General category overloaded (tm-define)

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
;;}}}

;;{{{ Keyboard event handling (overloads to maintain <document-data>)

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
                 (focus-is-zcite?)))
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
  (let ((fieldID-str (object->string (zfield-ID t))))
    (hash-remove! zt-zfield-disactivated? zfieldID)
    (when (case (zt-zfield-modified?-or-undef t)
            ((undef)
             (zt-set-zfield-modified?! zfieldID)) ;; returns boolean
            ((#t) #t)
            ((#f) #f))
      (zotero-refresh))))
                 

(tm-define (notify-disactivated t)
  (:require (and (in-tm-zotero-style?)
                 (focus-is-zcite?)))
  (set-message "zcite disactivated." "Zotero integration")
  (let ((fieldID-str (object->string (zfield-ID t))))
    (hash-set! zt-zfield-disactivated? zfieldID #t)
    ;;
    ;; When the tag is disactivated, and the zcite-Text has not been modified
    ;; from the original text that was set by Juris-M / Zotero, then this
    ;; refresh will catch any modifications to the reference database made
    ;; there. So if you modify the reference database item for the citation
    ;; cluster of this zcite tag and disactivate the tag, you'll see the
    ;; zcite-Text update.
    ;;
    (when (not (eq? #t (zt-zfield-modified?-or-undef zfieldID)));; might be 'undef
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
    (hash-remove! zt-zfield-modified?-cache zfieldID)))



;;; Todo: I want to be able to easily split a citation cluster.
;;;
;;; Use case: A citation cluster with two or three citations in it, but then I
;;;           decide that I want to split them into two clusters, one for the
;;;           first citation, and another for the remaining two, so that I can
;;;           write a sentence or two in between them.
;;;
;;; So disactivate the tag, then inside of there, a keybinding can automate it,
;;; perhaps when the cursor is on the semicolon between two of them or
;;; something like that. Inside of the zfield-Code's JSON is the information
;;; that Zotero's integration.js is going to look at when it retrieves it prior
;;; to presenting the dialog for editCitation. So, instead of copy + paste of
;;; the original citation in order to duplicate it, followed by editCitation of
;;; each, to delete the last two of them from the first cluster, and the first
;;; citation from the second one... I would put my cursor on the semicolon
;;; between the first two citations, and then push the keybinding or call the
;;; menu item that automatically splits it.


;;; Todo: I think it is spending too much time searching the document for zcite
;;;       tags. It does a lot of redundant traversal of the document tree that
;;;       can potentially be eliminated by maintaining a data structure
;;;       containing positions (really observers). Positions move automatically
;;;       when the document is edited, so that they remain attached to the same
;;;       tree they were created at, even as it moves.
;;;
;;; The data structure must be able to maintain the list of zfields in document
;;; order. It should be cheap to insert a new item or remove an item. I will
;;; use a red-black tree. It will contain only the positions, in document
;;; order. I will look through those positions to find the zfield-Code and
;;; zfield-Text; the zfield-ID can be the key to a concurrently maintained
;;; hashtable that associates the ids with their positions.
;;;
;;; Update: There is no ready-made rb-tree for Guile 1.8. The only rb-tree I
;;;         could find that was already for Guile was for >= 2.0, and it calls
;;;         for r6rs functionality that is not present in Guile 1.8. It would
;;;         take a lot of time and effort to port that, and I think it's better
;;;         to spend the time to port all of TeXmacs to Guile 2.n
;;;         instead. Since the `merge!' and `list-filter' functions don't have
;;;         to do very much work compared to an rb-tree's insert or delete with
;;;         all of it's associated tree balancing, up to a certain length, it
;;;         will be faster than the rb-tree anyway... so I'll just use a flat
;;;         list and `merge!' to insert, and `list-filter' to remove. After the
;;;         Guile 2.n port, perhaps an rb-tree can be used instead.



;;; Todo: I want to observe the cut or paste of trees that contain zcite or
;;;       zbibliography sub-trees...
;;;
;;; This is a prerequisite for being able to maintain an rb-tree of document
;;; positions of zfields...

;;; Mise en Place: Functions I'll need and what they do.
;;;  (starting by looking around in the src/Scheme/Glue, following the
;;;  functions called from inside of the glue functions to their origin, and
;;;  learning how the objects they are methods of interact with
;;;  TeXmacs... Using cscope or etags...)
;;;
;;; A "position" is essentially a C++ "observer".
;;;
;;;  position-new-path path          => position
;;;  position-delete   position      => unspecified
;;;  position-set      position path => unspecified
;;;  position-get      position      => path
;;;
;;; path-less?    path path => bool
;;; path-less-eq? path path => bool
;;; path->tree path => tree
;;;

;;; `buffer-notify' from (part part-shared) is what I was looking for.  It also
;;; defines `buffer-initialize', and both are called from
;;; `tm_buffer_rep::attach_notifier()' in new_buffer.cpp, which is called by
;;; `buffer-attach-notifier'. Thus, both must be defined here for this to work
;;; right since this is not a shared buffer.
;;;
;;;
;;; buffer-attach-notifier ultimately calls a c++ function that invokes first
;;; buffer-initialize, and then attaches the buffer-notify via a
;;; scheme_observer. So buffer-initialize is *not* where to call
;;; buffer-attach-notifier... I want to do that once, from some point of entry
;;; that is called once when the buffer is first loaded, for the case of a
;;; pre-existing document, or once when the style is first added to the
;;; document.
;;;
;;; So the first thing that happens after a document is loaded into a buffer is
;;; that the typesetter takes off, to render the display. It must initialize
;;; the styles for the document... and then

;; (tm-define (set-main-style style)
;;   (former style)
;;   (when (style-includes? style "tm-zotero")
;;     (tm-zotero-document-buffer-attach-notifier (get-documentID))))


;; (tm-define (add-style-package pack)
;;   (former pack)
;;   (when (== pack "tm-zotero")
;;     (tm-zotero-document-buffer-attach-notifier (get-documentID))))


;;; FixMe: Notice that these do not call (former id t buf) since the bottom of
;;; that stack is the (part shared-part) version... which does not presently
;;; specialize upon whether the document actually has any shared parts! That
;;; also implies that this won't play well with a buffer that does have shared
;;; parts...
;;;
;; (tm-define (buffer-initialize id t buf)
;;   (:require in-tm-zotero-style?)
;;   (noop))

;;;
;;; event can be: 'announce, 'touched, or 'done.
;;;
;;; modification-type can be:
;;;  'assign, 'insert, 'remove, 'split, 'var-split, 'join, 'var-join,
;;;  'assign-node, 'insert-node, 'remove-node, 'detach
;;;
;; (tm-define (buffer-notify event t mod)
;;   (:require in-tm-zotero-style?)
;;   (let* ((modtype (modification-type mod))
;;          (modpath (modification-path mod))
;;          (modtree (modification-tree mod))
;;          (modstree (tm->stree modtree)))
;;     (zt-format-debug "~sbuffer-notify:~s ~sevent:~s~s\n~st:~s~s\n~smod:~s~s\n\n"
;;                      ansi-red ansi-norm 
;;                      ansi-cyan event ansi-norm
;;                      ansi-cyan t ansi-norm
;;                      ansi-cyan ansi-norm (modification->scheme mod))
;;   (cond
;;     ((and (== event 'done)
;;           (== modtype 'assign)
;;           (== (car modstree) 'concat)
;;           (member (cadr modstree) zfield-tags))
;;      ;; Inserting (pasting) a zcite or zbibliography that had been cut.
;;      )
;;     ((and (== event 'done)
;;           (noop
;;            )
;;           )))))

;; (tm-define (buffer-notify event t mod)
;;;; I don't like this slot-ref here... is it going to be too slow? Will it run often?
;;   (:require (let ((zt-zfield-list (slot-ref (get-<document-data> (get-documentID))
;;                                             'zfield-ls)))
;;               (and (in-tm-zotero-style?)
;;                    (pair? zt-zfield-list)
;;                    (null? zt-zfield-list))))
;;   (zt-init-zfield-list)
;;   (former event t mod))


;;; Let's try using key-events instead, to avoid what I think will be a lot of
;;; overhead with lots of calls to the buffer-notify, like for every
;;; keypush. Instead, a key-bound function happens only on the event of that
;;; key being pushed...
;;;
;;; generic/generic-kbd.scm has kbd-map definitions in it. After some
;;; exploration, I see that the functions that I'll need to overload for sure
;;; are: clipboard-cut and clipboard-paste.


;;; This is called by both kbd-backspace and kbd-delete...
;;;
;;; I don't know what t is going to be. What about when it is (tree-is-buffer?
;;; t)?  Should I check for the section? And I don't want the backspace key to
;;; now disactivate the tag... So I need to only do anything when the area
;;; being removed is the selection.
;;;
;; (tm-define (kbd-remove t forwards?)
;;   (:require (and (in-tm-zotero-style?)
;;                  ;; (tree-is-buffer? t) ?
;;                  (tree-in? t zfield-tags)
;;                  (with-any-selection?)))
;;   ;;; for each zfield in t, remove it from the <document-data>.
;;   (prior t forwards)
;;   ;; (clipboard-cut "nowhere")
;;   ;; (clipboard-clear "nowhere")
;;   )

;;; ? kbd-insert
;;; ? kbd-select
;;; ? kbd-select-environment
;;; ? kbd-tab, kbd-variant

;;; clipboard-clear
;;; clipboard-copy
;;; clipboard-cut
;;; clipboard-cut-at
;;; clipboard-cut-between
;;; clipboard-get
;;; clipboard-paste
;;; clipboard-set
;;; tree-cut

;;; kill-paragraph
;;; yank-paragraph

;;; See: fold-edit.scm, etc. for examples.

;;; Also: db-edit.scm, at structured-remove-horizontal
;;; selections.scm

;;; clipboard-cut and clipboard-paste are overloaded in fold-edit.scm.


(tm-define (clipboard-cut which)
  (:require (and (in-tm-zotero-style?)
                 (in-text?)
                 ))

  (prior which) ;; ?
  )

(define (has-zfields? t)
  (tm-find t is-zfield?))

;;; untested
(tm-define (clipboard-paste which)
  (:require (and (in-tm-zotero-style?)
                 (not (focus-is-zfield?))
                 (in-text?)
                 (has-zfields? (clipboard-get which))))
  (let* ((t (clipboard-get which))
         (zfields (tm-search t is-zfield?)))
    (map (lambda (zfield)
           (tree-set! (get-zfield-zfieldID-t zfield) 
                      (stree->tree (get-new-fieldID))))
         zfields)
    (insert t)
    ;; todo: maintain the new data-structures here.
    ))

;;}}}

;;{{{ Preferences and Settings (with-like, global, document-wide)
;;;
;;; Todo: Invent a good naming convention for the below preferences and
;;; settings... There must be a differentiation between editor-wide
;;; preferences, document-wide ones, and ones that have either an explicit or
;;; implicit document-wide default that can be overrided locally by using a
;;; with-wrapper. Further, there are some that are not to be exposed to the end
;;; user, and others that are.
;;;
;;;  Idea: Make ones that are to be hidden have a special naming convention to
;;;  make it easier to implement the below functions which are used to
;;;  determine what to show in the toolbar menus.
;;;
;;; Todo: See (utils base environment), extend that logic-table with the ones
;;; for this? Can those be contextually overloaded? I guess it doesn't
;;; matter. It's just a variable identifier to description string mapping.
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
   (and (or (focus-is-zfield?)
            (focus-is-ztHref?))
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
   (and (focus-is-zbibliography?)
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
  (:require (and (focus-is-zbibliography?)
                 (== var "zbibColumns")))
  (list "1" "2"))

(tm-define (parameter-choice-list var)
  (:require (and (focus-is-zbibliography?)
                 (== var "zbibPageBefore")))
  (list "0" "1" "2"))


(tm-define (focus-tag-name l)
  (:require (focus-is-zfield?))
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
  (:require (and (focus-is-zcite?)
                 (!= (get-env "zotero-pref-noteType0") "true")
                 (or (== (get-env "zotero-pref-noteType1") "true")
                     (== (get-env "zotero-pref-noteType2") "true"))
                 (!= (get-env "zt-in-footnote") "true")
                 (!= (get-env "zt-in-endnote") "true")))
  (list (list "zt-option-this-zcite-in-text" "Force in-text?")
        ))


(tm-define (parameter-choice-list var)
  (:require (and (focus-is-zcite?)
                 (== var "zt-option-this-zcite-in-text")))
  (list "true" "false"))


(tm-define (hidden-child? t i)
  (:require (focus-is-zcite?))
  #f)
        

;;; Todo: go to next similar tag does not work right with zcite. Why?
;;; The following seems to have no effect...

;;; Ok, it might not be zcite; it might be everything. Tried with a \strong text block and got the same error.  Fails when there's
;;; only 1 \paragraph, but works when there's 2, but trying to go past last one gives same error.  I think this used to work, but
;;; now it does not. I can't fix it today.

;; (tm-define (similar-to lab)
;;   (:require (focus-is-zcite?))
;;   (list 'zcite))

;; (tm-define (similar-to lab)
;;   (:require (focus-is-zbibliography?))
;;   (list 'zbibliography))



(define (zt-notify-debug-trace var val)
  (set-message (string-append "zt-debug-trace? set to " val)
               "Zotero integration")
  (set! zt-debug-trace? (== val "on")))


(define-preferences
  ("zt-debug-trace?" "off" zt-notify-debug-trace))

;;; these need to be per-document preferences, not TeXmacs-wide ones.
  ;; ("zt-pref-in-text-hrefs-as-footnotes"         "on"  ignore)
  ;; ("zt-pref-in-text-hlinks-have-href-footnotes" "on"  ignore))

;;}}}

;;{{{ Word Processor commands: Zotero -> TeXmacs

;;;
;;; Each sends: [CommandName, [Parameters,...]].
;;;
;;; The response is expected to be a JSON encoded payload, or the unquoted and
;;; unescaped string: ERR: Error string goes here

;;{{{ Application_getActiveDocument
;;;
;;; Gets information about the client and the currently active
;;; document. documentID can be an integer or a string.
;;;
;;; ["Application_getActiveDocument", [int_protocolVersion]] -> [int_protocolVersion, documentID]
;;;
(define (zotero-Application_getActiveDocument tid pv)
  (zt-format-debug "zotero-Application_getActiveDocument called.\n")
  (tm-zotero-write tid (safe-scm->json-string (list pv (get-documentID)))))

;;}}}

;;{{{ Document_displayAlert

;;{{{ Alert dialog widget

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

;;}}}

;;; Shows an alert.
;;;
;;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;;
(define (tm-zotero-Document_displayAlert tid documentID str_dialogText int_icon
                                         int_buttons)
  (zt-format-debug "tm-zotero-Document_displayAlert called.\n")
  (dialogue-window (zotero-display-alert documentID str_dialogText int_icon int_buttons)
                   (lambda (val)
                     (tm-zotero-write tid (safe-scm->json-string val)))
                   "Zotero Alert!"))

;;}}}
;;{{{ Document_activate

;;;
;;; Brings the document to the foreground. (For OpenOffice, this is a no-op on non-Mac systems.)
;;;
;;; ["Document_activate", [documentID]] -> null
;;;
(define (tm-zotero-Document_activate tid documentID)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_canInsertField

;;;
;;; Indicates whether a field can be inserted at the current cursor position.
;;;
;;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;;
(define (tm-zotero-Document_canInsertField tid documentID str_fieldType)
  (zt-format-debug "tm-zotero-Document_canInsertField called.\n")
  (let ((ret (not
              (not
               (and (in-text?)
                    (not (in-math?))
                    (if (focus-is-zfield?)
                        (let ((t (focus-tree)))
                          (zt-format-debug "tm-zotero-Document_canInsertField:focus-is-zfield? => #t, (focus-tree) => ~s\n" t)
                          (or (and zt-new-fieldID
                                   (string=? zt-new-fieldID
                                             (object->string (zfield-ID t))))
                              #f))
                        #t))))))
    (tm-zotero-write tid (safe-scm->json-string ret))))

;;}}}
;;{{{ Document_getDocumentData

;;; Retrieves data string set by setDocumentData.
;;;
;;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;;
(define (tm-zotero-Document_getDocumentData tid documentID)
  (zt-format-debug "tm-zotero-Document_getDocumentData called.\n")
  (tm-zotero-write tid (safe-scm->json-string (get-zotero-DocumentData documentID))))

;;}}}
;;{{{ Document_setDocumentData

;;;
;;; Stores a document-specific persistent data string. This data
;;; contains the style ID and other user preferences.
;;;
;;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;;
(define (tm-zotero-Document_setDocumentData tid documentID str_dataString)
  (zt-format-debug "tm-zotero-Document_setDocumentData called.\n")
  (zt-set-DocumentData documentID str_dataString)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_cursorInField
;;;
;;; Indicates whether the cursor is in a given field. If it is, returns
;;; information about that field. Returns null, indicating that the cursor isn't
;;; in a field of this fieldType, or a 3 element array containing:
;;;
;;; zfieldID, int or string, A unique identifier corresponding to this field.
;;;
;;; fieldCode, UTF-8 string, The code stored within this field.
;;;
;;; noteIndex, int, The number of the footnote in which this field resides, or 0
;;;                 if the field is not in a footnote.
;;;
;;; ["Document_cursorInField", [documentID, str_fieldType]] -> null || [fieldID, fieldCode, int_noteIndex]
;;;
(define (tm-zotero-Document_cursorInField tid documentID str_fieldType)
  (zt-format-debug "tm-zotero-Document_cursorInField called.\n")
  (let ((ret
         (if (focus-is-zfield?)
             (begin
               (zt-format-debug "tm-zotero-Document_cursorInField: focus-is-zfield? => #t\n")
               (let* ((t (focus-tree))
                      (id (object->string (zfield-ID t))))
                 (if (not (and zt-new-fieldID
                               (string=? zt-new-fieldID id)))
                     (begin
                       (let ((code (zt-get-zfield-Code-string t))
                             (ni (object->string (get-zfield-NoteIndex t))))
                         (zt-format-debug
                          "tm-zotero-Document_cursorInField:id:~s:code:~s:ni:~s\n"
                          id code ni)
                         (list id code ni)))
                     '()))) ;; is the new field not finalized by Document_insertField
             '()))) ;; not tree-in? zfield-tags
    (tm-zotero-write tid (safe-scm->json-string ret))))

;;}}}
;;{{{ Document_insertField (bottom half or callback half)

;;; Inserts a new field at the current cursor position. Because there has to be
;;; time for the typesetting to run in order for it to create the footnote
;;; number and set the reference bindings for the noteIndex, by the time this
;;; routine is being called by Zotero, TeXmacs will have already inserted the
;;; new field (see insert-new-zfield, above) but in a pending state, finalized
;;; by this.
;;;
;;; tm-zotero cannot keep track of the noteIndex itself since it's not the only
;;; thing inserting footnotes. The user can insert them too, and so either this
;;; would have to keep track of those... but that's not necessary and is too
;;; costly... It naturally lets the typesetter run between insert-new-zfield
;;; and tm-zotero-Document_insertField due to the "delay" form in
;;; tm-zotero-listen, and so that sets up the reference binding so we can look
;;; up the noteIndex through the TeXmacs typesetter. See get-refbinding, above,
;;; and get-zfield-NoteIndex-str, above.
;;;
;;;
;;; str_fieldType, either "ReferenceMark" or "Bookmark"
;;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;;
;;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]
;;;
;;; Ignore: str_fieldType, since this does not distinguish between
;;;         ReferenceMark and Bookmark like LibreOffice or Word do.
;;;
;;; Ignore: int_noteType, which I am not using from here either. I assume that
;;;         when the document's CSL style and this document's Zotero document
;;;         prefs say it's a note style, then every citation not individualy
;;;         and explicitly marked as in-text will just go into either a
;;;         footnote or an endnote.
;;;
(define (tm-zotero-Document_insertField tid documentID
                                        str_fieldType
                                        int_noteType)
  (zt-format-debug "tm-zotero-Document_insertField called.\n")
  (let* ((new-zfieldID (get-document-new-zfieldID documentID))
         (new-zfield-zfd (and new-zfieldID
                              (get-document-<zfield-data>-by-zfieldID new-zfieldID)))
         (new-zfield (and new-zfield-zfd
                          (get-document-zfield-by-zfieldID documentID new-zfieldID)))
         (new-noteIndex (and new-zfield
                             (get-zfield-NoteIndex-str new-zfieldID))))
    (if new-zfield
        ;; then
        (begin
          (set-document-new-zfieldID! documentID #f) ; clear it
          
          ;; Report success to Zotero.
          (tm-zotero-write tid (safe-scm->json-string
                                (list id ""
                                      (object->string (get-zfield-NoteIndex new-zfieldID)))))
          )
        ;; else
        (tm-zotero-write tid (safe-scm->json-string "ERR:no new-zfield in tm-zotero-Document_insertField???")))))

;;}}}
;;{{{ Document_getFields

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
(define (tm-zotero-Document_getFields tid documentID str_fieldType)
  (zt-format-debug "tm-zotero-Document_getFields called.\n")
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
                         (cons (object->string (zfield-ID field)) ids)
                         (cons (zt-get-zfield-Code-string field) codes)
                         (cons (object->string (get-zfield-NoteIndex
                                                field)) indx))))))))
    (tm-zotero-write tid (safe-scm->json-string ret))))

;;}}}
;;{{{ Document_convert

;;; ["Document_convert" ??? (TODO in documentation.)
;;;
;;; public void convert(ReferenceMark mark, String fieldType, int noteType)
;;;
;;; I think this is for OpenOffice to convert a document from using
;;; ReferenceMark fields to Bookmark ones.  Maybe we could repurpose this for
;;; TeXmacs?  Better to make a new flag; and just ignore this one?
;;;
(define (tm-zotero-Document_convert tid . args)
  (zt-format-debug "tm-zotero-Document_convert called.\n")
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_setBibliographyStyle

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

;;{{{ Notes made during R&D
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
;;}}}

;;{{{ Length calculations

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

(define (tm-zotero-lineSpacing->tmlen meas)
  (let ((sep-mult (/ (if (= meas 0) 240 meas)
                     240)))
    (format #f "~,4f" (exact->inexact sep-mult)))) ;; times par-sep

(define (tm-zotero-entrySpacing->tmlen meas)
  (let ((sep-mult (/ (if (= meas 0) 240 meas)
                     240)))
    (format #f "~,4f" (exact->inexact sep-mult)))) ;; times item-vsep

(define (tm-zotero-firstLineIndent->tmlen meas)
  (let ((indent-tabs (/ meas 360))) ; can be zero
    (format #f "~,4ftab" (exact->inexact indent-tabs))))

(define (tm-zotero-bodyIndent->tmlen meas)
  (let ((indent-tabs (/ meas 360))) ; can be zero
    (format #f "~,4ftab" (exact->inexact indent-tabs))))


(define (tm-zotero-tabstop-arrayList->tmlen-ls tab-ls)
  (let loop ((tab-ls tab-ls)
             (ret '()))
    (cond
     ((null? tab-ls)
      (stree->tree `(tuple ,@(reverse! ret))))
      (#t (loop (cdr tab-ls)
                (cons (format #f "~,4ftab"
                              (exact->inexact
                               (/ (car tab-ls) 360)))
                      ret))))))

(define (tm-zotero-read-tabstop-arrayList)
  (with-input-from-string
      (get-env "zotero-BibliographyStyle_arrayList")
    (lambda () (read (current-input-port)))))

(define (tm-zotero-get-tabStopCount)
  (string->number
   (get-env "ztbibItemIndentTabN")))

;;}}}

(define (tm-zotero-Document_setBibliographyStyle
         tid documentID
         firstLineIndent bodyIndent lineSpacing entrySpacing
         arrayList tabStopCount)
  (zt-format-debug "tm-zotero-Document_setBibliographyStyle called.\n")
  (set-init-env "zotero-BibliographyStyle_firstLineIndent"
                (tm-zotero-firstLineIndent->tmlen firstLineIndent))
  (set-init-env "zotero-BibliographyStyle_bodyIndent"
                (tm-zotero-bodyIndent->tmlen bodyIndent))
  (set-init-env "zotero-BibliographyStyle_lineSpacing"
                (tm-zotero-lineSpacing->tmlen lineSpacing))
  (set-init-env "zotero-BibliographyStyle_entrySpacing"
                (tm-zotero-entrySpacing->tmlen entrySpacing))
  (set-init-env "zotero-BibliographyStyle_arrayList"
                (tm-zotero-tabstop-arrayList->tmlen-ls arrayList))
  (set-init-env "zotero-BibliographyStyle_tabStopCount"
                (format #f "~s" tabStopCount))
  ;;
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_cleanup

;;; Not documented, but exists in CommMessage.java in LibreOffice side of the
;;; connector. It appears to do nothing there either.
;;;
(define (tm-zotero-Document_cleanup tid documentID)
  (zt-format-debug "STUB:tm-zotero-Document_cleanup: ~s\n" documentID)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_complete (see tm-zotero-listen)

;;; Indicates that the given documentID will no longer be used and
;;; associated resources may be freed.
;;;
;;; ["Document_complete", [documentID]] -> null
;;;
;;; See: tm-zotero-listen, where this is checked for inline... but also enable it here since I might need to use it during
;;; development, at least. It's never called at all by tm-zotero-listen, so can just be commented off here.
;;;
;;; (tm-define (tm-zotero-Document_complete tid documentID)
;;;   (tm-zotero-write tid (safe-scm->json-string '()) )
;;;   (set! tm-zotero-active? #f)
;;;   ;; (close-tm-zotero-socket-port!)
;;;   )

;;}}}

;;{{{ Field_delete

;;; Deletes a field from the document (both its code and its contents).
;;;
;;; When I choose addCitation and then cancel without selecting one, it returns
;;; and immediately calls this function.
;;;
;;; zfieldID as originally returned by Document_cursorInField,
;;; Document_insertField, or Document_getFields.
;;;
;;; ["Field_delete", [documentID, fieldID]] -> null
;;;
(define (tm-zotero-Field_delete tid documentID zfieldID)
  (zt-format-debug "tm-zotero-Field_delete called.\n")
  (let* ((field (zt-find-zfield zfieldID))
         (code (and field (zt-zfield-Code field)))
         (text (and field (get-zfield-Text field))))
    (when field
      ;; clear from zt-zfield-Code-cache via the function in case it needs to
      ;; anything special later on.
      (zt-set-zfield-Code-from-string field "")
      (tree-set! field "")))
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_select

;;; Moves the current cursor position to encompass a field.
;;;
;;; ["Field_select", [documentID, fieldID]] -> null
;;;
;;; I think that whether or not this works as expected depends on settings made
;;; by the drd-props macro. I think that I want the cursor to be inside of it's
;;; light blue box, after it.... (writing this comment prior to testing. FLW.)
;;;
(define (tm-zotero-Field_select tid documentID zfieldID)
  (zt-format-debug "tm-zotero-Field_select called.\n")
  (go-to-document-zfield-by-zfieldID documentID zfieldID)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_removeCode

;;;
;;; ["Field_removeCode", [documentID, fieldID]] -> null
;;;
(define (tm-zotero-Field_removeCode tid documentID zfieldID)
  (zt-format-debug "tm-zotero-Field_removeCode called.\n")
  (let* ((field (zt-find-zfield zfieldID))
         (code (and field (zt-zfield-Code field))))
    (when code
      (tree-set! code "")))
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_setText
;;{{{ Notes made during R&D

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

;;}}}

;;{{{ Special handling of hyperlinks in citations and bibliography

;;;
;;; Todo: in tmtex.scm I find:
;;;
;;;    (if (string-starts? l "bib-") (string-drop l 4) l)
;;;
;;; ... and I think it's easier to read than using the substring as below...
;;;
;;; Todo: in document-part.scm, I find:
;;;
;;; (define (buffer-hide-preamble)
;;;   (with t (buffer-tree)
;;;     (when (match? t '(document (show-preamble :%1) (ignore (document :*))))
;;;       (tree-assign! t `(document (hide-preamble ,(tree-ref t 0 0))
;;; 				 ,@(tree-children (tree-ref t 1 0)))))))
;;;
;;; So... consider rewrite in terms of (cond ... (match? ...
;;;

(define (move-link-to-own-line lnk)
  "Move links to their own line, in smaller text, so that long links
will not overflow into the page margins. Keep punctuation before and after,
including parentheses and <less> <gtr> around the link put there by some
styles."
  (zt-format-debug "move-link-to-own-line called.\n")
  (let* ((pre-lnk-txt (tree-ref (tree-up lnk) (- (tree-index lnk) 1)))
         (pre-lnk-str (and pre-lnk-txt (tree->stree pre-lnk-txt)))
         (post-lnk-txt (tree-ref (tree-up lnk) (+ (tree-index lnk) 1)))
         (post-lnk-str (and post-lnk-txt (tree->stree post-lnk-txt)))
         (is-doi? (and (string? pre-lnk-str)
                       (or (string-suffix? "doi:" pre-lnk-str)
                           (string-suffix? "doi: " pre-lnk-str)
                           (string-suffix? "doi: " pre-lnk-str)))))
    (zt-format-debug "lnk before: ~s\n" lnk)
    (zt-format-debug "pre-lnk-str: ~s\n" pre-lnk-str)
    (zt-format-debug "post-lnk-str: ~s\n" post-lnk-str)
    (unless is-doi?
      (zt-format-debug "is-doi? => #f\n")
      (when (string? pre-lnk-str)
        (cond
          ((and (string? post-lnk-str)
                (string-suffix? "<less>" pre-lnk-str)
                (string-prefix? "<gtr>" post-lnk-str))
           ;; Keep link wrapped in <less> <gtr> and put on it's own line
           ;; (zt-format-debug
           ;;  "Keep link wrapped in <less> <gtr> and put on it's own line (1).\n")
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
           ;; (zt-format-debug
           ;;  "Keep link wrapped in <less> <gtr> and put on it's own line (2).\n")
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
                ;;(zt-format-debug "Keep link next to the prefix text.\n")
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
            ;;(zt-format-debug "Punctuation: '~s'" (car strs))
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
    (zt-format-debug "lnk after: ~s\n" lnk))
  lnk)


;; (define (delete-one-space-to-left-of lnk)
;;   (zt-format-debug "delete-one-space-to-left-of called.\n")
;;   (let* ((pre-lnk-txt (tree-ref (tree-up lnk) (- (tree-index lnk) 1)))
;;          (pre-lnk-str (and pre-lnk-txt (tree->stree pre-lnk-txt))))
;;     (when (or (string-suffix? " " pre-lnk-str)
;;               (string-suffix? " " pre-lnk-str))
;;       (set! pre-lnk-str (substring pre-lnk-str
;;                                    0
;;                                    (- (string-length pre-lnk-str)
;;                                       1)))
;;       (tree-set! pre-lnk-txt (stree->tree pre-lnk-str)))))



;; (define (fixup-embedded-slink-as-url lnk)
;;   (zt-format-debug "fixup-embedded-slink-as-url called.\n")
;;   (cond
;;     ((and (tree-in? lnk '(ztHrefFromBibToURL ztHrefFromCiteToBib))
;;           (tree-in? (tree-ref lnk 1) '(slink verbatim)))
;;      (let ((slink-or-verbatim (tree-ref lnk 1)))
;;        (tree-set! slink-or-verbatim (tree-ref slink-or-verbatim 0)))))
;;   lnk)

(define (fixup-embedded-slink-as-url lnk)
  (when (match? lnk '((:or ztHrefFromBibToURL ztHrefFromCiteToBib) :%1 ((:or slink verbatim) :%1)))
    (tree-assign! lnk `(,(tree-label lnk) ,(tree-ref lnk 0) ,(tree-ref lnk 1 0)))))

;;}}}

;;{{{ Regexp transformations of UTF-8 string sent by Zotero

;;{{{ Notes made during R&D

;;;  tid:10 len:190 cmdstr:"[\"Field_setText\",[\"10724-(1)\",\"+3LuhRbmY22me9N\",\"\\\\textit{Statutes in derogation of
;;; common law not strictly construed --- Rules of equity prevail.}, Title 68, Chapter 3 § 2 (2014).\",false]]"
;;;
;;;  ("Field_setText" (10 "10724-(1)" "+3LuhRbmY22me9N" "\\textit{Statutes in derogation of common law not strictly construed
;;; --- Rules of equity prevail.}, Title 68, Chapter 3 § 2 (2014)." #f))
;;;
;;; tm-zotero-UTF-8-str_text->texmacs:t before: <tree <with|font-shape|italic|Statutes in derogation of common law not strictly
;;; construed \V Rules of equity prevail.>, Title 68, Chapter 3 � 2 (2014).>
;;;
;;; tm-zotero-UTF-8-str_text->texmacs:select lt: ()
;;;
;;; tm-zotero-UTF-8-str_text->texmacs:t after: <tree <with|font-shape|italic|Statutes in derogation of common law not strictly
;;; construed \V Rules of equity prevail.>, Title 68, Chapter 3 � 2 (2014).>
;;;
;;;  tm-zotero-write: 10 "null"
;;;
;;;  tid:11 len:49 cmdstr:"[\"Field_getText\",[\"10724-(1)\",\"+3LuhRbmY22me9N\"]]"
;;;
;;;  ("Field_getText" (11 "10724-(1)" "+3LuhRbmY22me9N"))
;;;
;;;  tm-zotero-write: 11 "\"(concat (with \\\"font-shape\\\" \\\"italic\\\" \\\"Statutes in derogation of common law not strictly
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

;;}}}

;;; Todo: Perhaps this ought to be configurable, by making it possible for the
;;; user to put their own ones into a separate configuration file?
;;;
(define tm-zotero-regex-replace-clauses
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
         ;; For: Utah Code 78B-7-115
         ;; Title: 05UC#@078B#@07#@115#@Dismissal of Protective Order
         ;;
         ;; Notice that using the prefix 03USC#@, I get sorting to 3rd category, and the string USC to search with for finding
         ;; it. This stripping of the prefix must happen prior to the abbrev substitutions below or the USC will get replaced in the
         ;; sorting prefix, leaving 03\abbrev{U.S.C.}#@ there, which is not what I want, obviously.
         ;;
         ;; Perhaps ideally the CSL should sort them according to a special sort macro designed for sorting the USC laws into the
         ;; correct order, and then the Juris-M / Zotero user interface ought to be able to sort them in the same order. But for
         ;; now, it doesn't do that, but this makes sorting them by title group them together and in the expected (defined) order.
         ;;
         ;; Adding _ and -, allows:
         ;;
         ;; Title: 03_42USC_02000ee1#@Privacy and civil liberties officers.
         ;; Title: 05_UC_78B-07-115#@Dismissal of Protective Order
         ;;
         ;; All this does is strip the prefix off of the title of the item, so the prefix is used for sorting, in both the
         ;; user-interface and bibliography, but not for rendering the citation. It of course assumes that normally titles don't
         ;; contain strings that match this pattern.
         ;;
         ;; Putting the USC or UC and the law number in the prefix allows it to be sorted by law number, and also provides a search
         ;; string that is very usable when you want to cite a particular statute. To find Utah Code items, I can just type UC_ and
         ;; it narrows to those, etc.
         ;;
         (("(([0-9][-.0-9a-zA-Z]+#@)+)")
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
         (("(\\ParagraphSignGlyph\\{\\})([  ])")
          pre 1 "\\hspace{0.5spc}" post)
         (("(§)")
          pre "\\SectionSignGlyph{}" post)
         (("(\\SectionSignGlyph\\{\\})([  ])")
          pre 1 "\\hspace{0.5spc}" post)
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
         (("(Dr\\.|Mr\\.|Mrs\\.|Jr\\.|PhD\\.|Jd\\.|Md\\.|Inc\\.|Envtl\\.|Cir\\.|Sup\\.|Ct\\.|App\\.|U\\.|Mass\\.|Const\\.|art\\.|Art\\.|sec\\.|Sec\\.|ch\\.|Ch\\.|para\\.|Para\\.|Loy\\.|Rev\\.)")
          pre "\\abbr{" 1 "}" post)
         (("(Cal\\.|Kan\\.)")
          pre "\\abbr{" 1 "}" post)
         (("([A-Z]\\.)([  ])")
          pre "\\abbr{" 1 "}" 2 post)
         )))

;;; ("<abbr>([^<]+)</abbr>"
;;;  pre "\\abbr{" 1 "}" post)

;;; What this suggests the need for is a way to add new ones to it on-the-fly,
;;; with no need to reload the editor extension. It might also be useful to
;;; have something like the regexp-opt that there is in GNU Emacs.


(define (tm-zotero-regex-transform str_text)
  (set-message "Zotero: regex transform..." "Zotero integration")
  (zt-format-debug "tm-zotero-regex-transform:before...\n")
  (let ((text str_text))
    (do ((rc tm-zotero-regex-replace-clauses (cdr rc)))
        ((null? rc)
         text)
      ;; each is applied in turn, so later ones can modify results of earlier
      ;; ones if you like.
      ;;(zt-format-debug "tm-zotero-regex-transform:during:text: ~S\n" text)
      (apply regexp-substitute/global `(#f ,(caar rc) ,text ,@(cdar rc))))))


(cond-expand
  (guile-2
   (define creturn #\return))
  (else
    (define creturn #\cr)))
(define cnewline #\newline)

;;;
;;; This runs for both in-text or note citations as well as for the bibliography.
;;;
;;; Todo: It spends a looonnnngggg time in here when typesetting a large zbibliography.
;;;
(define (tm-zotero-UTF-8-str_text->texmacs str_text is-note? is-bib?)
  (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs called... !!!\n")
  ;; With a monkey-patched Juris-M / Zotero, even when the real outputFormat is
  ;; bbl rather than rtf, the integration.js doesn't know that, and wraps
  ;; strings in {\rtf ,Body}. This removes it when it has done that.
  ;;
  ;; Conveniently, it also pastes together the bibliography with \r\n. Some
  ;; regex run on very large strings can take a very long time to finish
  ;; running. The same regex run on a much shorter string will finish
  ;; relatively quickly. So, when the str_text is very long, it will have \r\n
  ;; in it, and we can split it into multiple strings at those points, then
  ;; paste them back together again after, with \n. There was already a regex
  ;; for s,\r\n,\n, and this replaces it.
  ;;
  (let* ((str_text (if (string-prefix? "{\\rtf " str_text)
                       (substring str_text 6 (1- (string-length str_text)))
                       str_text))
         ;; (strls (string-split str_text creturn))
         ;; (strls (map (cut string-trim <> cnewline) strls))
         (strls (string-decompose str_text "\r\n"))
         (strls (map tm-zotero-regex-transform strls))
         ;; Q: What advantage would there be to have parse-latex accept a
         ;; UTF-8, rather than Cork encoded, string?
         (str_text (string-convert
                    (string-join strls "\n")
                    "UTF-8" "Cork"))
         (t (latex->texmacs (parse-latex str_text)))
         (b (buffer-new)))
    (set-message "Zotero: str_text->texmacs..." "Zotero integration")
    (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs after let*. !!!\n")
    (buffer-set-body b t) ;; This is magical.
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    ;;
    ;; Used from inside tm-zotero.ts
    ;;
    (let ((lt (select t '(:* (:or ztHref hlink href)))))
      ;; It turns out that tm-select will return these not in tree or document 
      ;; order.  For this function, that's alright.
      ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:t ztHref hlink href before: ~s\n" t)
      ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:select lt: ~s\n" lt)
      (let loop ((lt2 lt))
        (let ((lnk (and (pair? lt2) (car lt2)))) ; lnk will be bool or tree
          (cond
            ((null? lt2) #t)
            ((or is-note? is-bib?)
             (move-link-to-own-line lnk)
             (loop (cdr lt2)))
            (else
              (loop (cdr lt2)))))))
    ;;
    ;; from propachi-texmacs/bootstrap.js monkeypatch VariableWrapper
    ;;
    (let ((lt (select t '(:* (:or ztHrefFromBibToURL ztHrefFromCiteToBib)))))
      ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:t ztHrefFromBibToURL ztHrefFromCiteToBib before: ~s\n" t)
      ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:select lt: ~s\n" lt)
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
              ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:fixup-slink-as-url lnk:~s\n" lnk)
              (fixup-embedded-slink-as-url lnk))))))
    ;;
    ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:before tree-simplify\n")
    (tree-simplify t)
    ;; (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:after tree-simplify\n")
    (zt-format-debug "tm-zotero-UTF-8-str_text->texmacs:after.\n")
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    (buffer-close b)
    (recall-message)
    t))

;;}}}

;;{{{ zfield testing predicates IsBib?, IsNote?
;;;
;;; Remember that there is a difference between the source document tree and
;;; the typeset tree, and that it is not always the case that the cursor focus
;;; is on the field when it's being tested. These two don't require the cursor
;;; focus to be there, and should not, and they work on the source document
;;; where the typesetting environment has not necessarily been formed at the
;;; point in time where these are run! That's why it can not simply use
;;; focus-is-zcite? or focus-is-zbibliography?. Those are for the cursor-focus
;;; tree while editting. These are for the zotero integration for seeing how to
;;; format the final result of translating LaTeX bbl to TeXmacs.
;;;
;;; In particular, it can not rely on zt-not-inside-note, zt-in-footnote, or
;;; zt-in-endnote, since those are part of the dynamic typesetting tree
;;; environment, not the static source document tree environment. Only the
;;; init-env, knowledge of the defaults, and the "with" surrounding can be seen
;;; by these predicates... they look at the static source tree, not the
;;; typesetter's resultant box tree, nor at the dynamic environment inside of
;;; the typeset tree during or after typesetting.
;;;
;;; Input is a field tree, already found.
;;;
(define (zfield-IsBib? zfield)
  ;; (zt-format-debug "zfield-IsBib? called... zfield label:~s\n"
  ;;                  (tree-label zfield))
  (tree-is? zfield 'zbibliography))


;;;
;;; Input is a field tree, already found.
;;;
(define (zfield-IsNote? zfield)
  ;; (zt-format-debug "zfield-IsNote? called.\n")
  ;; Inside a "with" context that has zt-option-this-zcite-in-text true?
  (and (not (tree-is? zfield 'zbibliography))
       (let* ((with-t (with-like-search (tree-ref zfield :up)))
              (in-text-opt (and with-t                             
                                (with-ref with-t
                                          "zt-option-this-zcite-in-text")))
              (forced-in-text? (and in-text-opt
                                    (== (tree->string in-text-opt) "true"))))
         (or
          (and (not forced-in-text?)
               ;; Document init-env pref is set due to a CSL "note" style: (default)
               (and (test-env? "zotero-pref-noteType0" "false") ;; Overrides
                    (or (test-env? "zotero-pref-noteType1" "true")
                        (test-env? "zotero-pref-noteType2" "true"))))
          (let* ((fn-t (tree-search-upwards (tree-ref zfield :up)
                                            '(zt-footnote footnote)))
                 (in-footnote? (not (not fn-t))))
            ;; Explicitly written inside of a user-inserted footnote?
            in-footnote?)))))

;;}}}

;;; Sets the (visible) text of a field.
;;;
;;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;;
;;; Let's assume that for this, it's always "isRich", so ignore that arg.
;;;
(define (tm-zotero-Field_setText tid documentID zfieldID str_text isRich)
  (zt-format-debug "tm-zotero-Field_setText called.\n")
  (let* ((zfield   (zt-find-zfield zfieldID)) ; zcite tree
         (text-t   (and zfield (get-zfield-Text-t zfield)))
         (is-note? (and zfield (zfield-IsNote? zfield)))
         (is-bib?  (and zfield (zfield-IsBib? zfield))))
         (tmtext
          (tm-zotero-UTF-8-str_text->texmacs str_text is-note? is-bib?)))
    (when text-t
      (tree-set! text-t tmtext)
      (set-document-zfield-orig-text-by-zfieldID! documentID zfieldID tmtext)
      (set-zfield-is-modified?-flag! zfield "false"))
    (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_getText

;;; Gets the (visible) text of a field.
;;;
;;; ["Field_getText", [documentID, fieldID]] -> str_text
;;;
(define (tm-zotero-Field_getText tid documentID zfieldID)
  (zt-format-debug "tm-zotero-Field_getText called.\n")
  (let* ((zfield (zt-find-zfield zfieldID))
         (str_text (or (and zfield
                            (tmtext-t->string (get-zfield-Text-t zfield)))
                       ""))
         (str_utf8 (string-convert str_text "Cork" "UTF-8")))
    (tm-zotero-write tid (safe-scm->json-string str_utf8))))

;;}}}
;;{{{ Field_setCode

;;; Sets the (hidden, persistent) code of a field.
;;;
;;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;;
(define (tm-zotero-Field_setCode tid documentID zfieldID str_code)
  (zt-format-debug "tm-zotero-Field_setCode called.\n")
  (let* ((zfield (zt-find-zfield zfieldID)))
    (when zfield
      (zt-set-zfield-Code-from-string zfield str_code)))
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_getCode

;;; Gets the code of a field.
;;;
;;; ["Field_getCode", [documentID, fieldID]] -> str_code
;;;
(define (tm-zotero-Field_getCode tid documentID zfieldID)
  (zt-format-debug "tm-zotero-Field_getCode called.\n")
  (let* ((zfield (zt-find-zfield zfieldID))
         (code_str (or (and field (zt-get-zfield-Code-string zfield))
                       "")))
    (tm-zotero-write tid code_str)))

;;}}}
;;{{{ Field_convert

;;; Converts a field from one type to another.
;;;
;;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;;; null
;;;
(define (tm-zotero-Field_convert tid documentID
                                 zfieldID str_fieldType int_noteType)
  (zt-format-debug "STUB:zotero-Field_convert: ~s ~s ~s ~s\n"
                   documentID zfieldID
                   str_fieldType int_noteType)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}

;;}}}

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Local Variables:
;;; fill-column: 79
;;; truncate-lines: t
;;; folded-file: t
;;; End:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
