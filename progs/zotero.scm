;;;
;;
;; MODULE      : zotero.scm
;; DESCRIPTION : Zotero Connector Plugin
;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;

(texmacs-module (zotero)
  (:use (kernel texmacs tm-modes)
        (kernel library content)
        (utils library cursor)
        (convert tools sxml)
        ;; (convert rtf rtftm)
        ))


;; Ported from Guile 2.0 to Guile 1.8 by Karl M. Hegbloom.
(use-modules (json))
(use-modules (ice-9 format))

(tm-define (zt-format-error . args)
  (:secure)
  (apply format (cons (current-error-port) ,@args)))


;; (define-public zotero-debug-trace? #f)
(define-public zotero-debug-trace? #t)

(tm-define (zt-format-debug . args)
  (:secure)
  (when zotero-debug-trace?
    (apply format (cons (current-output-port) args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions
;;

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


;; The documentID; this is not stable from one run to the next since it's value
;; depends on whether this is the first document buffer upon launching TeXmacs
;; or some subsequently loaded one. It does not have to be stable from one run
;; to the next, but unique for each document being processed during this run.
;;
;; An alternative would be to assign it a stable value and store that in the
;; init-env. It could be a UUID or a string formed just like the fieldID's are.
;;
;; I'm pretty sure that the LibreOffice document ID's are just sequential
;; numbers counting from 0 each time the editor is launched. They identify it's
;; internal document state management objects, which may actually be
;; transcient, existing only during the duration of an "integration command"
;; sequence.
;;
(tm-define (zotero-getDocId)
  (as-string (car (buffer-path))))

;; There can be only 1 new field pending per document. For now I assume that we
;; are editting only 1 document per instance of TeXmacs. If that changes, then
;; this can become a hash table, or part of a per-document state management
;; object.
;;
(define-public zotero-new-fieldID #f)

(tm-define (zotero-get-new-fieldID)
  (as-string (create-unique-id)))

(tm-define (zotero-ref-binding-key fieldID)
  (string-append "zotero" fieldID "-noteIndex"))

;; The set-binding call happens inside of the macro that renders the
;; citation. I spent half a day figuring out how to write a glue-exported
;; accessor function... then discovered this trick:
;;
(tm-define (get-reference-binding key)
  (texmacs-exec `(get-binding ,key)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Style sheet helper functions and tag accessors
;;
(define zt-cite-tags '(zcite zbibliography))


(tm-define (zt-flag-if-modified fieldID)
  (:secure)
  (let* ((fieldID (as-string fieldID))
         (field (zotero-find-zcite fieldID))
         (text (format #f "~s" (tree->stree (zotero-zcite-fieldText field))))
         (orig-fieldText (zotero-get-orig-fieldText fieldID)))
    (if (and orig-fieldText
             (not (string=? text orig-fieldText)))
        (begin
          (zt-format-debug "Debug: zt-flag-if-modified: Field is modified: ~s\n" fieldID)
          '(concat (flag "Modified!" "red")))
        (begin
          (zt-format-debug "Debug: zt-flag-if-modified: Field is NOT modified: ~s\n" fieldID)
          '(concat (flag "Not Modified." "green"))))))


(tm-define (zotero-insert-new-field tag)
  (:secure)
  (if (not (in-zfield?))
      (let ((id (zotero-get-new-fieldID)))
        (set! zotero-new-fieldID id)
        (insert `(,tag ,id (raw-data "") "{Citation}"))
        ;; Perhaps add it to the cache here
        )
      (begin
        (zt-format-error "ERR: zotero-insert-new-field ~s : focus-tree is a ~s\n"
                         tag (tree-label (focus-tree)))
        #f)))



;;; This young code says that it wants to be a GOOPS object someday. I'm not
;;; sure if that's right for it yet.

;; Operations on zcite fields.
;;
;;

;;; tm-find returns an incorrect result! Use tm-search.
;;;
(tm-define (zotero-find-zcite fieldID)
  (car
   (tm-search
    (buffer-tree)
    (lambda (t)
      (and (tree-in? t zt-cite-tags)
           (string=? fieldID
                     (as-string (zotero-zcite-fieldID t))))))))


(tm-define (zotero-go-to-zcite documentID fieldID)
  ;; Ignore documentID for now, assuming work with focused document only.
  ;;
  ;; for now just tree-go-to, but perhaps look up the position from the cache
  ;; and jump to it without a tree-search having to happen first. (think "large
  ;; documents")
  (tree-go-to (zotero-find-zcite fieldID) 1))


;; These must match the definitions in tm-zotero.ts;
;;
;;  L     0         1           2
;; (zcite "fieldID" "fieldCode" "fieldText")
;;
;; fieldNoteIndex is gotten via a reference binding.
;;
(tm-define (zotero-zcite-fieldID t)
  (tree-ref t 0))

;; I decided I like them better without the raw-data wrapper.
;; (tm-define (zotero-zcite-fieldCode t)
;;   ;; upgrade old tags, also fixup hand-entered ones?
;;   (let ((code (tree-ref t 1)))
;;     (cond
;;       ((tm-func? code 'raw-data)
;;        (tree-ref code 0))
;;       ((and (not (tm-func? code 'raw-data))
;;             (tm-atomic? code))
;;        (tree-set! code (stree->tree `(raw-data ,(tree->stree code))))
;;        (tree-ref code 0))
;;       ((not (tm-func? code 'raw-data))
;;        (tree-set! code (stree->tree '(raw-data "")))
;;        (tree-ref code 0)))))

(tm-define (zotero-zcite-fieldCode t)
  (tree-ref t 1))
  ;; ;; upgrade old tags, also fixup hand-entered ones?
  ;; (let ((code (tree-ref t 1)))
  ;;   (cond
  ;;     ((tm-func? code 'raw-data)
  ;;      (tree-set! code (tree-ref code 0))
  ;;      code)
  ;;     ;; ((and (not (tm-func? code 'raw-data))
  ;;     ;;       (tm-atomic? code))
  ;;     ;;  (tree-set! code (stree->tree `(raw-data ,(tree->stree code))))
  ;;     ;;  (tree-ref code 0))
  ;;     ((not (tm-func? code 'raw-data))
  ;;      code))))

;; For "note" styles, this reference binding links a citation field with
;; the footnote number that it appears in.
;;
(tm-define (zotero-zcite-fieldNoteIndex field)
  (get-reference-binding
   (zotero-ref-binding-key
    (as-string (zotero-zcite-fieldID field)))))


;; This next field is set automatically, below, with the result of converting
;; the rich-text that Zotero sends back into a TeXmacs tree.
;; 
;;
;; Todo: But what if I use drd-props to make it accessible so I can edit it,
;;       and then do edit it? OpenOffice lets you edit them, but prompts you
;;       that it's been editted before replacing it.
;;
;; Idea: When it's editted, perhaps a diff could be kept? Or some kind of
;;       mechanism that finds out what is changed and sends it to Zotero?
;;
;;    A: I think that's not easy to do and more trouble than it's worth.
;;       It's easier to just curate your reference collection to make it
;;       produce what you want, right?
;;
(tm-define (zotero-zcite-fieldText t)
  (tree-ref t 2))



(tm-define (zotero-get-zcite-fields-list documentID fieldType)
  ;; Maybe ensure active document is documentID? For now assume it is.
  ;; Also for now assume fieldType is always "ReferenceMark", so ignore it.
  ;; Q: What about "save-excursion" or "save-buffer-excursion"?
  ;; A: This is searching the document tree, not moving the cursor.
  (let ((all-fields (tm-search
                     (buffer-tree)
                     (lambda (t)
                       (and (tree-in? t zt-cite-tags)
                            (not
                             (and zotero-new-fieldID
                                  (string=? (as-string
                                             (zotero-zcite-fieldID t))
                                            zotero-new-fieldID))))))))
    all-fields))



;; The fieldCode is a JSON string. Among other things, it is how Zotero keeps
;; track of whether or not the user has editted the automatically formatted
;; citation text in the fieldText. When it has been editted, Zotero prompts
;; first before overwriting it. By parsing that JSON and accessing that
;; information ourself, we can render a red flag when it has been modified, to
;; create a visual signal to the user. In order to make that happen, all
;; setting and getting of the fieldCode must happen via these functions.
;;
(define zotero-fieldCode-cache (make-ahash-table))

(tm-define (zotero-get-fieldCode-string field)
  (let ((id (as-string (zotero-zcite-fieldID field)))
        (str_code (as-string (zotero-zcite-fieldCode field))))
    ;; So that Document_getFields causes this to happen.
    (when (and (not (ahash-ref zotero-fieldCode-cache id #f))
               (not (and zotero-new-fieldID
                         (string=? zotero-new-fieldID id))))
      (zotero-parse-and-cache-fieldCode field str_code))
    str_code))

;; Must handle empty string for zotero-Field_delete. Since it does not
;; actually delete the tag from the document, it does not need to delete it
;; from the cache.
;;
;; Also handle empty string for zotero-Field_removeCode.
;;
;; Also: What happens when I manually delete a zcite tag? How do I maintain the
;; fieldCode and field positions cache?
;;
;; It is unlikely that a zcite will be manually deleted during the course of an
;; integration command / editor-integration command sequence... Zotero is
;; already designed to handle the case where you've manually removed a citation
;; field... So these routines simply need to check that the field is really
;; there before returning any cached information...
;;
;; What if I cut and paste a zcite from one location to another, and so the
;; cached document position is no longer valid, but the zcite really is still
;; in the document? For that case, I must fall back on a search of the document
;; tree for a field with the sought-for fieldID, then update the cache.
;;
(tm-define (zotero-set-fieldCode-from-string field str_code)
  (let ((code (zotero-zcite-fieldCode field)))
    (zotero-parse-and-cache-fieldCode field str_code)
    (tree-set! code str_code)))

;; It goes through here so that this can also be called from the
;; Document_getFields, in order to 
(tm-define (zotero-parse-and-cache-fieldCode field str_code)
  (let* ((id (as-string (zotero-zcite-fieldID field)))
         (code (zotero-zcite-fieldCode field))
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
          (display* (car scm-code) "\n") ;; debug error trace
          (noop)) ;; silent error?
        (when scm-code
          (hash-set! zotero-fieldCode-cache id scm-code)))))


(tm-define (zotero-get-orig-fieldText field_or_id)
  (let* ((id (or (and (string? field_or_id)
                      field_or_id)
                 (as-string
                  (zotero-zcite-fieldID field_or_id))))
         (scm-code (hash-ref zotero-fieldCode-cache id #f))
         (props (and scm-code
                     (hash-ref scm-code "properties" #f)))
         (plainCitation (and props
                             (hash-ref props "plainCitation" #f))))
    plainCitation))



;; DocumentData
;;
;; AFAIK the only pref that this program needs access to is noteType, and that
;; access is read-only. The noteType is a document-wide setting, since it goes
;; with the CSL stylesheet chosen. But it is also passed to
;; Document_insertField, Document_convert (?), and Field_convert, so really
;; it could be a per-field setting. I choose to make it document-wide.
;;
;; enum noteType
;;
(define-public zotero-NOTE_IN_TEXT  0)
(define-public zotero-NOTE_FOOTNOTE 1)
(define-public zotero-NOTE_ENDNOTE  2)

;;
;; The rest of the DocumentData settings are "opaque" from the viewpoint of
;; this interface. They control Zotero, not TeXmacs.
;;
;; All of them are set via the zotero controlled dialog. That dialog is
;; displayed automatically when the document does not yet have
;; zoteroDocumentData set, because at the start of the transaction, Zotero will
;; call zotero-Document_getDocumentData, which returns null to Zotero unless
;; it's been set. After setting it, the next thing Zotero sends is a
;; zotero-Document_setDocumentData message. It can also be invoked by sending a
;; zotero-setDocPrefs message, which will call zotero-Document_getDocumentData,
;; then let you edit that in Zotero's dialog, and send it back with
;; zotero-Document_setDocumentData. So from here, we never need to write the
;; prefs by any means other than having Zotero set it.
;;
;; Perhaps a future iteration could provide initial hints based on the language
;; of the document being editted? But that's sort of a global thing anyway, and
;; setting the language takes only a few clicks.
;;
;; Access it from Guile with: (get-env "zotero-pref-noteType")
;; Access it from TeXmacs with: <value|zotero-pref-noteType>


;; Here's what the typical DocumentData looks like, parsed to sxml:
;;
;; (define zotero-sample-DocumentData-sxml
;;   '(*TOP*
;;     (data (@ (data-version "3") (zotero-version "4.0.29.9m75"))
;;      (session (@ (id "gk3doRA9")))
;;      (style (@ (id "http://juris-m.github.io/styles/jm-indigobook-in-text")
;;                (locale "en-US")
;;                (hasBibliography "1")
;;                (bibliographyStyleHasBeenSet "0")))
;;      (prefs
;;       (pref (@ (name "citationTransliteration")       (value "en")))
;;       (pref (@ (name "citationTranslation")           (value "en")))
;;       (pref (@ (name "citationSort")                  (value "en")))
;;       (pref (@ (name "citationLangPrefsPersons")      (value "orig")))
;;       (pref (@ (name "citationLangPrefsInstitutions") (value "orig")))
;;       (pref (@ (name "citationLangPrefsTitles")       (value "orig")))
;;       (pref (@ (name "citationLangPrefsJournals")     (value "orig")))
;;       (pref (@ (name "citationLangPrefsPublishers")   (value "orig")))
;;       (pref (@ (name "citationLangPrefsPlaces")       (value "orig")))
;;       (pref (@ (name "citationAffixes")
;;                  (value "|||||||||||||||||||||||||||||||||||||||||||||||")))
;;       (pref (@ (name "projectName")
;;                  (value "Project:TeXmacsTesting")))
;;       (pref (@ (name "extractingLibraryID")           (value "0")))
;;       (pref (@ (name "extractingLibraryName")
;;                  (value "No group selected")))
;;       (pref (@ (name "fieldType")                   (value "ReferenceMark")))
;;       (pref (@ (name "storeReferences")               (value "true")))
;;       (pref (@ (name "automaticJournalAbbreviations") (value "true")))
;;       (pref (@ (name "noteType")                      (value "0")))
;;       (pref (@ (name "suppressTrailingPunctuation")   (value "true")))))))


;; For now ignore documentID; assume it's always the active document anyway.  I
;; think it's really just meant for a key to a table of document objects for
;; keeping local state. For this application that state is in the actual
;; document itself. Depending upon the way the documentID is formed, it could
;; be used to obtain the buffer file name, document title, etc.
;;
(define (zotero-get-DocumentData documentID)
  (get-env "zoteroDocumentData"))

(define (zotero-set-DocumentData documentID str_dataString)
  (init-env "zoteroDocumentData" str_dataString)
  (zotero-init-env-zotero-prefs documentID str_dataString))


(define (zotero-init-env-zotero-prefs documentID str_dataString)
  ;; (write (parse-xml (zotero-get-DocumentData)))
  ;; (newline)
  (let ((zotero-init-env-zotero-prefs-sub
         (lambda (prefix attr-list)
           (let loop ((attr-list attr-list))
                (cond
                  ((null? attr-list) #t)
                  (#t (init-env (string-append prefix (symbol->string
                                                       (caar attr-list)))
                                (cadar attr-list))
                   (loop (cdr attr-list))))))))
    (let loop ((sxml (cdr (parse-xml str_dataString))))
         (cond
           ((null? sxml) #t)
           ((eq? 'data (sxml-name (car sxml)))
            (zotero-init-env-zotero-prefs-sub "zotero-data-" (sxml-attr-list
                                                              (car sxml)))
            (loop (sxml-content (car sxml))))
           ((eq? 'session (sxml-name (car sxml)))
            (zotero-init-env-zotero-prefs-sub "zotero-session-" (sxml-attr-list
                                                                 (car sxml)))
            (loop (cdr sxml)))
           ((eq? 'style (sxml-name (car sxml)))
            (zotero-init-env-zotero-prefs-sub "zotero-style-" (sxml-attr-list
                                                               (car sxml)))
            (loop (cdr sxml)))
           ((eq? 'prefs (sxml-name (car sxml)))
            (loop (sxml-content (car sxml))))
           ((eq? 'pref (sxml-name (car sxml)))
            (init-env (string-append "zotero-pref-" (sxml-attr (car sxml) 'name))
                      (sxml-attr (car sxml) 'value))
            (when (string=? "noteType" (sxml-attr (car sxml) 'name))
              ;; The TeXmacs style language case statements can not test an
              ;; environment variable that is a string against any other
              ;; string... the string it's set to has to be "true" or "false"
              ;; to make boolean tests work. It can not check for "equals 0",
              ;; "equals 1", etc.
              (init-env "zotero-pref-noteType0" "false")
              (init-env "zotero-pref-noteType1" "false")
              (init-env "zotero-pref-noteType2" "false")
              (init-env (string-append "zotero-pref-noteType"
                                       (sxml-attr (car sxml) 'value)) "true")) 
            (loop (cdr sxml)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Protocol between tm_zotero and ZoteroTeXmacsIntegration.js
;;
;; https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol
;;
;; The Firefox or Zotero Standalone process operates a server on port 23116,
;; which the extension residing within TeXmacs connects to. All frames consist
;; of a 32 bits specifying the transaction ID, a big-endian 32-bit integer
;; specifying the length of the payload, and the payload itself, which is
;; either UTF-8 encoded JSON or an unescaped string beginning with “ERR:”.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(define zotero-socket-port #f)
(define zotero-socket-inet-texmacs-port-number 23117)
(define zotero-socket-inet-zotero-port-number 23116)

(define (set-nonblocking sock)
  (fcntl sock F_SETFL (logior O_NONBLOCK
                              (fcntl sock F_GETFL))))

(define (set-blocking sock)
  (fcntl sock F_SETFL (logand (lognot O_NONBLOCK)
                              (fcntl sock F_GETFL))))

;; From /usr/include/linux/tcp.h
(define TCP_NODELAY 1)

(define (get-zotero-socket-port!)
  (catch 'system-error
    (lambda ()
      (if (and (port? zotero-socket-port)
               (not (port-closed? zotero-socket-port)))
          zotero-socket-port
          (begin
            (set! zotero-socket-port (socket PF_INET SOCK_STREAM 0))
            (setsockopt zotero-socket-port SOL_SOCKET SO_REUSEADDR 1)
            (bind    zotero-socket-port AF_INET INADDR_LOOPBACK 
                     zotero-socket-inet-texmacs-port-number)
            (connect zotero-socket-port AF_INET INADDR_LOOPBACK
                     zotero-socket-inet-zotero-port-number)
            (setvbuf zotero-socket-port _IOFBF)
            (setsockopt zotero-socket-port IPPROTO_TCP TCP_NODELAY 1)
            (set-blocking zotero-socket-port)
            zotero-socket-port)))
    (lambda args
      (zt-format-error "ERR: Exception caught in get-zotero-socket-port!: ~s\n" args)
      (close-port zotero-socket-port)
      (set! zotero-socket-port #f)
      (set! zotero-active? #f)
      (dialogue-window
       (zotero-display-alert
        (zotero-getDocId)
        (string-append "\\begin{center}\n"
                       "Exception caught in: "
                       "\\texttt{get-zotero-socket-port!}\n\n"
                       "\\textbf{System Error:} " (caar (cdddr args)) "\n\n"
                       "Is Zotero running?\n\n"
                       "If so, then you may need to {\\em restart} Firefox\\\\\n"
                       "or Zotero Standalone.\n"
                       "\\end{center}\n")
        DIALOG_ICON_STOP
        DIALOG_BUTTONS_OK)
       (lambda (val)
         (noop))
       "System Error in get-zotero-socket-port!")
      #f)))


(define (close-zotero-socket-port!)
  (if (and (port? zotero-socket-port)
           (not (port-closed? zotero-socket-port)))
      (begin
        (close-port zotero-socket-port)
        (set! zotero-socket-port #f))))


(sigaction SIGPIPE (lambda (sig)
                     (set! zotero-active? #f)
                     (close-zotero-socket-port!)))


(define (write-network-u32 value port)
  (let ((v (make-u32vector 1 0)))
    (u32vector-set! v 0 (htonl value))
    (uniform-vector-write v port)))

(define (read-network-u32 port)
  (let ((v (make-u32vector 1 0)))
    (uniform-vector-read! v port)
    (ntohl (u32vector-ref v 0))))


(define (zotero-write tid cmd)
  (zt-format-debug "Debug: zotero-write: ~s ~s\n" tid cmd)
  (let ((zp (get-zotero-socket-port!)))
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
        (close-zotero-socket-port!)
        (set! zotero-active #f)
        (dialogue-window
         (zotero-display-alert 
          (zotero-getDocId)
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
  (let ((zp (get-zotero-socket-port!)))
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
      (when (char-ready? zotero-socket-port)
        (with (tid len cmdstr) (zotero-read)
          (zt-format-debug "Debug: tid:~s len:~s cmdstr:~s\n" tid len cmdstr)
          (if (> len 0)
              (with (editCommand args) (safe-json-string->scm cmdstr)
                (zt-format-debug "Debug: ~s\n" (list editCommand (cons tid args)))
                (cond
                  ((and (>= (string-length editCommand) 4)
                        (string=? (string-take editCommand 4) "ERR:"))
                   (zotero-write tid editCommand) ;; send the error to Zotero
                   (set! counter 40)
                   (set! wait 10)) ;; keep listening
                  ((string=? editCommand "Document_complete")
                   (zotero-write tid (scm->json-string '()))
                   ;;(close-zotero-socket-port!)
                   (set! wait 0)
                   (set! zotero-active? #f))
                  (#t
                   (apply (eval ;; to get the function itself
                           (string->symbol 
                            (string-append "zotero-"
                                           editCommand)))
                          (cons tid args))
                   (set! counter 40)
                   (set! wait 10))))
              (begin
                (set! counter (- counter 1))       ;; Sometimes when Firefox is
                (when (<= counter 0)               ;; stopped in the middle of
                  (close-zotero-socket-port!)      ;; it, char-ready? returns #t
                  (set! wait 0)                    ;; but zotero-read does not
                  (set! zotero-active? #f))))))))) ;; read anything.


;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;; Editor commands.
;;
;; See: zotero-menu.scm
;; See: zotero-kbd.scm
;;
(define (zotero-call-integration-command cmd)
  (when (not zotero-active?) ;; one at a time only
    (let ((zp (get-zotero-socket-port!)))
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



;; ---------

(tm-define (zotero-addCitation)
  (unless zotero-new-fieldID ;; one at a time only
    (try-modification
      (if (and (zotero-insert-new-field 'zcite)
               (zotero-call-integration-command "addCitation"))
          #t
          (begin
            (set! zotero-new-fieldID #f)
            #f)))))
                

(tm-define (zotero-editCitation)
  (zotero-call-integration-command "editCitation"))

;; ---------

(tm-define (zotero-addBibliography)
  (unless zotero-new-fieldID ;; one at a time only
    (try-modification
      ;; It is what Zotero expects. I'd expect to put {Bibliography} there.
      (if (and (zotero-insert-new-field 'zbibliography)
               (zotero-call-integration-command "addBibliography"))
          #t
          (begin
            (set! zotero-new-fieldID #f)
            #f)))))


(tm-define (zotero-editBibliography)
  (zotero-call-integration-command "editBibliography"))


;; ---------


(tm-define (zotero-refresh)
  (zotero-call-integration-command "refresh"))


;; (tm-define (zotero-removeCodes)
;;   (zotero-call-integration-command "removeCodes"))


;; ---------


(tm-define (zotero-setDocPrefs)
  (zotero-call-integration-command "setDocPrefs"))




;; (tm-define (notify-activated t)
;;   (:require (and (style-has? "tm-zotero-dtd")
;;                  (inside-which zt-cite-tags)))
;;   ;; do something when activating a tag that's just like
;;   ;; using the menus or toolbar.
;;   (noop))
                 
;; (tm-define (notify-disactivated t)
;;   (:require (and (style-has? "tm-zotero-dtd")
;;                  (inside-which zt-cite-tags)))
;;   ;; do something when activating a tag that's just like
;;   ;; using the menus or toolbar.
;;   (noop))



;;; Preferences and Settings
;;;
;;; Todo:
;;;
;;; Setting: Zotero zcite and bibliograph by default in all new documents?
;;;


;; (tm-define (focus-parameter-menu-item l)
;;   (:require (and (tree-label-parameter? (string->symbol l))
;;                  )))

;; (tm-define (customizable-parameters t)
;;   (:require (tree-in? t '(zcite)))
;;   (list (list "zt-pref-this-cite-in-text?" "This zcite in-text?")))

;; (tm-define (parameter-choice-list var)
;;   (:require (and (tree-in? t '(zcite))
;;                  (in? var '("zt-pref-this-cite-in-text?"
;;                             "zt-pref-hlinks-as-footnotes?"
;;                             "zt-pref-hlinks-as-tt?"
;;                             "zt-pref-hlinks-as-smaller?"))))
;;   (list "true" "false"))


;;; Todo: See  update-document  at generic/document-edit.scm:341
;;
;; Maybe this should only happen from the Zotero menu?
;;
(tm-define (update-document what)
  (:require (in-tm-zotero-style?))
  (delayed
    (:idle 1)
    (cursor-after
     (when (or (== what "all")
               (== what "bibliography"))
       (zotero-refresh))
     (unless (== what "bibliography")
       (former what)))))


;;
;; Word Processor commands: Zotero -> TeXmacs
;;
;; Each sends: [CommandName, [Parameters,...]].
;;
;; The response is expected to be a JSON encoded payload, or the unquoted and
;; unescaped string: ERR: Error string goes here
;;
;; Gets information about the client and the currently active
;; document. documentID can be an integer or a string.
;;
;; ["Application_getActiveDocument", [int_protocolVersion]] -> [int_protocolVersion, documentID]
;;
(tm-define (zotero-Application_getActiveDocument tid pv)
  (zotero-write tid (safe-scm->json-string (list pv (zotero-getDocId)))))



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


;; Shows an alert.
;;
;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;
(tm-define (zotero-Document_displayAlert tid documentID str_dialogText int_icon int_buttons)
  (dialogue-window (zotero-display-alert documentID str_dialogText int_icon int_buttons)
                   (lambda (val)
                     (zotero-write tid (safe-scm->json-string val)))
                   "Zotero Alert!"))


;; Brings the document to the foreground. (For OpenOffice, this is a no-op on non-Mac systems.)
;;
;; ["Document_activate", [documentID]] -> null
;;
(tm-define (zotero-Document_activate tid documentID)
  (zotero-write tid (safe-scm->json-string '())))


;; Indicates whether a field can be inserted at the current cursor position.
;;
;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;
(tm-define (zotero-Document_canInsertField tid documentID str_fieldType)
  ;; Todo: This is probably not working quite right.
  (let ((ret (not
              (not
               (and (in-text?)
                    (not (in-math?))
                    (let ((t (inside-which zt-cite-tags)))
                      (zt-format-debug "Debug: zotero-Document_canInsertField (inside-which zt-cite-tags) => ~s\n" t)
                      (or (not t)
                          (and zotero-new-fieldID
                               (string=? zotero-new-fieldID
                                         (zotero-zcite-fieldID t))))))))))
    (zotero-write tid (safe-scm->json-string ret))))



;; Retrieves data string set by setDocumentData.
;;
;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;
(tm-define (zotero-Document_getDocumentData tid documentID)
  (zotero-write tid (safe-scm->json-string (zotero-get-DocumentData documentID))))


;; Stores a document-specific persistent data string. This data
;; contains the style ID and other user preferences.
;;
;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;
(tm-define (zotero-Document_setDocumentData tid documentID str_dataString)
  (zotero-set-DocumentData documentID str_dataString)
  (zotero-write tid (safe-scm->json-string '())))



;; Indicates whether the cursor is in a given field. If it is, returns
;; information about that field. Returns null, indicating that the cursor isn't
;; in a field of this fieldType, or a 3 element array containing:
;;
;; fieldID, int or string, A unique identifier corresponding to this field.
;;
;; fieldCode, UTF-8 string, The code stored within this field.
;;
;; noteIndex, int, The number of the footnote in which this field resides, or 0
;;                 if the field is not in a footnote.
;;
;; ["Document_cursorInField", [documentID, str_fieldType]] -> null || [fieldID, fieldCode, int_noteIndex]
;;
(tm-define (zotero-Document_cursorInField tid documentID str_fieldType)
  (let ((ret
         (if (tree-in? (cursor-tree) zt-cite-tags)
             ;; Q: Do I need to canonicalize the cursor location by moving it?
             (let* ((t (cursor-tree))
                    (id (as-string (zotero-zcite-fieldID t))))
               (if (not (and zotero-new-fieldID
                             (string=? zotero-new-fieldID id)))
                   (begin
                     (list id
                           (zotero-get-fieldCode-string t)
                           (as-string (zotero-zcite-fieldNoteIndex t))))
                   '())) ;; is the new field not finalized by Document_insertField
             '()))) ;; not tree-in? zt-cite-tags
    (zotero-write tid (safe-scm->json-string ret))))



;; Inserts a new field at the current cursor position. Because there has to be
;; time for the typesetting to run in order for it to create the footnote
;; number and set the reference bindings for the noteIndex, by the time this
;; routine is being called by Zotero, TeXmacs must have already inserted the
;; new field, but in a pending state, finalized by this.
;;
;; str_fieldType, either "ReferenceMark" or "Bookmark"
;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;
;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]
;;
;; Ignore: str_fieldType
;;
(tm-define (zotero-Document_insertField tid documentID str_fieldType int_noteType)
  (let ((field (zotero-find-zcite zotero-new-fieldID))
        (id zotero-new-fieldID))
    (set! zotero-new-fieldID #f)
    ;; (tree-go-to field 1)
    ;; Q: Is it useful to initialize the fieldCode to anything here?
    (zotero-write tid (safe-scm->json-string
                       (list id "" (as-string (zotero-zcite-fieldNoteIndex field)))))))

  

;; Get all fields present in the document, in document order.
;;
;; str_fieldType is the type of field used by the document, either ReferenceMark or Bookmark
;;
;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]
;;
;;
;; Todo: Perhaps use a hash table to memoize buffer positions for each field so
;;       that after this, access is more like O(1) rather than O(n), assuming
;;       hash lookup is faster than short list traversal with string
;;       compare... but this is more than list traversal; it's buffer-tree
;;       traversal; that's not the slow part though; typing is slow when the
;;       document is complicated because of the O(n^2) box-tree to
;;       document-tree ip (inverse path) search algorithm. Finding these fields
;;       in the source document is really just straightforward recursive DAG
;;       traversal, right?
;;
;; Lets get it working first, then option setting features next, then see if it
;; needs this.
;;
;; A protocol trace watching the traffic between Libreoffice and Zotero shows
;; that the BIBL field is also sent as one of the fields in this list.
;;
(tm-define (zotero-Document_getFields tid documentID str_fieldType)
  (let ((ret
         (let loop ((zcite-fields (zotero-get-zcite-fields-list
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
                         (cons (as-string (zotero-zcite-fieldID field)) ids)
                         (cons (zotero-get-fieldCode-string field) codes)
                         (cons (as-string (zotero-zcite-fieldNoteIndex
                                                  field)) indx))))))))
    (zotero-write tid (safe-scm->json-string ret))))



;; ["Document_convert" ??? (TODO in documentation.)
;;
;; public void convert(ReferenceMark mark, String fieldType, int noteType)
;;
;; I think this is for OpenOffice to convert a document from using ReferenceMark fields to Bookmark ones.
;; Maybe we could repurpose this for TeXmacs?  Better to make a new flag; and just ignore this one.
;;
(tm-define (zotero-Document_convert tid . args)
  (zotero-write tid (safe-scm->json-string '())))



;; ["Document_setBibliographyStyle", ??? (TODO)
;;
;; public void setBibliographyStyle(int firstLineIndent, int bodyIndent, int lineSpacing,
;;    		int entrySpacing, ArrayList<Number> arrayList, int tabStopCount)
;;
;; ["Document_setBibliographyStyle", [2,0,0,240,240,[],0]]
;;
;; The first argument is documentID. After that, they match up to the above
;; Java method signature.
;;
;; 
(tm-define (zotero-Document_setBibliographyStyle tid documentID
            firstLineIndent bodyIndent
            lineSpacing entrySpacing
            arrayList tabStopCount)
  ;;
  ;; static final double MM_PER_100_TWIP = 25.4/1440*100;
  ;;
  ;;    1 twip = 1/20 * postscript point
  ;;
  ;; 4:  5.01875e-6 texpt / twip
  ;; 3:  199252.801993 twip / texpt
  ;; 2:  566929.13386 twip / mm
  ;; 1:  1.76388888888e-6 mm / twip
  ;;
  ;; // first line indent
  ;; styleProps.setPropertyValue("ParaFirstLineIndent", (int) (firstLineIndent*MM_PER_100_TWIP));
  ;; // indent
  ;; styleProps.setPropertyValue("ParaLeftMargin", (int) (bodyIndent*MM_PER_100_TWIP));
  ;; // line spacing
  ;; LineSpacing lineSpacingStruct = new LineSpacing();
  ;; lineSpacingStruct.Mode = LineSpacingMode.MINIMUM;
  ;; lineSpacingStruct.Height = (short) (lineSpacing*MM_PER_100_TWIP);
  ;; styleProps.setPropertyValue("ParaLineSpacing", lineSpacingStruct);
  ;; // entry spacing
  ;; styleProps.setPropertyValue("ParaBottomMargin", (int) (entrySpacing*MM_PER_100_TWIP));
  ;;
  ;; I don't like this use of non-font-size-relative measurements. Let's just
  ;; always leave them at the document defaults for the time being and let this
  ;; function be a noop.
  ;;
  (zotero-write tid (safe-scm->json-string '())))



;; Not documented, but exists in CommMessage.java in LibreOffice side of the
;; connector. It appears to do nothing there either.
;;
(tm-define (zotero-Document_cleanup tid documentID)
  (zotero-write tid (safe-scm->json-string '())))
  


;; Indicates that the given documentID will no longer be used and
;; associated resources may be freed.
;;
;; ["Document_complete", [documentID]] -> null
;;
;; See: zotero-listen, where this is checked for inline... but also enable it here since I might need to use it during
;; development, at least. It's never called at all by zotero-listen, so can just be commented off here.
;;
;; (tm-define (zotero-Document_complete tid documentID)
;;   (zotero-write tid (safe-scm->json-string '()) )
;;   (set! zotero-active? #f)
;;   ;; (close-zotero-socket-port!)
;;   )



;; Deletes a field from the document (both its code and its contents).
;;
;; Note: I thought this should just remove the entire tag but IIRC it did not
;; work right. I think that Zotero expected to just clear the fields, leaving
;; the tag there for reuse? Need to look into this.
;;
;; fieldID as originally returned by Document_cursorInField,
;; Document_insertField, or Document_getFields.
;;
;; ["Field_delete", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_delete tid documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID))
         (code (zotero-zcite-fieldCode field))
         (text (zotero-zcite-fieldText field)))
    (tree-assign! code "")
    (zotero-set-fieldCode-from-string field "") ;; does not remove from document!
    (tree-assign! text ""))
  (display* "zotero-Field_delete " tid documentID fieldID "\n") ;; debug trace
  (zotero-write tid (safe-scm->json-string '())))



;; Moves the current cursor position to encompass a field.
;;
;; ["Field_select", [documentID, fieldID]] -> null
;;
;; Whether or not this works as expected depends on settings made by the
;; drd-props macro. I think that I want the cursor to be inside of it's light
;; blue box, after it.... (writing this comment prior to testing. FLW.)
;;
(tm-define (zotero-Field_select tid documentID fieldID)
  (zotero-got-to-zcite documentID fieldID)
  (zotero-write tid (safe-scm->json-string '())))



;;
;; ["Field_removeCode", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_removeCode tid documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID))
         (code (zotero-zcite-fieldCode field)))
    (tree-assign! code ""))
  (zotero-write tid (safe-scm->json-string '())))



;; This could also do some processing of either the text prior to parsing and
;; conversion to a tree, or of the tree after that phase.
;;
;; Todo: Here is where to implement client-side munging of the fieldText prior
;; to setting that argument of the zcite tag. Ideas include:
;;
;;  * For styles that include an href or hlink, ensure proper formatting when
;;    displayed as an in-text or as a note style citation. That means that the
;;    hlink should become an href where the label is the URL, and that it must
;;    be placed on it's own line with a spring on the end of the line above it
;;    so that the remainder of the citation is filled properly and not
;;    displayed with inch-wide spaces between words.
;;
;;  * Turn in-text hlinks into hlinks with footnote hrefs.
;;
;;  * Turn hlinks that display the URL in the textual part into hrefs instead,
;;    also moved to a footnote, unless already in a footnote.
;;
;; The result of this must be a concat, or in-line-content
;; 
(tm-define (zotero-field-str_text-to-texmacs str_text)
  (let ((t (latex->texmacs (parse-latex str_text))))
    ;; (zt-format-debug "Debug:zotero-field-str_text-to-texmacs:str_text:~s\n"
    ;;               str_text)
    ;; (zt-format-debug "Debug:zotero-field-str_text-to-texmacs:t:~s\n" t)
    ;;
    t))


;; (tm-define (zotero-field-str_text-to-texmacs str_text)
;;   (rtf-snippet->texmacs str_text))



;; Sets the (visible) text of a field.
;;
;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;
;; Let's assume that for this, it's always "isRich", so ignore that arg.
;;
(tm-define (zotero-Field_setText tid documentID fieldID str_text isRich)
  (let* ((field   (zotero-find-zcite fieldID)) ;; zcite tree
         (text    (zotero-zcite-fieldText field)) ;; string
         (tmtext  (zotero-field-str_text-to-texmacs str_text))) ;; concat tree
    (tree-assign! text tmtext))
  (zotero-write tid (safe-scm->json-string '())))



;; Gets the (visible) text of a field.
;;
;; ["Field_getText", [documentID, fieldID]] -> str_text
;;
(tm-define (zotero-Field_getText tid documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID)))
    (zotero-write tid
                  (safe-scm->json-string
                   (format #f "~s" (tree->stree
                                    (zotero-zcite-fieldText field)))))))



;; Sets the (hidden, persistent) code of a field.
;;
;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;
(tm-define (zotero-Field_setCode tid documentID fieldID str_code)
  (let* ((field (zotero-find-zcite fieldID)))
    (zotero-set-fieldCode-from-string field str_code))
  (zotero-write tid (safe-scm->json-string '())))



;; Gets the code of a field.
;;
;; ["Field_getCode", [documentID, fieldID]] -> str_code
;;
(tm-define (zotero-Field_getCode tid documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID)))
    (zotero-write tid
                  (safe-scm->json-string
                   (as-string
                    (zotero-get-fieldCode field))))))



;; Converts a field from one type to another.
;;
;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;; null
;;
(tm-define (zotero-Field_convert tid documentID
             fieldID str_fieldType int_noteType)
  (zotero-write tid (safe-scm->json-string '())))



;;; Local Variables:
;;; fill-column: 120
;;; End:
