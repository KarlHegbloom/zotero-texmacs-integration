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

;;; I can not simply use the normal TeXmacs client mechanism since it is set up
;;; for a different protocol, where the size is printed as an ascii string
;;; representation, followed by a newline, then a string representation of a
;;; scheme expression, (id (command args)). The Zotero connector protocol is
;;; different. There's a 32 bit transaction ID, then a 32 bit size in network
;;; order, followed by a JSON string. I'm going to borrow a lot from the
;;; TeXmacs client scheme code, but do the socket read/write myself. I hope
;;; this works.

(texmacs-module (zotero)
  (:use (kernel texmacs tm-modes)
        (kernel library content)
        (convert tools sxml)))

(use-modules (ice-9 popen))
(use-modules (ice-9 format))
(use-modules (json)) ;; Ported from Guile 2.0 to Guile 1.8 by Karl M. Hegbloom.

;;; The normal client-base mechanism will probably deal with this:
;; (sigaction SIGPIPE (lambda (sig) #t))

;; this just restructures the error object to match what TeXmacs is expecting inside the scheme session.
(define (safe-json-string->scm str)
  (catch 'json-invalid
    (lambda ()
      (json-string->scm str))
    (lambda args
      (throw 'json-invalid "Invalid JSON" "Invalid JSON" #f))))



;;;; Protocol between tm_zotero and ZoteroTeXmacsIntegration.js
;;
;; https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol
;;
;; The Firefox or Zotero Standalone process operates a server on port 23116,
;; which the extension residing within TeXmacs connects to. All frames consist
;; of a 32 bits specifying the transaction ID, a big-endian 32-bit integer
;; specifying the length of the payload, and the payload itself, which is
;; either UTF-8 encoded JSON or an unescaped string beginning with “ERR:”.
;;
;; Base:
;;


(define zotero-socket-port #f)
(define zotero-socket-inet-texmacs-port-number 23117)
(define zotero-socket-inet-zotero-port-number 23116)
  
(define (get-zotero-socket-port!)
  (catch 'system-error
    (lambda ()
      (if (and (port? zotero-socket-port)
               (not (port-closed? zotero-socket-port)))
          zotero-socket-port
          (begin
            (set! zotero-socket-port (socket PF_INET SOCK_STREAM 0))
            (setsockopt zotero-socket-port SOL_SOCKET SO_REUSEADDR 1)
            ;; (setsockopt zotero-socket-port IPPROTO_TCP TCP_NODELAY 1)
            (setvbuf zotero-socket-port _IOFBF)
            (bind    zotero-socket-port AF_INET INADDR_LOOPBACK 
                     zotero-socket-inet-texmacs-port-number)
            (connect zotero-socket-port AF_INET INADDR_LOOPBACK
                     zotero-socket-inet-zotero-port-number)
            zotero-socket-port)))
    (lambda args
      (close-port zotero-socket-port)
      (set! zotero-socket-port #f)
      (apply throw args))))


(define (close-zotero-socket-port!)
  (if (and (port? zotero-socket-port)
           (not (port-closed? zotero-socket-port)))
      (begin
        (close-port zotero-socket-port)
        (set! zotero-socket-port #f))))


(define (write-network-u32 value port)
  (let ((v (make-u32vector 1 0)))
    (u32vector-set! v 0 (htonl value))
    (uniform-vector-write v port)))

(define (read-network-u32 port)
  (let ((v (make-u32vector 1 0)))
    (uniform-vector-read! v port)
    (ntohl (u32vector-ref v 0))))


(define (zotero-write tid cmd)
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
        (write args)
        (apply throw args)))))


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
        (write args)
        (apply throw args)))))

(define (zotero-select-then-read)
  (let ((zp (get-zotero-socket-port!)))
    (with (r w e) (select (list zp) '() '() 2);; 2 sec
      (if (not (null? r))
          (zotero-read)
          (list 0 0 "")))))


;; see: (generic widgets):wait-for-toolbar in
;; progs/generic/generic-widgets.scm at 366 for how to implement a timeout
;; using delayed.

(define zotero-active? #f)
(define zotero-Document_insert-result #f)

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
  (with wait 1
    (delayed
      (:while zotero-active?)
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with (tid len cmdstr) (zotero-select-then-read)
        (write (list 'tid: tid 'len: len 'cmdstr: cmdstr))
        (newline)
        (when (> len 0)
          (with (editCommand args) (safe-json-string->scm cmdstr)
            (write (list editCommand args))
            (newline)
            (cond
              ((string=? editCommand "Document_complete")
               (zotero-write tid (scm->json-string '()))
               (set! zotero-active? #f)
               (close-zotero-socket-port!)
               (set! wait 1))
              ;; (zotero-Document_insert-result
              ;;  => (lambda (result)
              ;;       (set! zotero-Document_insert-result #f)
              ;;       (zotero-write tid (scm->json-string result))))
              ;; ((string=? editCommand "Document_insertField")
              ;;  ((set! zotero-Document_insertField-state-run-bottom-half? #t)
              ;;   (apply (eval ;; to get the function itself
              ;;                       (string->symbol 
              ;;                        (string-append "zotero-"
              ;;                                       editCommand)))
              ;;                      args)
              ;;    (write result)
              ;;    (newline)
              ;;    (write (scm->json-string result))
              ;;    (newline)
              ;;    (display "--------------------\n\n")
              ;;    (zotero-write tid (scm->json-string result))
              ;;    (set! wait 1)))
              (#t (with result (apply (eval ;; to get the function itself
                                       (string->symbol 
                                        (string-append "zotero-"
                                                       editCommand)))
                                      args)
                    (write result)
                    (newline)
                    (write (scm->json-string result))
                    (newline)
                    (display "--------------------\n\n")
                    (zotero-write tid (scm->json-string result))
                    (set! wait 1))))))))))


;;; Ensure that the tm-zotero.ts is part of the document style.
;;
(define (ensure-tm-zotero-style!)
  (add-style-package "tm-zotero"))


;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;; Editor commands.
;;
(tm-define (zotero-addCitation)
  (zotero-write 0 (scm->json-string "addCitation"))
  (zotero-listen))

(tm-define (zotero-editCitation)
  (zotero-write 0 (scm->json-string "editCitation"))
  (zotero-listen))

;; ---------

(tm-define (zotero-addBibliography)
  (zotero-write 0 (scm->json-string "addBibliography"))
  (zotero-listen))

(tm-define (zotero-editBibliography)
  (zotero-write 0 (scm->json-string "editBibliography"))
  (zotero-listen))

;; ---------

(tm-define (zotero-refresh)
  (zotero-write 0 (scm->json-string "refresh"))
  (zotero-listen))

;; (tm-define (zotero-removeCodes)
;;   (:secure #t)
;;   (zotero-write 0 (scm->json-string "removeCodes"))
;;   (zotero-listen))

;; ---------

(tm-define (zotero-setDocPrefs)
  (zotero-write 0 (scm->json-string "setDocPrefs"))
  (zotero-listen))



(menu-bind zotero-menu
  ("Add Citation" (zotero-addCitation))
  ("Edit Citation" (zotero-editCitation))
  ---
  ("Add Bibliography" (zotero-addBibliography))
  ("Edit Bibliography" (zotero-editBibliography))
  ---
  ("Refresh" (zotero-refresh))
;;  ("Remove Codes" (zotero-removeCodes))
  ---
  ("Set Document Prefs" (zotero-setDocPrefs)))


(menu-bind texmacs-extra-menu
  (former)
  (if (style-has? "tm-zotero-dtd")
      (=> "Zotero"
          (link zotero-menu))))


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

;; (define-preferences
;;   ("tm-zotero:hlinks-as-footnotes" "on" init-env)
;;   ("tm-zotero:hlinks-as-tt"        "on" init-env)
;;   ("tm-zotero:hlinks-as-smaller"   "on" init-env))

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
;; (tm-define (update-document what)
;;   (:require (style-has? "tm-zotero-dtd"))
;;   (for (.. 0 doc-update-times)
;;     (delayed ; allow typesetting/magic to happen before next update
;;       (:idle 1)
;;       (cursor-after
;;        (cond ((== what "all") 
;;               (generate-all-aux) (inclusions-gc) (wait-update-current-buffer))
;;              ((== what "bibliography")
;;               (generate-all-aux) (wait-update-current-buffer))
;;              ((== what "buffer") 
;;               (wait-update-current-buffer))
;;              (else (generate-aux what)))))))
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
(define (zotero-getDocId)
  (car (buffer-path)))

(tm-define (zotero-Application_getActiveDocument pv)
  (list pv (zotero-getDocId)))



;; (define DIALOG_ICON_STOP 0)
;; (define DIALOG_ICON_NOTICE 1)
;; (define DIALOG_ICON_CAUTION 2)

;; (define DIALOG_BUTTONS_OK 0)
;; (define DIALOG_BUTTONS_OK_OK_PRESSED 1)

;; (define DIALOG_BUTTONS_OK_CANCEL 1)
;; (define DIALOG_BUTTONS_OK_CANCEL_CANCEL_PRESSED 0)
;; (define DIALOG_BUTTONS_OK_CANCEL_OK_PRESSED 1)

;; (define DIALOG_BUTTONS_YES_NO 2)
;; (define DIALOG_BUTTONS_YES_NO_NO_PRESSED 0)
;; (define DIALOG_BUTTONS_YES_NO_YES_PRESSED 1)

;; (define DIALOG_BUTTONS_YES_NO_CANCEL 3)
;; (define DIALOG_BUTTONS_YES_NO_CANCEL_CANCEL_PRESSED 0)
;; (define DIALOG_BUTTONS_YES_NO_CANCEL_NO_PRESSED 1)
;; (define DIALOG_BUTTONS_YES_NO_CANCEL_YES_PRESSED 2)


(tm-widget ((zotero-display-alert str_Text int_Icon int_Buttons) cmd)
  (hlist ((icon (list-ref (map %search-load-path
                               '("icon-stop.png"
                                 "icon-notice.png"
                                 "icon-caution.png"))
                          int_Icon)) (noop))
         ;;; This does not work right with newline characters in the string.
         >> (text str_Text))
  (bottom-buttons >>> (cond
                        ((= int_Buttons 0)
                         ("Ok"     (cmd 1)))
                        ((= int_Buttons 1)
                         ("Ok"     (cmd 1))
                         ("Cancel" (cmd 0)))
                        ((= int_Buttons 2)
                         ("Yes"    (cmd 1))
                         ("No"     (cmd 0)))
                        ((= int_Buttons 3)
                         ("Yes"    (cmd 2))
                         ("No"     (cmd 1))
                         ("Cancel" (cmd 0))))))


;; Shows an alert.
;;
;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;
(tm-define (zotero-Document_displayAlert documentID str_dialogText int_icon int_buttons)
  (let ((ret 0))
    (dialogue-window (zotero-display-alert str_dialogText int_icon int_buttons)
                     (lambda (val)
                       (set! ret val))
                     "Zotero Alert!")
    ret))


;; Brings the document to the foreground. (For OpenOffice, this is a no-op on non-Mac systems.)
;;
;; ["Document_activate", [documentID]] -> null
;;
(tm-define (zotero-Document_activate documentID)
  ;; stub
  '())


;; Indicates whether a field can be inserted at the current cursor position.
;;
;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;
(tm-define (zotero-Document_canInsertField documentID str_fieldType)
  (and (in-text?)
       (not (in-math?))
       (not (inside-which zt-cite-tags))))



;; AFAIK the only pref that this program needs access to is noteType, and that
;; access is read-only. The noteType is a document-wide setting.
;;
;; enum noteType
;;
(define-public zotero-NOTE_IN_TEXT  0)
(define-public zotero-NOTE_FOOTNOTE 1)
(define-public zotero-NOTE_ENDNOTE  2)
;;
;; The rest of the DocumentData settings are "opaque" from the viewpoint of
;; this interface. They control Zotero, not this connector.
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
;;       (pref (@ (name "citationAffixes")               (value "|||||||||||||||||||||||||||||||||||||||||||||||")))
;;       (pref (@ (name "projectName")                   (value "Project:TeXmacsTesting")))
;;       (pref (@ (name "extractingLibraryID")           (value "0")))
;;       (pref (@ (name "extractingLibraryName")         (value "No group selected")))
;;       (pref (@ (name "fieldType")                     (value "ReferenceMark")))
;;       (pref (@ (name "storeReferences")               (value "true")))
;;       (pref (@ (name "automaticJournalAbbreviations") (value "true")))
;;       (pref (@ (name "noteType")                      (value "0")))
;;       (pref (@ (name "suppressTrailingPunctuation")   (value "true")))))))


(define (zotero-get-DocumentData)
  (get-env "zoteroDocumentData"))

(define (zotero-set-DocumentData str_dataString)
  (init-env "zoteroDocumentData" str_dataString)
  (zotero-init-env-zotero-prefs))

(define (zotero-init-env-zotero-prefs)
  (write (parse-xml (zotero-get-DocumentData)))
  (newline)
  (let ((zotero-init-env-zotero-prefs-sub
         (lambda (prefix attr-list)
           (let loop ((attr-list attr-list))
                (cond
                  ((null? attr-list) #t)
                  (#t (init-env (string-append prefix (symbol->string
                                                       (caar attr-list)))
                                (cadar attr-list))
                   (loop (cdr attr-list))))))))
    (let loop ((sxml (cdr (parse-xml (zotero-get-DocumentData)))))
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
            (loop (cdr sxml)))))))
          

;; Retrieves data string set by setDocumentData.
;;
;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;
(tm-define (zotero-Document_getDocumentData documentID)
  (zotero-get-DocumentData))


;; Stores a document-specific persistent data string. This data
;; contains the style ID and other user preferences.
;;
;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;
(tm-define (zotero-Document_setDocumentData documentID str_dataString)
  (zotero-set-DocumentData str_dataString)
  '())



;; Indicates whether the cursor is in a given field. If it is, returns
;; information about that field. Returns null, indicating that the
;; cursor isn't in a field, or an array:
;;
;; fieldID, int or string, A unique identifier corresponding to this
;; field fieldCode, int or string, The code stored within this field
;; noteIndex, int, The number of the footnote in which this field
;;                 resides, or 0 if the field is not in a footnote.
;;
;; ["Document_cursorInField", [documentID, str_fieldType]] -> null || [fieldID, fieldCode, int_noteIndex]
;;
(tm-define (zotero-Document_cursorInField documentID str_fieldType)
  (with t (inside-which '(zcite* zcite))
    (if t
        (begin
          (list (tree->string (zotero-zcite-fieldID t))
                (tree->string (zotero-zcite-filedCode t))
                (tree->string (zotero-zcite-noteIndex t))))
        '())))


;; Inserts a new field at the current cursor position.
;;
;; str_fieldType, either "ReferenceMark" or "Bookmark"
;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;
;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]
;;
;; Ignore: str_fieldType
;;
(tm-define (zotero-Document_insertField documentID str_fieldType int_noteType)
  (let* ((id (string-append (format "~s" documentID #f) (create-unique-id))))
    (insert `(zcite ,id "TEMP" "TEMP" ""))
    ;; This does not work. The typesetter has to run! Need delayed somehow.
    (let ((field (zotero-find-zcite id)))
      (list id "TEMP" (zotero-zcite-fieldNoteIndex field)))))
    ;; ;; Give the typesetter a second to update the noteIndex...
    ;; (delayed
    ;;   (:delay 1)
      
    ;;   (let ((field (zotero-find-zcite id)))
    ;;     (set! zotero-Document_insert-result
    ;;       (list id "TEMP" (zotero-zcite-noteIndex field)))))))


  
;; Get all fields present in the document, in document order.
;;
;; str_fieldType is the type of field used by the document, either ReferenceMark or Bookmark
;;
;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]
;;

;; (<tree <zcite|id01|code|0|Text>> <tree <zcite|id02|code|0|Text2>> <tree
;; <zcite|id03|code|0|Text3>>)
;; ((zcite "id01" "code" "0" "Text") (zcite "id02" "code" "0" "Text2") (zcite "id03" "code" "0" "Text3"))

(define zt-cite-tags '(zcite zcite*))

(tm-define (zotero-Document_getFields documentID str_fieldType)
  (let loop ((zcite-fields (map tree->stree
                                (tm-search (buffer-tree)
                                           (cut tm-in? <> zt-cite-tags))))
             (ids '()) (codes '()) (indx '()))
       (cond
         ((null? zcite-fields) (list ids codes indx))
         (#t
          (let ((field (car zcite-fields)))
            (loop (cdr zcite-fields)
                  (cons (second field) ids)
                  (cons (third  field) codes)
                  (cons (fourth field) indx)))))))
    

;;; May need: go-to-id from link/link-navigate.scm:490

;; ["Document_convert" ??? (TODO in documentation.)
;;
(tm-define (zotero-Document_convert . args)
  ;; stub
  (write (list "STUB:zotero-Document_convert" args))
  (newline)
  '())


;; ["Document_setBibliographyStyle", ??? (TODO)
;;
(tm-define (zotero-Document_setBibliographyStyle . args)
  ;; stub
  (write (list "STUB:zotero-Document_setBibliographyStyle" args))
  (newline)
  '())


;; Indicates that the given documentID will no longer be used and
;; associated resources may be freed.
;;
;; ["Document_complete", [documentID]] -> null
;;
;; See: zotero-listen, where this is checked for inline... but also enable it
;; here since I might need to use it during development, at least.
;;
(tm-define (zotero-Document_complete documentID)
  (set! zotero-active? #f)
  (close-zotero-socket-port!)
  '())



;; Operations on zcite fields.
;;
;;
(define (zotero-find-zcite fieldID)
  "Returns the tree with the specified @fieldID string, or #f."
  (tm-find (buffer-tree)
           (lambda (t)
             (and (tm-in? t zt-cite-tags)
                  (string=? (tree->stree (zotero-zcite-fieldID t))
                            fieldID)))))

;; These must match the definitions in tm-zotero.ts; the fieldID
;; is expected to come first in the code above.
;;
(define (zotero-zcite-fieldID t)
  (tm-ref t 0))

(define (zotero-zcite-fieldCode t)
  (tm-ref t 1))

(define (zotero-zcite-fieldRawText t)
  (tm-ref t 2))

;; Todo: how do I get a reference binding? Is there an accessor for them?
;;
;; Hack: inside of a hidden*, write a tuple of them as the action of some
;; observer...

;; The set-binding call happens inside of the macro that renders the
;; citation. I spent half a day figuring out how to write a glue-exported
;; accessor function... then discovered this trick:
;;
(define (get-reference-binding label)
  (texmacs-exec `(get-binding ,label)))

;; For "note" styles, this reference binding links a citation field with
;; the footnote number that it appears in.
;;
(define (zotero-zcite-fieldNoteIndex t)
  (get-reference-binding (string-append
                          "zotero-"
                          (tree->string (zotero-zcite-fieldID t))
                          "-noteIndex")))

;; This field is set automatically, below, with the result of:
;;
;;   (latex->texmacs (parse-latex fieldRawText))
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
(define (zotero-zcite-fieldText t)
  (tm-ref t 3))


;; Deletes a field from the document (both its code and its contents).
;;
;; fieldID as originally returned by Document_cursorInField,
;; Document_insertField, or Document_getFields.
;;
;; ["Field_delete", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_delete documentID fieldID)
  (tree-assign! (zotero-find-zcite fieldID) "")
  '())


;; Moves the current cursor position to encompass a field.
;;
;; ["Field_select", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_select documentID fieldID)
  (tree-go-to (zotero-find-zcite fieldID) 0)
  '())


;; Removes the field code from a field, leaving it as plain text.
;;
;; ["Field_removeCode", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_removeCode documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID))
         (text (zotero-zcite-fieldText field)))
    (tree-set-diff field text))
  '())


;; Sets the (visible) text of a field.
;;
;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;
;; Let's assume that for this, it's always "isRich", so ignore that arg.
;;
(tm-define (zotero-Field_setText documentID fieldID str_text isRich)
  (let* ((field (zotero-find-zcite fieldID))
         (rawText (zotero-zcite-fieldRawText field))
         (text (zotero-zcite-fieldText field)))
    ;; There are two copies because it's planned to make it possible
    ;; to edit one, but for now it only really uses the last one.
    (tree-assign! rawText (latex->texmacs (parse-latex str_text)))
    ;; newly consed, not eq? to rawText.
    (tree-assign! text (latex->texmacs (parse-latex str_text))))
  '())


;; Gets the (visible) text of a field.
;;
;; ["Field_getText", [documentID, fieldID]] -> str_text
;;
(tm-define (zotero-Field_getText documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID)))
    (zotero-zcite-fieldText field)))

;; Sets the (hidden, persistent) code of a field.
;;
;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;
(tm-define (zotero-Field_setCode documentID fieldID str_code)
  (let* ((field (zotero-find-zcite fieldID))
         (code (zotero-zcite-fieldCode field)))
    (tree-assign! code str_code)) ;; opaque, just store it.
  '())


;; Converts a field from one type to another.
;;
;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;; null
;;
(tm-define (zotero-Field_convert documentID fieldID str_fieldType int_noteType)
  (noop)
  '())

;;; Local Variables:
;;; fill-column: 120
;;; End:
