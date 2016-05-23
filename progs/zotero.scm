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
        (convert tools sxml)
        (convert rtf rtftm)))


;; Ported from Guile 2.0 to Guile 1.8 by Karl M. Hegbloom.
(use-modules (json))

;; This just restructures the error object to match what TeXmacs is expecting
;; inside the scheme session.
;;
(define (safe-json-string->scm str)
  (catch 'json-invalid
    (lambda ()
      (json-string->scm str))
    (lambda args
      (throw 'json-invalid "Invalid JSON" "Invalid JSON" #f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Helper functions
;;

(define (coerce-to-string obj)
  "Some arguments in this protocol can be number or string. Coerce to string."
  (cond
    ((number? obj)
     (number->string obj))
    ((tree? obj)
     (tree->string obj))
    ((string? obj) obj)
    (#t
     (display "Coerce to string? Error:")
     (write obj)
     (newline))))


;; The documentID; this is not stable from one run to the next since it's value
;; depends on whether this is the first document buffer upon launching TeXmacs
;; or some subsequently loaded one. It does not have to be stable from one run
;; to the next, but unique for each document being processed during this run.
;;
(tm-define (zotero-getDocId)
  (coerce-to-string (car (buffer-path))))

(tm-define (zotero-get-new-fieldID)
  (coerce-to-string (create-unique-id)))

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
(define zt-cite-tags '(zcite zcite* zbibliography))

;;; Ensure that the tm-zotero.ts is part of the document style.
;;
(define (ensure-tm-zotero-style!)
  (add-style-package "tm-zotero"))

;;; This young code says that it wants to be a GOOPS object someday. I'm not
;;; sure if that's right for it yet.

;; Operations on zcite fields.
;;
;;

;;; tm-find returns an incorrect result! Use tm-search.
;;;
(define (zotero-find-zcite fieldID)
  (car
   (tm-search
    (buffer-tree)
    (lambda (t)
      (and (tree-in? t zt-cite-tags)
           (string=? fieldID
                     (tree->string (tree-ref t 0))))))))
    
;; These must match the definitions in tm-zotero.ts;
;;
;;  L     0         1           2              3
;; (zcite "fieldID" "fieldCode" "fieldRawText" "fieldText")
;;
;; fieldNoteIndex is gotten via a reference binding.
;;
(define-public (zotero-zcite-fieldID t)
  (tree-ref t 0))

(define-public (zotero-zcite-fieldCode t)
  (tree-ref t 1))

(define-public (zotero-zcite-fieldRawText t)
  (tree-ref t 2))

;; For "note" styles, this reference binding links a citation field with
;; the footnote number that it appears in.
;;
(define-public (zotero-zcite-fieldNoteIndex field)
  (get-reference-binding
   (zotero-ref-binding-key
    (tree->string (zotero-zcite-fieldID field)))))


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
(define-public (zotero-zcite-fieldText t)
  (tree-ref t 3))




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
      (set! zotero-active? #f)
      ;; Todo: Improve error dialog.
      (zotero-Document_displayAlert 0 "System Error: Is Zotero running? Also maybe restart Firefox or Zotero SA." 0 0))))


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
    (with (r w e) (select (list zp) '() '() 1);; 1 sec
      (if (not (null? r))
          (zotero-read)
          (list 0 0 "")))))


;; see: (generic widgets):wait-for-toolbar in
;; progs/generic/generic-widgets.scm at 366 for how to implement a timeout
;; using delayed.

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
  (with (counter wait) '(1000 10)
    (delayed
      (:while zotero-active?)
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with (tid len cmdstr) (zotero-select-then-read)
        (write (list 'tid: tid 'len: len 'cmdstr: cmdstr))
        (newline)
        (when (not (> len 0))
          (set! counter (- counter 1))
          (when (= counter 0)
            (set! zotero-active? #f)
            (zotero-write tid (scm->json-string '()))
            (set! wait 0)))
        (when (> len 0)
          (with (editCommand args) (safe-json-string->scm cmdstr)
            (write (list editCommand args))
            (newline)
            (cond
              ((string=? editCommand "Document_complete")
               (zotero-write tid (scm->json-string '()))
               (set! zotero-active? #f)
               (close-zotero-socket-port!)
               (set! wait 10))
              (#t
               (with result (apply (eval ;; to get the function itself
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
                    (set! wait 10))))))))))



(define zotero-new-fieldID #f)

;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;; Editor commands.
;;
(tm-define (zotero-addCitation)
  (let ((t (inside-which zt-cite-tags)))
    (if t
        (when (not (and zotero-new-fieldID
                        (string=? zotero-new-fieldID
                                  (zotero-zcite-fieldID t))))
          (zotero-editCitation))
        (begin
          (let* ((id (zotero-get-new-fieldID)))
            (set! zotero-new-fieldID id)
            (insert `(zcite ,id "" "{Citation}" "")))
          (zotero-write 0 (scm->json-string "addCitation"))
          (zotero-listen)))))

(tm-define (zotero-editCitation)
  (zotero-write 0 (scm->json-string "editCitation"))
  (zotero-listen))

;; ---------

(tm-define (zotero-addBibliography)
  (let ((id (string-append (coerce-to-string (zotero-getDocId)) (create-unique-id))))
    (set! zotero-new-fieldID id)
    (insert `(zbibliography ,id "" "{Citation}" "")))
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
  ;; ("Remove Codes" (zotero-removeCodes))
  ---
  ("Set Document Prefs" (zotero-setDocPrefs)))


(menu-bind texmacs-extra-menu
  (when (style-has? "tm-zotero-dtd")
    (former)
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
  (noop)
  '())


;; Indicates whether a field can be inserted at the current cursor position.
;;
;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;
(tm-define (zotero-Document_canInsertField documentID str_fieldType)
  (and (in-text?)
       (not (in-math?))
       (let ((t (inside-which zt-cite-tags)))
         (or (not t)
             (and zotero-new-fieldID
                  (string=? zotero-new-fieldID (zotero-zcite-fieldID t)))))))



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
  (zotero-init-env-zotero-prefs str_dataString))


(define (zotero-init-env-zotero-prefs str_dataString)
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
  (if (tree-in? (cursor-tree) zt-cite-tags)
      (let ((t (cursor-tree)))
        (if (not (and zotero-new-fieldID
                      (string=? zotero-new-fieldID
                                (coerce-to-string
                                 (zotero-zcite-fieldID t)))))
            (begin
              (list (coerce-to-string (zotero-zcite-fieldID t))
                    (coerce-to-string (zotero-zcite-fieldCode t))
                    (coerce-to-string (zotero-zcite-fieldNoteIndex t))))
            '()))
      '()))



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
  (let ((field (zotero-find-zcite zotero-new-fieldID))
        (id zotero-new-fieldID))
    (set! zotero-new-fieldID #f)
    ;; (tree-go-to field 1)
    (list id "" (coerce-to-string (zotero-zcite-fieldNoteIndex field)))))

  

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
(tm-define (zotero-Document_getFields documentID str_fieldType)
  (let loop ((zcite-fields (tm-search
                            (buffer-tree)
                            (lambda (t)
                              (and (tree-in? t zt-cite-tags)
                                   (not
                                    (and zotero-new-fieldID
                                         (string=? (zotero-zcite-fieldID t)
                                                   zotero-new-fieldID)))))))
             (ids '()) (codes '()) (indx '()))
       (cond
         ((null? zcite-fields) (if (nnull? ids)
                                   (list (reverse ids)
                                         (reverse codes)
                                         (reverse indx))
                                   '((0) ("TEMP") (0))))
         (#t
          (let ((field (car zcite-fields)))
            (loop (cdr zcite-fields)
                  (cons (coerce-to-string (zotero-zcite-fieldID        field)) ids)
                  (cons (coerce-to-string (zotero-zcite-fieldCode      field)) codes)
                  (cons (coerce-to-string (zotero-zcite-fieldNoteIndex field)) indx)))))))
    


;; ["Document_convert" ??? (TODO in documentation.)
;;
;; public void convert(ReferenceMark mark, String fieldType, int noteType)
;;
;; I think this is for OpenOffice to convert a document from using ReferenceMark fields to Bookmark ones.
;; Maybe we could repurpose this for TeXmacs?  Better to make a new flag; and just ignore this one.
;;
(tm-define (zotero-Document_convert . args)
  (noop)
  '())



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
(tm-define (zotero-Document_setBibliographyStyle
            documentID
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
  (noop)
  '())



;; Not documented, but exists in CommMessage.java in LibreOffice side of the
;; connector. It appears to do nothing there either.
;;
(tm-define (zotero-Document_cleanup documentID)
  (noop)
  '())

;; Indicates that the given documentID will no longer be used and
;; associated resources may be freed.
;;
;; ["Document_complete", [documentID]] -> null
;;
;; See: zotero-listen, where this is checked for inline... but also enable it here since I might need to use it during
;; development, at least. It's never called at all by zotero-listen, so can just be commented off here.
;;
(tm-define (zotero-Document_complete documentID)
  (set! zotero-active? #f)
  (close-zotero-socket-port!)
  '())



;; Deletes a field from the document (both its code and its contents).
;;
;; fieldID as originally returned by Document_cursorInField,
;; Document_insertField, or Document_getFields.
;;
;; ["Field_delete", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_delete documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID))
         (code (zotero-zcite-fieldCode field))
         (rawText (zotero-zcite-fieldRawText field))
         (text (zotero-zcite-fieldText field)))
    (tree-assign! code "")
    (tree-assign! rawText "")
    (tree-assign! text ""))
  '())


;; Moves the current cursor position to encompass a field.
;;
;; ["Field_select", [documentID, fieldID]] -> null
;;
;; Whether or not this works as expected depends on settings made by the drd-props macro. I think that I want the cursor
;; to be inside of it's light blue box, after it.... (writing this comment prior to testing. FLW.)
;;
(tm-define (zotero-Field_select documentID fieldID)
  (let ((field (zotero-find-zcite fieldID)))
    (tree-go-to field 0))
  '())


;;
;; ["Field_removeCode", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_removeCode documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID))
         (code (zotero-zcite-fieldCode field)))
    (tree-assign! code ""))
  '())


(define-public (zotero-field-str_text-to-texmacs str_text)
  (latex->texmacs (parse-latex str_text)))

;; (tm-define (zotero-field-str_text-to-texmacs str_text)
;;   (rtf-snippet->texmacs str_text))

;; Sets the (visible) text of a field.
;;
;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;
;; Let's assume that for this, it's always "isRich", so ignore that arg.
;;
(tm-define (zotero-Field_setText documentID fieldID str_text isRich)
  (let* ((field   (zotero-find-zcite fieldID))
         (rawtext (zotero-zcite-fieldRawText field))
         (text    (zotero-zcite-fieldText field))
         (tmtext  (zotero-field-str_text-to-texmacs str_text)))
    (display "----------------- rawtext ----------------\n")
    (write rawtext) (newline)
    (display "----------------- tmtext -----------------\n")
    (write tmtext) (newline)
    (display "------------------------------------------\n")
    (tree-assign! rawtext str_text)
    (tree-assign! text tmtext))
  '())


;; Gets the (visible) text of a field.
;;
;; ["Field_getText", [documentID, fieldID]] -> str_text
;;
(tm-define (zotero-Field_getText documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID)))
    ;; (write field)
    ;; (newline)
    (coerce-to-string (zotero-zcite-fieldRawText field))))

;; Sets the (hidden, persistent) code of a field.
;;
;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;
(tm-define (zotero-Field_setCode documentID fieldID str_code)
  (let* ((field (zotero-find-zcite fieldID))
         (code (zotero-zcite-fieldCode field)))
    (tree-assign! code str_code)) ;; opaque, just store it.
  '())


(tm-define (zotero-Field_getCode documentID fieldID)
  (let* ((field (zotero-find-zcite fieldID)))
    (coerce-to-string (zotero-zcite-fieldCode field))))


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
