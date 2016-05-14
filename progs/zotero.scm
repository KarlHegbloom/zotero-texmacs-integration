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

(texmacs-module (zotero))

(use-modules (ice-9 format))
(use-modules (json)) ;; Ported from Guile 2.0 to Guile 1.8 by Karl M. Hegbloom.

(sigaction SIGPIPE (lambda (sig) #t))

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


;; socket family   style        proto
;;        PF_INET  SOCK_STREAM  0
(define zotero-socket-port #f)
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
            (bind    zotero-socket-port AF_INET INADDR_LOOPBACK 23117)
            (connect zotero-socket-port AF_INET INADDR_LOOPBACK 23116)
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


(tm-define (zotero-write tid cmd)
  (let ((zp (get-zotero-socket-port!)))
    (catch 'system-error
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


(tm-define (zotero-read)
  (let ((zp (get-zotero-socket-port!)))
    (catch 'system-error
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

(tm-define (zotero-select-then-read)
  (let ((zp (get-zotero-socket-port!)))
    (with (r w e) (select (list zp) '() '() 10);; 10 sec
      (if (not (null? r))
          (zotero-read)
          (list 0 0 "")))))


(define zotero-active? #f)

;; see: (generic widgets):wait-for-toolbar in
;; progs/generic/generic-widgets.scm at 366 for how to implement a timeout
;; using delayed.

;;; State machine; protocol is essentially synchronous, and user expects to
;;; wait while it finishes before doing anything else anyhow.
;;;
;;; When this is entered, one of the Integration commands has just been sent to
;;; Juris-M / Zotero. It is expected to call back and begin a word processing
;;; command sequence, culminating with Document_complete.
;;;
;; (tm-define (zotero-listen)
;;   (set! zotero-active? #t)
;;   (while zotero-active?
;;     (with (tid len cmdstr) (zotero-select-then-read)
;;       (display* "tid:" tid " len:" len "cmdstr:" cmdstr "\n")
;;       (when (> len 0)
;;         (with (editCommand args) (safe-json-string->scm cmdstr)
;;           (cond
;;             ((string=? editCommand "Document_complete")
;;              (zotero-write tid (scm->json-string '()))
;;              (set! zotero-active? #f)
;;              (close-zotero-socket-port!))
;;             (#t (with result (apply (eval 
;;                                      (string->symbol 
;;                                       (string-append "zotero-" editCommand)))
;;                                     args)
;;                   (zotero-write tid (scm->json-string result))))))))))


(tm-define (zotero-listen)
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
              (#t (with result (apply (eval 
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



;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;; Editor commands.
;;
(tm-define (zotero-addCitation)
  (:secure #t)
  (zotero-write 0 (scm->json-string "addCitation"))
  (zotero-listen))

(tm-define (zotero-editCitation)
  (:secure #t)
  (zotero-write 0 (scm->json-string "editCitation"))
  (zotero-listen))

(tm-define (zotero-addBibliography)
  (:secure #t)
  (zotero-write 0 (scm->json-string "addBibliography"))
  (zotero-listen))

(tm-define (zotero-editBibliography)
  (:secure #t)
  (zotero-write 0 (scm->json-string "editBibliography"))
  (zotero-listen))

(tm-define (zotero-refresh)
  (:secure #t)
  (zotero-write 0 (scm->json-string "refresh"))
  (zotero-listen))

(tm-define (zotero-removeCodes)
  (:secure #t)
  (zotero-write 0 (scm->json-string "removeCodes"))
  (zotero-listen))

(tm-define (zotero-setDocPrefs)
  (:secure #t)
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
  ("Remove Codes" (zotero-removeCodes))
  ---
  ("Set Document Prefs" (zotero-setDocPrefs)))

(menu-bind texmacs-extra-menu
  (former)
  (if (style-has? "tm-zotero-dtd")
      (=> "Zotero"
          (link zotero-menu))))
      

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
(define-public (zotero-getDocId)
  (car (buffer-path)))

(tm-define (zotero-Application_getActiveDocument pv)
  ;; stub
  (write (list "zotero-Application_getActiveDocument" pv))
  (newline)
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
  (write (list "zotero-Document_displayAlert" documentID str_dialogText int_icon int_buttons))
  (newline)
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
  (write (list "zotero-Document_activate" documentID))
  (newline)
  '())


;; Indicates whether a field can be inserted at the current cursor position.
;;
;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
(tm-define (zotero-Document_canInsertField documentID str_fieldType)
  ;; stub
  (write (list "zotero-Document_canInsertField" documentID str_fieldType))
  (newline)
  #t)


;; Stores a document-specific persistent data string. This data
;; contains the style ID and other user preferences.
;;
;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;
(define zotero-document-data (make-ahash-table)) ;;; todo: store it in aux

(tm-define (zotero-Document_setDocumentData documentID str_dataString)
  ;; stub
  (write (list "zotero-Document_setDocumentData" documentID str_dataString))
  (newline)
  (ahash-set! zotero-document-data documentID str_dataString)
  '())


;; Retrieves data string set by setDocumentData.
;;
;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;
(tm-define (zotero-Document_getDocumentData documentID)
  ;; stub: Todo: get from document aux
  (write (list "zotero-Document_getDocumentData" documentID))
  (newline)
  (ahash-ref zotero-document-data documentID ""))


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
  ;; stub
  (write (list "zotero-Document_cursorInField" documentID str_fieldType))
  (newline)
  '())


(define NOTE_IN_TEXT 0)
(define NOTE_FOOTNOTE 1)
(define NOTE_ENDNOTE 2)

(define zotero-fields (make-ahash-table))

(define (zotero-get-noteindex id)
  ;; stub
  1)

;; Inserts a new field at the current cursor position.
;;
;; str_fieldType, either "ReferenceMark" or "Bookmark"
;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;
;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]
;;
(tm-define (zotero-Document_insertField documentID str_fieldType int_noteType)
  ;; stub
  (write (list "zotero-Document_insertField" documentID str_fieldType int_noteType))
  (newline)
  (let* ((id (string-append (format "~s" documentID #f) (create-unique-id)))
         (ni (if (= int_noteType 0) 0
                 (zotero-get-noteindex id))))
    (ahash-set! zotero-fields id (list "TEMP" 0))
    (insert `(zcite ,id "TEMP"))
    (list id "TEMP" ni)))


(define zotero-fields-list '())

(tm-define (zotero-set-fields-list! tuple)
  (:secure #t)
  (set! zotero-fields-list (cdr (tree->stree tuple))))
  
;; Get all fields present in the document, in document order.
;;
;; str_fieldType is the type of field used by the document, either ReferenceMark or Bookmark
;;
;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]
;;
(tm-define (zotero-Document_getFields documentID str_fieldType)
  ;; stub
  (write (list "zotero-Document_getFields" documentID str_fieldType))
  (newline)
  
  (list (list 0) (list "ReferenceMark") (list 0)))


;; ["Document_convert" ??? (TODO in documentation.)
;;
(tm-define (zotero-Document_convert . args)
  ;; stub
  (write (list "zotero-Document_convert" args))
  (newline)
  '())


;; ["Document_setBibliographyStyle", ??? (TODO)
;;
(tm-define (zotero-Document_setBibliographyStyle . args)
  ;; stub
  (write (list "zotero-Document_setBibliographyStyle" args))
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
  ;; stub
  (write (list "zotero-Document_complete" documentID))
  (newline)
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
  ;; stub
  (write (list "zotero-Field_delete" documentID fieldID))
  (newline)
  '())


;; Moves the current cursor position to encompass a field.
;;
;; ["Field_select", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_select documentID fieldID)
  ;; stub
  (write (list "zotero-Field_select" documentID fieldID))
  (newline)
  '())


;; Removes the field code from a field, leaving it as plain text.
;;
;; ["Field_removeCode", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_removeCode documentID fieldID)
  ;; stub
  (write (list "zotero-Field_removeCode" documentID fieldID))
  (newline)
  '())


;; Sets the (visible) text of a field.
;;
;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;
(tm-define (zotero-Field_setText documentID fieldID str_text isRich)
  ;; stub
  (write (list "zotero-Field_setText" documentID fieldID str_text isRich))
  (newline)
  '())


;; Gets the (visible) text of a field.
;;
;; ["Field_getText", [documentID, fieldID]] -> str_text
;;
(tm-define (zotero-Field_getText documentID fieldID)
  ;; stub
  (write (list "zotero-Field_getText" documentID fieldID))
  (newline)
  "STUB Field Text STUB")


;; Sets the (hidden, persistent) code of a field.
;;
;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;
(tm-define (zotero-Field_setCode documentID fieldID str_code)
  ;; stub
  (write (list "zotero-Field_setCode" documentID fieldID str_code))
  (newline)
  '())


;; Converts a field from one type to another.
;;
;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;; null
;;
(tm-define (zotero-Field_convert documentID fieldID str_fieldType int_noteType)
  ;; stub
  (write (list "zotero-Field_convert" documentID fieldID str_fieldType
               int_noteType))
  (newline)
  '())
