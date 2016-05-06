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
      (or zotero-socket-port
          (begin
            (set! zotero-socket-port (socket PF_INET SOCK_STREAM 0))
            (setsockopt zotero-socket-port SOL_SOCKET SO_REUSEADDR 1)
            ;; (setsockopt zotero-socket-port IPPROTO_TCP TCP_NODELAY 1)
            (setvbuf zotero-socket-port _IOFBF)
            (bind zotero-socket-port AF_INET INADDR_LOOPBACK 23117)
            (connect zotero-socket-port AF_INET INADDR_LOOPBACK 23116)
            zotero-socket-port)))
    (lambda args
      (close zotero-socket-port)
      (set! zotero-socket-port #f)
      (apply throw args))))


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
    (with (r w e) (select (list zp) '() '() 0 500000) ;; 0.5 sec
      (if (not (null? r))
          (zotero-read)
          (list 0 0 "")))))

(define zotero-active? #f)

(tm-define (zotero-listen)
  (set! zotero-active? #t)
  (with wait 1
    (delayed
      (:while zotero-active?)
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with (tid len cmdstr) (zotero-select-then-read)
        (when (> 0 len)
          (with (editCommand args) (json-string->scm cmdstr)
            (cond
              ((string-equal? editCommand "Document_complete")
               (write-zotero tid (scm->json-string '()))
               (set! zotero-active? #f)
               (set! wait 1))
              (#t (with result (apply (string-append "zotero-" editCommand) args)
                    (write-zotero tid (scm->json-string result))
                    (set! wait 1))))))))))



;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;; Editor commands.
;;
(tm-define (zotero-addCitation)
  (zotero-write 0 (scm->json-string "addCitation"))
  (zotero-listen))

(tm-define (zotero-editCitation)
  (zotero-write 0 (scm->json-string "editCitation"))
  (zotero-listen))

(tm-define (zotero-addBibliography)
  (zotero-write 0 (scm->json-string "addBibliography"))
  (zotero-listen))

(tm-define (zotero-editBibliography)
  (zotero-write 0 (scm->json-string "editBibliography"))
  (zotero-listen))

(tm-define (zotero-refresh)
  (zotero-write 0 (scm->json-string "refresh"))
  (zotero-listen))

(tm-define (zotero-removeCodes)
  (zotero-write 0 (scm->json-string "removeCodes"))
  (zotero-listen))

(tm-define (zotero-setDocPrefs)
  (zotero-write 0 (scm->json-string "setDocPrefs"))
  (zotero-listen))



(define-public DIALOG_ICON_STOP 0)
(define-public DIALOG_ICON_NOTICE 1)
(define-public DIALOG_ICON_CAUTION 2)

(define-public DIALOG_BUTTONS_OK 0)
(define-public DIALOG_BUTTONS_OK_OK_PRESSED 1)

(define-public DIALOG_BUTTONS_OK_CANCEL 1)
(define-public DIALOG_BUTTONS_OK_CANCEL_CANCEL_PRESSED 0)
(define-public DIALOG_BUTTONS_OK_CANCEL_OK_PRESSED 1)

(define-public DIALOG_BUTTONS_YES_NO 2)
(define-public DIALOG_BUTTONS_YES_NO_NO_PRESSED 0)
(define-public DIALOG_BUTTONS_YES_NO_YES_PRESSED 1)

(define-public DIALOG_BUTTONS_YES_NO_CANCEL 3)
(define-public DIALOG_BUTTONS_YES_NO_CANCEL_CANCEL_PRESSED 0)
(define-public DIALOG_BUTTONS_YES_NO_CANCEL_NO_PRESSED 1)
(define-public DIALOG_BUTTONS_YES_NO_CANCEL_YES_PRESSED 2)

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
  (display* "zotero-Application_getActiveDocument: " pv "\n")
  (list pv (zotero-getDocId)))

;; Shows an alert.
;;
;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;
(tm-define (zotero-Document_displayAlert documentID str_dialogText int_icon int_buttons)
  ;; stub
  (display* "zotero-Document_displayAlert: " documentID " " str_dialogText " "
            int_icon " " int_buttons "\n")
  0)


;; Brings the document to the foreground. (For OpenOffice, this is a no-op on non-Mac systems.)
;;
;; ["Document_activate", [documentID]] -> null
;;
(tm-define (zotero-Document_activate documentID)
  ;; stub?
  (display* "zotero-Document_activate: " documentID "\n")
  '())


;; Indicates whether a field can be inserted at the current cursor position.
;;
;; ["Document_canInsertField", [documentID]] -> boolean
(tm-define (zotero-Document_canInsertField documentID)
  ;; stub
  (display* "zotero-Document_canInsertField: " documentID "\n")
  #t)


;; Stores a document-specific persistent data string. This data
;; contains the style ID and other user preferences.
;;
;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;
(define zotero-document-data (make-ahash-table)) ;;; todo: store it in aux

(tm-define (zotero-Document_setDocumentData documentID str_dataString)
  ;; stub
  (display* "zotero-Document_setDocumentData: " documentID " " str_dataString "\n")
  (ahash-set! zotero-document-data documentID str_dataString)
  '())


;; Retrieves data string set by setDocumentData.
;;
;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;
(tm-define (zotero-Document_getDocumentData documentID)
  ;; stub: Todo: get from document aux
  (display* "zotero-Document_getDocumentData: " documentID "\n")
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
  (display* "zotero-Document_cursorInField: " documentID " " str_fieldType "\n")
  '())


(define NOTE_IN_TEXT 0)
(define NOTE_FOOTNOTE 1)
(define NOTE_ENDNOTE 2)

;; Inserts a new field at the current cursor position.
;;
;; str_fieldType, either "ReferenceMark" or "Bookmark"
;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;
;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]
;;
(tm-define (zotero-Document_insertField documentID str_fieldType int_noteType)
  ;; stub
  (display* "zotero-Document_insertField: " documentID " " str_fieldType " " int_noteType "\n")
  (list 0 "ReferenceMark" 0))


;; Get all fields present in the document, in document order.
;;
;; str_fieldType is the type of field used by the document, either ReferenceMark or Bookmark
;;
;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]
;;
(tm-define (zotero-Document_getFields documentID str_fieldType)
  ;; stub
  (display* "zotero-Document_getFields: " documentID " " str_fieldType "\n")
  (list (list 0) (list "ReferenceMark") (list 0)))


;; ["Document_convert" ??? (TODO in documentation.)
;;
(tm-define (zotero-Document_convert . args)
  ;; stub
  (display* "zotero-Document_convert: " args "\n")
  '())


;; ["Document_setBibliographyStyle", ??? (TODO)
;;
(tm-define (zotero-Document_setBibliographyStyle . args)
  ;; stub
  (display* "zotero-Document_setBibliographyStyle: " args "\n")
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
  (display* "zotero-Document_complete: " documentID "\n")
  (set! zotero-active? #f)
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
  (display* "zotero-Field_delete: " documentID " " fieldID "\n")
  '())


;; Moves the current cursor position to encompass a field.
;;
;; ["Field_select", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_select documentID fieldID)
  ;; stub
  (display* "zotero-Field_select: " documentID " " fieldID "\n")
  '())


;; Removes the field code from a field, leaving it as plain text.
;;
;; ["Field_removeCode", [documentID, fieldID]] -> null
;;
(tm-define (zotero-Field_removeCode documentID fieldID)
  ;; stub
  (display* "zotero-Field_removeCode: " documentID " " fieldID "\n")
  '())


;; Sets the (visible) text of a field.
;;
;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null
;;
(tm-define (zotero-Field_setText documentID fieldID str_text isRich)
  ;; stub
  (display* "zotero-Field_setText: " documentID " " fieldID " " str_text " "
            isRich "\n")
  '())


;; Gets the (visible) text of a field.
;;
;; ["Field_getText", [documentID, fieldID]] -> str_text
;;
(tm-define (zotero-Field_getText documentID fieldID)
  ;; stub
  (display* "zotero-Field_getText: " documentID " " fieldID "\n")
  "STUB Field Text STUB")


;; Sets the (hidden, persistent) code of a field.
;;
;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;
(tm-define (zotero-Field_setCode documentID fieldID str_code)
  ;; stub
  (display* "zotero-Field_setCode: " documentID " " fieldID " " str_code "\n")
  '())


;; Converts a field from one type to another.
;;
;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;; null
;;
(tm-define (zotero-Field_convert documentID fieldID str_fieldType int_noteType)
  ;; stub
  (display* "zotero-Field_convert: " documentID " " fieldID " " str_fieldType
            " " int_noteType "\n")
  '())
