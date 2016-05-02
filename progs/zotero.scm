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

(define zotero-transaction-id 0)

(define (get-next-zotero-transaction-id!)
  (set! zotero-transaction-id (+ 1 zotero-transaction-id))
  (if (> zotero-transaction-id 16383)
      (set! zotero-transaction-id 1))
  zotero-transaction-id)

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


(define (write-zotero id cmd)
  (let ((zp (get-zotero-socket-port!)))
    (catch 'system-error
      (lambda ()
        (let* ((cmdv (list->u8vector (map char->integer
                                          (string->list cmd))))
               (len (u8vector-length cmdv)))
          (write-network-u32 id zp)
          (write-network-u32 len zp)
          (uniform-vector-write cmdv zp)
          (force-output zp)))
      (lambda args
        (write args)
        (apply throw args)))))


(define (read-zotero)
  (let ((zp (get-zotero-socket-port!)))
    (catch 'system-error
      (lambda ()
        (let* ((id (read-network-u32 zp))
               (len (read-network-u32 zp))
               (cmdv (make-u8vector len 0)))
          (uniform-vector-read! cmdv zp)
          (list id len (list->string (map integer->char 
                                          (u8vector->list cmdv))))))
      (lambda args
        (write args)
        (apply throw args)))))



(define zotero-active? (make-ahash-table))

(define (zotero-add id)
  (ahash-set! zotero-active? id #t)
  (with wait 1
    (delayed
      (:while (ahash-ref zotero-active? id))
      (:pause ((lambda () (inexact->exact (round wait)))))
      (:do (set! wait (min (* 1.01 wait) 2500)))
      (with (id len cmdstr) (read-zotero)
        (when (> 0 len)
          ;;; now what?
          ;; (with (msg-id msg-cmd) (string->object msg)
          ;;   (client-eval (list server msg-id) msg-cmd)
          (with (editCommand args) (json-string->scm cmdstr)
            (with result (apply (string-append "zotero-" editCommand) (cons id args))
              (write-zotero id (scm->json-string result));;; !!! HERE !!! NOT
;;; DONE  Library closed!
            (ahash-remove! zotero->active? id)
            (set! wait 1)))))))



;; Integration commands: TeXmacs -> Zotero, no reply, Zotero connects back with
;; Editor commands.
;;
(define-macro (make-zotero-integration-command cmd)
  `(tm-define (,(string-append "zotero-" cmd))
     (write-zotero (get-next-zotero-transaction-id!) ,cmd)))

(let loop ((cmds '("addCitation"
                   "editCitation"
                   "addBibliography"
                   "editBibliography"
                   "refresh"
                   "removeCodes"
                   "setDocPrefs")))
     (cond
       ((null? cmds) #t)
       (#t (make-zotero-integration-command (car cmds))
           (loop (cdr cmds)))))


(define DIALOG_ICON_STOP 0)
(define DIALOG_ICON_NOTICE 1)
(define DIALOG_ICON_CAUTION 2)

(define DIALOG_BUTTONS_OK 0)
(define DIALOG_BUTTONS_OK_OK_PRESSED 1)

(define DIALOG_BUTTONS_OK_CANCEL 1)
(define DIALOG_BUTTONS_OK_CANCEL_CANCEL_PRESSED 0)
(define DIALOG_BUTTONS_OK_CANCEL_OK_PRESSED 1)

(define DIALOG_BUTTONS_YES_NO 2)
(define DIALOG_BUTTONS_YES_NO_NO_PRESSED 0)
(define DIALOG_BUTTONS_YES_NO_YES_PRESSED 1)

(define DIALOG_BUTTONS_YES_NO_CANCEL 3)
(define DIALOG_BUTTONS_YES_NO_CANCEL_CANCEL_PRESSED 0)
(define DIALOG_BUTTONS_YES_NO_CANCEL_NO_PRESSED 1)
(define DIALOG_BUTTONS_YES_NO_CANCEL_YES_PRESSED 2)

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
(define zotero-protocolVersion 3)

(define (zotero-Application-getActiveDocument id pv)
  (write-zotero id (format #f "[~s, ~s]" zotero-protocolVersion (zotero-getDocId))))

;; Shows an alert.
;;
;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed



;; Brings the document to the foreground. (For OpenOffice, this is a no-op on non-Mac systems.)
;;
;; ["Document_activate", [documentID]] -> null



;; Indicates whether a field can be inserted at the current cursor position.
;;
;; ["Document_canInsertField", [documentID]] -> boolean



;; Stores a document-specific persistent data string. This data
;; contains the style ID and other user preferences.
;;
;; ["Document_setDocumentData", [documentID, str_dataString]] -> null



;; Retrieves data string set by setDocumentData.
;;
;; ["Document_getDocumentData", [documentID]] -> str_dataString



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



(define NOTE_IN_TEXT 0)
(define NOTE_FOOTNOTE 1)
(define NOTE_ENDNOTE 2)

;; Inserts a new field at the current cursor position.
;;
;; str_fieldType, either "ReferenceMark" or "Bookmark"
;; int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
;;
;; ["Document_insertField", [documentID, str_fieldType, int_noteType]] -> [fieldID, fieldCode, int_noteIndex]



;; Get all fields present in the document, in document order.
;;
;; str_fieldType is the type of field used by the document, either ReferenceMark or Bookmark
;;
;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]



;; ["Document_convert" ??? (TODO in documentation.)



;; ["Document_setBibliographyStyle", ??? (TODO)



;; Indicates that the given documentID will no longer be used and
;; associated resources may be freed.
;;
;; ["Document_complete", [documentID]] -> null



;; Deletes a field from the document (both its code and its contents).
;;
;; fieldID as originally returned by Document_cursorInField,
;; Document_insertField, or Document_getFields.
;;
;; ["Field_delete", [documentID, fieldID]] -> null



;; Moves the current cursor position to encompass a field.
;;
;; ["Field_select", [documentID, fieldID]] -> null



;; Removes the field code from a field, leaving it as plain text.
;;
;; ["Field_removeCode", [documentID, fieldID]] -> null



;; Sets the (visible) text of a field.
;;
;; ["Field_setText", [documentID, fieldID, str_text, isRich]] -> null



;; Gets the (visible) text of a field.
;;
;; ["Field_getText", [documentID, fieldID]] -> str_text



;; Sets the (hidden, persistent) code of a field.
;;
;; ["Field_setCode", [documentID, fieldID, str_code]] -> null



;; Converts a field from one type to another.
;;
;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] -> null
