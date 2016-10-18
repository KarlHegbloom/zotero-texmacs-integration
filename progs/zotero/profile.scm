;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;;; MODULE      : zotero/profile.scm
;;; DESCRIPTION : Zotero Connector Plugin code profiling (development aid)
;;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;;
;;; This software falls under the GNU general public license version 3 or
;;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;;
;;;;
;;
;; Guile 2 has `statprof'... but Guile 1.8 does not and can't run it.

(use-modules (srfi srfi-1)) ;; for-each

(define-public zt-profile-funcs
  '(
    ;;x zt-shown-buffer-body-paragraphs
    as-string
    ;;x zt-get-DocumentID
    ;;x zt-get-new-fieldID
    ;;x zt-field-refbinding-key
    ;;x zt-get-refbinding
    zt-insert-new-field
    zt-find-zfield
    zt-go-to-zfield
    ;;x zt-zfield-ID ;; accessor (tree-ref t 0)
    ;;x zt-zfield-Code ;; accessor
    ;;zt-zfield-NoteIndex
    ;;x zt-zfield-Text ;; accessor (tree-ref t 2)
    zt-zfield-search
    zt-get-zfields-list ;; ? could be a slow one ?
    zt-get-zfield-Code-string
    zt-set-zfield-Code-from-string
    zt-parse-and-cache-zfield-Code ;; Called from zt-set-zfield-Code-from-string
    zt-get-zfield-Code-cache-ht-by-fieldID
    ;;x zt-ztbibItemRefs-ht-reset!
    zt-ztbibItemRefs-get-all-refs
    ;;x zt-ztbibItemRefs-get-zciteBibLabel
    ;;x zt-ztbibItemRefs-get-ztHrefFromCiteToBib-text
    ;;x zt-ztbibItemRefs-get-subcite-sysID
    zt-ztbibItemRefs-get-zfieldID
    zt-ztbibItemRefs-get-target-label
    uniq-equal?
    zt-ztbibItemRefs-cache-1-zbibItemRef
    zt-ztbibItemRefs-to-tree
    zt-ztbibItemRefs-parse-all
    zt-ext-ztbibItemRefsList
    zt-get-orig-zfield-Text
    zt-set-zfield-modified?!
    zt-zfield-modified?-or-undef
    zt-ext-flag-if-modified
    zt-ext-document-has-zbibliography?
    ;;
    zt-ext-ztShowID
    zt-ext-zbibCitationItemID
    zt-ext-bibitem
    zt-get-DocumentData
    zt-set-DocumentData
    zt-set-init-env-zotero-prefs
    close-zt-zotero-socket-port!
    ;;x set-nonblocking
    ;;x set-blocking
    ;;x get-logname
    ;;x zt-os-x-integration-pipe-locations
    get-zt-zotero-socket-port!
    write-network-u32
    read-network-u32
    zotero-write
    zotero-read
    safe-json-string->scm
    safe-scm->json-string
    zotero-listen ;; async, dispatcher.
    zt-call-zotero-integration-command
    ;;zotero-addCitation
    ;;zotero-editCitation
    ;;zotero-addBibliography
    ;;zotero-editBibliography
    ;;zotero-refresh
    ;;xUNDEF zotero-removeCodes
    ;;zotero-setDocPrefs
    ;;x notify-activated
    ;;x notify-disactivated
    ;;x parameter-show-in-menu?
    ;;x parameter-choice-list
    ;;x parameter-choice-list
    ;;x focus-tag-name
    ;;x customizable-parameters
    ;;x parameter-choice-list
    ;;x hidden-child?
    ;;x zt-notify-debug-trace
    ;;x update-document
    zotero-Application_getActiveDocument
    zotero-Document_displayAlert
    zotero-Document_activate
    zotero-Document_canInsertField
    zotero-Document_getDocumentData
    zotero-Document_setDocumentData
    zotero-Document_cursorInField
    zotero-Document_insertField
    zotero-Document_getFields
    zotero-Document_convert
    zotero-Document_setBibliographyStyle
    zotero-Document_cleanup
    zotero-Field_delete
    zotero-Field_select
    zotero-Field_removeCode
    zt-move-link-to-own-line
    zt-delete-one-space-to-left-of
    zt-fixup-embedded-slink-as-url
    zt-zotero-regex-transform
    zt-zotero-str_text->texmacs
    with-like-search
    zt-zfield-IsBib?
    zt-zfield-IsNote?
    zotero-Field_setText
    zotero-Field_getText
    zotero-Field_setCode
    zotero-Field_getCode
    zotero-Field_convert
    ))


(use-modules (ice-9 format))
(use-modules (ice-9 debugging traps))
(use-modules (ice-9 debugging trace))

;;; Default: (set-trace-layout "|~3@a: ~a\n" trace/stack-real-depth trace/info)
;;;
(set-trace-layout "|~a:~3@a: ~a\n"
                  trace/source
                  trace/stack-real-depth
                  trace/info)



(define calls (make-hash-table))
(define total (make-hash-table))


(define (calls+1! procname)
  (hash-set! calls procname
             (+ (hash-ref calls procname 0) 1)))


(define (total+! procname t)
  (hash-set! total procname
             (+ (hash-ref total procname 0) t)))


;; This does not function properly.
(define-public (make-accumulate-time-func procname)
  "Trap behavior: Measure the accumulated time spent in and below a specified procedure."
  `(lambda (trap-context)
     (calls+1! ,procname)
     (let ((entry (current-time)))
       (at-exit trap-context
                (lambda (ignored)
                  (total+! (- (current-time) entry)))))))

(define-public (time! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour (make-accumulate-time-func procname))))


(define-public (setup-time-profiling)
  (for-each (lambda (f)
              (time! f) zt-profile-funcs)))


(define-public (show-times)
  (let loop ((funcs zt-profile-funcs))
    (cond
      ((null? funcs) (newline))
      (else
        (let* ((f (car funcs))
               (s (symbol->string f)))
          (format (current-output-port) "~a:~a:~a\n"
            s
            (hash-ref calls s 0)
            (hash-ref total s 0))
          (loop (cdr funcs)))))))


(define-public (clear-times)
  (set! calls (make-hash-table))
  (set! total (make-hash-table)))



(define-public (break! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour gds-debug-trap)))


(define-public (trace! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour (list trace-trap
                                    trace-at-exit))))

(define-public (setup-tracing)
  (for-each trace! zt-profile-funcs))

(define-public (trace-subtree! proc)
  (install-trap (make <procedure-trap>
                  #:procedure proc
                  #:behaviour (list trace-trap
                                    trace-until-exit))))


