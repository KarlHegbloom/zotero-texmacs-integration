;;;;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;;; MODULE      : tm-zotero.scm
;;; DESCRIPTION : Zotero Connector Plugin
;;; COPYRIGHT   : (C) 2016, 2017  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;;
;;;;;;
;;;
;;; This software stands with the GNU general public license version 3 or
;;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;;
;;;;;;

;;{{{ Module definition and uses, routines from other parts of TeXmacs

;;;
;;; Save a few things to be sure not to lose them if they are needed.
;;;
(define-public tm-make make)            ; tm-defined in (utils library cpp-wrap)
(define guile-user:current-time current-time)
(define guile-user:color color)
(define cformat format)

;;;;;;
;;;
;;; Just to be sure that (a) these libraries are actually installed where this
;;; program is being used, and (b) that known working versions of them are
;;; installed, I have bundled them with tm-zotero. Within TeXmacs, these ones
;;; will shadow any that are also installed in Guile's own %load-path
;;; directories.
;;;
(texmacs-module (tm-zotero)
  (:use (oop goops)
        (oop goops accessors)
        (oop goops describe)            ; import (oop goops) first
        (kernel texmacs tm-modes)
        (kernel library content)
        (kernel library list)
        (utils base environment)
        (utils edit selections)
        (utils library cursor)
        (utils library tree)
        (generic document-edit)
        (text text-structure)
        (generic document-part)
        (generic document-style)
        (generic generic-edit)
        (generic format-edit)
        (convert tools sxml)
        (tm-zotero json)
        (srfi srfi-19)                  ; current-time, etc.
        (ice-9 format)
        (ice-9 regex)
        (ice-9 common-list)
        (ice-9 rw)
        (compat guile-2)
        (term ansi-color)
        (ice-9 optargs)
        ))


;;;;;;
;;;
;;; There has to be a way to define or set this via the normal prefs mechanism
;;; after guessing a default. For now, hard-code the magic location.
;;;
(define tm-zotero-csl-styles-base-directory
  "/home/karlheg/.juris-m/zotero/8l87vugc.default/zotero/styles")


;;;;;;
;;;
;;; This is not defined by TeXmacs since GOOPS is not used by it.
;;;
;;; Because <tree> is really just a <blackbox> there isn't any way to define
;;; other things that are really <blackbox> to have a distinct GOOPS type.
;;;
;;; <observer> is already magically defined. It is the type of a tree-pointer.
;;;
(define <tree> (class-of (tm->tree ""))) ; really a <blackbox> right now.


(define-method (make (tag <symbol>))
  (tm-make tag))

(define-method (make (tag <symbol>) (arity <integer>))
  (tm-make tag arity))


;;;;;;
;;;
;;; From (generic document-part):
;;;
;;; Perhaps these should be exported from there?
;;;
(define buffer-body-paragraphs (@@ (generic document-part) buffer-body-paragraphs))

;;;;;;
;;;
;;; (buffer-get-part-mode)
;;;   modes are:  :preamble :all :several :one
;;; (buffer-test-part-mode? mode)
;;;
(define buffer-get-part-mode (@@ (generic document-part) buffer-get-part-mode))
(define buffer-test-part-mode? (@@ (generic document-part) buffer-test-part-mode?))

;;;;;;
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

;;;;;;
;;;
;;; From: generic/format-edit.scm, not exported or tm-define'd there either.
;;;
(define (with-like-search t)
  (if (with-like? t) t
      (and (or (tree-atomic? t) (tree-in? t '(concat document)))
	   (and-with p (tree-ref t :up)
	     (with-like-search p)))))

(define wait-update-current-buffer (@@ (generic document-edit) wait-update-current-buffer))

;;}}}
;;{{{ Error and debugging printouts

;;;;;;
;;;
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

(define color-code-rx
  (map (lambda (elt)
         (cons (apply make-regexp `,(car elt))
               (cdr elt)))
       `((("(_BLACK_)")
          pre ,(color 'BLACK) post)
         (("(_BLINK_)")
          pre ,(color 'BLINK) post)
         (("(_BLUE_)")
          pre ,(color 'BLUE) post)
         (("(_BOLD_)")
          pre ,(color 'BOLD) post)
         (("(_CLEAR_)")
          pre ,(color 'CLEAR) post)
         (("(_CONCEALED_)")
          pre ,(color 'CONCEALED) post)
         (("(_CYAN_)")
          pre ,(color 'CYAN) post)
         (("(_DARK_)")
          pre ,(color 'DARK) post)
         (("(_GREEN_)")
          pre ,(color 'GREEN) post)
         (("(_MAGENTA_)")
          pre ,(color 'MAGENTA) post)
         (("(_ON-BLACK_)")
          pre ,(color 'ON-BLACK) post)
         (("(_ON-BLUE_)")
          pre ,(color 'ON-BLUE) post)
         (("(_ON-CYAN_)")
          pre ,(color 'ON-CYAN) post)
         (("(_ON-GREEN_)")
          pre ,(color 'ON-GREEN) post)
         (("(_ON-MAGENTA_)")
          pre ,(color 'ON-MAGENTA) post)
         (("(_ON-RED_)")
          pre ,(color 'ON-RED) post)
         (("(_ON-WHITE_)")
          pre ,(color 'ON-WHITE) post)
         (("(_ON-YELLOW_)")
          pre ,(color 'ON-YELLOW) post)
         (("(_RED_)")
          pre ,(color 'RED) post)
         (("(_RESET_)")
          pre ,(color 'RESET) post)
         (("(_REVERSE_)")
          pre ,(color 'REVERSE) post)
         (("(_UNDERLINE_)")
          pre ,(color 'UNDERLINE) post)
         (("(_UNDERSCORE_)")
          pre ,(color 'UNDERSCORE) post)
         (("(_WHITE_)")
          pre ,(color 'WHITE) post)
         (("(_YELLOW_)")
          pre ,(color 'YELLOW) post))))

(define color-code-strip-rx
  (map (lambda (elt)
         (cons (apply make-regexp `,(car elt))
               (cdr elt)))
       `((("(_BLACK_)") pre post)
         (("(_BLINK_)") pre post)
         (("(_BLUE_)") pre post)
         (("(_BOLD_)") pre post)
         (("(_CLEAR_)") pre post)
         (("(_CONCEALED_)") pre post)
         (("(_CYAN_)") pre post)
         (("(_DARK_)") pre post)
         (("(_GREEN_)") pre post)
         (("(_MAGENTA_)") pre post)
         (("(_ON-BLACK_)") pre post)
         (("(_ON-BLUE_)") pre post)
         (("(_ON-CYAN_)") pre post)
         (("(_ON-GREEN_)") pre post)
         (("(_ON-MAGENTA_)") pre post)
         (("(_ON-RED_)") pre post)
         (("(_ON-WHITE_)") pre post)
         (("(_ON-YELLOW_)") pre post)
         (("(_RED_)") pre post)
         (("(_RESET_)") pre post)
         (("(_REVERSE_)") pre post)
         (("(_UNDERLINE_)") pre post)
         (("(_UNDERSCORE_)") pre post)
         (("(_WHITE_)") pre post)
         (("(_YELLOW_)") pre post))))

(define tm-zotero-colorize? #t)

(define (colorize-format-string str)
  (let ((str str)
        (rc (if tm-zotero-colorize? color-code-rx color-code-strip-rx)))
    (do ((rc color-code-rx (cdr rc)))
        ((null? rc) str)
      ;; each is applied in turn, so later ones can modify results of earlier
      ;; ones if you like.
      (set! str (apply regexp-substitute/global `(#f ,(caar rc) ,str ,@(cdar rc)))))))


(define timestamp-format-string
  "_BOLD__GREEN_~9,,,'0@s_WHITE_:_GREEN_~9,,,'0@s_WHITE_:(_CYAN_~9,,,'0@s_WHITE_:_CYAN_~9,,,'0@s_WHITE_):_RESET_")

(define last-time (current-time))

(define (timestamp time)
  "@time is a time-utc as returned by srfi-19:current-time."
  (let* ((td (time-difference time last-time))
         (ret (format #f timestamp-format-string
                (time-second time)
                (time-nanosecond time)
                (time-second td)
                (time-nanosecond td))))
    (set! last-time time)
    ret))


(define (tm-zotero-format-error . args)
  (tm-errput
   (apply format (cons #f
                       (cons
                        (colorize-format-string
                         (string-concatenate
                          (list
                           "~&~!_RESET_"
                           (timestamp (current-time))
                           (car args)
                           "_RESET_\n")))
                        (cdr args))))))


(define zt-debug-trace? #f)

;;;;;;
;;;
;;; TODO Improve. Channels, plus I want to print a newline at the beginning of
;;;      a debug message, ahead of the timestamp, but only sometimes, like for
;;;      the first one inside of a function. There needs to be a flag for it I
;;;      guess.
;;;
(define (tm-zotero-format-debug . args)
  (when zt-debug-trace?
    (tm-output
     (apply format (cons #f
                         (cons
                          (colorize-format-string
                           (string-concatenate
                            (list
                             "~&~!_RESET_"
                             (timestamp (current-time))
                             (car args)
                             "_RESET_\n")))
                          (cdr args)))))))

;;}}}

;;;;;;
;;;
;;; Throughout this program:
;;;
;;;   zfield   is a texmacs tree.
;;;   zfieldID is a <string>.
;;;
;;;   symbol with suffix -t means texmacs tree.
;;;
;;;;;;

;;{{{ Misc. functions used within this program

(define (get-documentID)
  (url->string (current-buffer)))

(define (document-buffer documentID)
  (string->url documentID))

;;;;;;
;;;
;;; The set-binding call for the zfield noteIndex happens inside of the macro
;;; that renders the citation. After the citation zfield has been typeset once,
;;; the binding is set. Prior to then, it's not.
;;;
;;; This get-refbinding function can be used to retrieve the tree associated
;;; with any reference binding key. The key is a string.
;;;
(tm-define (get-refbinding key)
  (texmacs-exec `(get-binding ,key)))

;;;;;;
;;;
;;; The noteIndex reference binding keys must have deterministic format so the
;;; program can build them from data provided by Juris-M / Zotero. If this is
;;; ever changed, older documents might not work right.
;;;
(define (zfield-noteIndex-refbinding-key zfieldID)
  (string-append "zotero" zfieldID "-noteIndex"))


;;;;;;
;;;
;;; For "note" styles, this reference binding links a citation field with
;;; the footnote number that it appears in.
;;;
;;; See note at tm-zotero-Document_insertField regarding the necessity of
;;; letting the typesetter run in order for this reference binding to have a
;;; get-binding value (vs undefined).
;;;
;;; DONE When the document being editted is a book (or any one of a number of
;;;      styles that do this), the footnotes are numbered per-chapter, like 1.1
;;;      for the first footnote of the first chapter. The problem is that AFAIK
;;;      zotero / citeproc expects the noteIndex to be an integer! So 1.1
;;;      should be sent as 1001, 1.123 as 1123, 2.123 as 2123, 42.1 as 42001,
;;;      42.123 as 42123; 1.1.1 as 001001001, 12.1.123 as 012001123 (but trim
;;;      leading zeros). Does anyone ever really put more than 999 footnotes in
;;;      a chapter?
;;;
;;;      A result of this method that I find acceptible is that because a
;;;      near-note is output by citeproc for note styles only for anything
;;;      within 5 footnotes, near-notes will never reference back past a
;;;      chapter beginning in the book style.
;;;
;;; TODO If any style file defines the footnote numbering any differently, this
;;;      might not work right any longer... Actually, that happens in book
;;;      style when you have an Appendix that is labelled not by a number, but
;;;      by a letter.
;;;
(define (zfield-NoteIndex-t zfieldID-or-zfield)
  (let ((zfieldID (or (and (string? zfieldID-or-zfield)
                           zfieldID-or-zfield)
                      (zfield-zfieldID zfieldID-or-zfield))))
    ;; (tm-zotero-format-debug "zfield-NoteIndex-t: zfield => ~s, zfieldID => ~s"
    ;;                         (tree->stree zfield)
    ;;                         zfieldID)
    (get-refbinding
     (zfield-noteIndex-refbinding-key zfieldID))))

;;;;;;
;;;
;;; TODO This works fine for numbered chapters separated by a period. Maybe
;;;      there needs to be support inside the style sheets for the NoteIndex or
;;;      something?  Or just a better way to turn a footnote label into a
;;;      NoteIndex... maybe overloading or overriding the thing that increments
;;;      the footnote number?
;;;
(define (zfield-NoteIndex zfieldID-or-zfield)
  (let* ((NoteIndex-str (tree->stree (zfield-NoteIndex-t zfieldID-or-zfield)))
         (NoteIndex-parts (string-tokenize-by-char NoteIndex-str #\.)))
    (do ((ls NoteIndex-parts (cdr ls))
         (n 0 (+ (* n 1000) (string->number (car ls)))))
        ((null? ls) (number->string n)))))


;;;;;;
;;;
;;; There can be more than one sysID the same in a citation cluster, e.g., for
;;; parallel legal citations, or more than one reference to the same source,
;;; but to different pages or chapters, perhaps. When forming the
;;; ztbibItemRefsList trees for the bibliography entries, there's no point in
;;; linking back to the same citation cluster zfield more than once.
;;;
;;; Just for that, make sure the lists are uniq-equal? (since uniq uses memq,
;;; and this uses member, and we need to compare using equal? to make it
;;; recurse through list structure.
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


;;;;;;
;;;
;;; See: info:(guile-1.8) Guardians
;;;
(define tp-guardian (make-guardian))

(define (tp-guardian-after-gc)
  (do ((tp (tp-guardian) (tp-guardian)))
      ((and tp (observer? tp)))
    (when (and tp (observer? tp))
      (tree-pointer-detach tp)))
  #t)

;;;??? does not run at all now.
;(add-hook! after-gc-hook tp-guardian-after-gc)


;;;;;;
;;;
;;; tree-pointers are used when maintaining the list of zfields in
;;; document-order. This comparator is used to accomplish that.
;;;
(define (tree-pointer-less? tp1 tp2)
  (if (or (not tp1)
          (not tp2))
      #t
      (path-less? (tree->path (tree-pointer->tree tp1))
                  (tree->path (tree-pointer->tree tp2)))))


;;; <zfield-data> is defined below.
;;;
;;; This is effectively a generic function because tree-pointer is an accessor
;;; method in both the <zfield-data> and the <ztHrefFromCiteToBib-data>.
;;;
;;; This is used for keeping lists of them sorted in document order.
;;;
(define (<*-data>-less? z*d1 z*d2)
  (if (or (not z*d1)
          (not z*d2))
      #t
      (tree-pointer-less? (tree-pointer z*d1)
                          (tree-pointer z*d2))))



(define (inside-footnote? t)
  (not (not (tree-search-upwards t '(footnote zt-footnote)))))

(define (inside-endnote? t)
  (not (not (tree-search-upwards t '(endnote zt-endnote)))))

(define (inside-note? t)
  (or (in-footnote? t)
      (in-endnote? t)))



(define (inside-zcite? t)
  (not (not (tree-search-upwards t '(zcite)))))

(define (inside-zbibliography? t)
  (not (not (tree-search-upwards t '(zbibliography)))))

(define (inside-zfield? t)
  (not (not (tree-search-upwards t zfield-tags))))



(define (inside-inactive? t)
  (or (and (not (tree? t))
           #f)
      (and (tm-atomic? t)
           (== "+DISACTIVATED" (tree->stree t)))
      (tree-search-upwards t 'inactive)))


;;;;;;
;;;
;;; I had assumed that when a zfield is not inside of a shown part, that it
;;; would not be even seen by the typesetter... but now that I've learned
;;; otherwise, I can see that it makes sense, since if I have sections 1 and 4
;;; showing, at least the things that affect the environment of section 4 that
;;; happen during typestting of sections 2 and 3 must be evaluated by the
;;; typesetter. (e.g., footnote numbers, section numbers, potentially macro or
;;; redefinitions... Of course the preamble is usually hidden.)
;;;
;;; So tm-zotero-ext:ensure-zfield-interned! must look and see that the zfield
;;; it is ensuring the internment of is inside of a shown part first,
;;; otherwise, it does not need to intern it.
;;;
(define (inside-shown-part? t)
  (let ((mode (buffer-get-part-mode)))
    (cond                               ; don't return a <tree>, return <boolean>
      ((in? mode '(:one :several)) (not (not (tree-search-upwards t 'show-part))))
      ((in? mode '(:preamble)) #f)
      ((in? mode '(:all)) #t))))


;;;;;;
;;;
;;; Status line messages and system-wait messages.
;;;
;;;;;;

(define right-message "Juris-M / Zotero <---> TeXmacs Integration")

;;;;;;
;;;
;;; These strings might ultimately (inside the C++) end up going through a
;;; language translation table lookup, so I'm using the same strings that are
;;; found throughout the TeXmacs scheme sources. ☺
;;;
(define please-wait "please wait")
(define soon-ready "(soon ready)")

(define (prefix-message message)
  (string-append "tm-zotero: " message))


(define (tm-zotero-set-message message . timeout)
  (if (nnull? timeout)
      (set-temporary-message (prefix-message message) right-message (car timeout))
      (set-temporary-message (prefix-message message) right-message 1000)))

(define (tm-zotero-system-wait message arg)
  (system-wait (prefix-message message) arg))

(define (tm-zotero-set-message-and-system-wait message arg . timeout)
  (if (nnull? timeout)
      (tm-zotero-set-message message (car timeout))
      (tm-zotero-set-message message))
  (tm-zotero-system-wait message arg))

;;}}}

;;{{{ General category tm-define overloaded

;;;;;;
;;;
;;; update-document is defined at tmsrc://generic/document-edit.scm:341
;;;
(tm-define (update-document what)
  (:require (in-tm-zotero-style?))
  (delayed ;; allow typesetting/magic to happen before next update
    (:idle 1)
    (cursor-after
     (cond
       ((or (== what "all")
            (== what "bibliography"))
        (tm-zotero-refresh)
        (wait-update-current-buffer)))
     (former what))))


;;;;;;
;;;
;;; Whenever the document part mode is changed, clear the <document-data>. It
;;; will be regenerated automatically by the lazy interning during
;;; typesetting. Only the data for fields that are visible will be generated,
;;; so that it will be faster. That also means that no data in the hidden
;;; fields will be updated for things like the tm-zotero-refresh, which implies
;;; that once the entire document is shown again, that ought to be run
;;; (manually) by the user. That will happen during update-document, above.
;;;
(tm-define (buffer-set-part-mode mode)
  (:require (in-tm-zotero-style?))
  ;;(tm-zotero-format-debug "buffer-get-part-mode called, mode => ~s" mode)
  (clear-<document-data>! (get-documentID))
  (former mode)
  (wait-update-current-buffer))



;;;;;;
;;;
;;; Update the is-modified flag after the zfield is reactivated.
;;;
;;; The user can put the cursor at the end of a zcite and press Backspace to
;;; disactivate the tag, and then hand-edit the citation inside of it. When the
;;; tag is reactivated, the flag will turn red if the text was modified
;;; relative to what Zotero set it to.
;;;
(tm-define (notify-activated zfield)
  (:require (and (in-tm-zotero-style?)
                 (is-zfield? zfield)))
  ;; (tm-zotero-format-debug "notify-activated:called...")
  (tm-zotero-set-message "zcite reactivated! Checking for modification...")
  (let* ((origText (zfield-Code-origText zfield))
         (newText  (zfield-Text zfield))
         (is-modified? (if (string=? newText origText) "false" "true")))
    ;; (tm-zotero-format-debug "notify-activated: origText => ~s" origText)
    ;; (tm-zotero-format-debug "notify-activated: newText => ~s" newText)
    (set! (zfield-Code-is-modified?-flag zfield) is-modified?)
    (tm-zotero-set-message
     (string-append "zcite reactivated! Checking for modification... is-modified? => "
                    is-modified? ". Done.")))
  (wait-update-current-buffer)
  #t)


;;;;;;
;;;
;;; NOTE Some examples of how clipboard-cut and clipboard-paste can be
;;;      overloaded are in fold-edit.scm.
;;;
;;;;;;
;;;
;;; clipboard-cut must maintain the list of zfields that is built by
;;; tm-zotero-ext:ensure-zfield-interned!, which is called by the typesetter
;;; each time a zfield is encountered.
;;;

;;;;;;
;;;
;;; This is flag that will be set while the clipboard-cut operation is taking
;;; place, in an attempt to try and prevent the ztHrefFromCiteToBib fields
;;; inside of a selection about to be cut from being interned again right after
;;; uninterning them just prior to actually cutting the text out of the
;;; document.
;;;
(define inside-tm-zotero-clipboard-cut #f)

(define (unintern-ztHrefFromCiteToBib-for-cut documentID zfield)
  ;;(tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:_GREEN_called..._RESET_")
  (let ((zhd-ht (get-document-ztbibItemRefs-ht documentID))
        (zfieldID (zfield-zfieldID zfield))
        (ztHref*-ls (tm-search
                     zfield
                     (cut tm-func? <> 'ztHrefFromCiteToBib))))
    (map (lambda (ztHref*)
           ;; (tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:  _GREEN_map lambda_WHITE_:ztHref* => ~s_RESET_"
           ;;                         (tree->stree ztHref*))
           (let* ((sysID (ztHref*-sysID ztHref*))
                  (ref-label (ztHrefFromCiteToBib-reflabel zfieldID sysID))
                  (zhd (hash-ref zhd-ht ref-label #f)))
             (if zhd
                 (begin
                   ;; (tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:  _GREEN_map lambda_WHITE_:_GREEN_removing zhd with ref-label: _BOLD_~s_RESET_" ref-label)
                   ;; (tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:  _GREEN_map lambda_RESET_:ls before: ~s"
                   ;;                         (map (lambda (z)
                   ;;                                (ztHrefFromCiteToBib-reflabel (the-zfieldID-of z) (the-sysID-of z)))
                   ;;                              (hash-ref zhd-ht sysID '())))
                   (hash-set! zhd-ht sysID
                              (list-filter
                               (hash-ref zhd-ht sysID '())
                               (lambda (z)
                                 (not (eq? zhd z)))))
                   ;; (tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:  _GREEN_map lambda_RESET_:ls after: ~s"
                   ;;                         (map (lambda (z)
                   ;;                                (ztHrefFromCiteToBib-reflabel (the-zfieldID-of z) (the-sysID-of z)))
                   ;;                              (hash-ref zhd-ht sysID '())))
                   (clear-tree-pointer zhd)
                   (hash-remove! zhd-ht ref-label))
                 (begin
                   ;; (tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:  _GREEN_map lambda_WHITE_:_BOLD__RED_no zhd with ref-label _GREEN_~s_RESET_" ref-label)
                   #f))))
         ztHref*-ls))
  ;; (tm-zotero-format-debug "_CYAN_unintern-ztHrefFromCiteToBib-for-cut_WHITE_:_GREEN_returning._RESET_")
  )


(tm-define (clipboard-copy which)
  (:require (and (in-tm-zotero-style?)
                 (in-text?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (has-zfields? (selection-tree))))
  (with-fluids
      ((fluid/is-during-tm-zotero-clipboard-cut? #t))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-copy_WHITE_:_GREEN_called_RESET_")
    (let* ((documentID (get-documentID))
           (zfd-ht (get-document-zfield-zfd-ht documentID))
           (zfd-ls (get-document-zfield-zfd-ls documentID))
           (zb-zfd-ls (get-document-zbibliography-zfd-ls documentID))
           (new-zfield-zfd (get-document-new-zfield-zfd documentID))
           (selection-t (selection-tree))
           (copy-t (tree-copy selection-t))
           (c-zfields (tm-search copy-t is-zfield?)))
      (map (lambda (zfield)
           (let* ((zfieldID (zfield-zfieldID zfield))
                  (zfd (hash-ref zfd-ht zfieldID #f)))
             (cond
               ((and zfd (not (eq? zfd new-zfield-zfd)))
                ;;(tm-zotero-format-debug "clipboard-copy:zfd => ~s (~s)" zfd zfieldID)
                ;; (hash-remove! zfd-ht zfieldID)
                ;; (set-document-zfield-zfd-ls! documentID
                ;;                              (list-filter zfd-ls
                ;;                                           (lambda (elt)
                ;;                                             (not (eq? elt zfd)))))
                ;; (when (is-zbibliography? zfield)
                ;;   ;;(tm-zotero-format-debug "clipboard-copy:is-zbibliography? => #t")
                ;;   (set-document-zbiblioraphy-zfd-ls!
                ;;    (list-filter zb-zfd-ls
                ;;                 (lambda (elt)
                ;;                   (not (eq? elt zfd))))))
                ;; TODO Experiment + UTSL to find out if I need to do this
                ;;      with tree-pointer. I am not sure if it sticks to the
                ;;      tree while the tree is detached, or if it gets left
                ;;      at the place where the selection tree has been cut
                ;;      from. Also is it GC'd, or would it leak?
                ;; (clear-tree-pointer zfd)
                (unintern-ztHrefFromCiteToBib-for-cut documentID zfield))
               (zfd
                (let ((tp (tree-pointer zfd))) ; is the new one???
                  ;;
                  ;; How? This can happen only when the protocol between
                  ;; Zotero and TeXmacs has failed for some reason, usually
                  ;; due to a bug in this program causing it to be
                  ;; interrupted, leaving the new-zfield in the document.
                  ;;
                  (tm-zotero-format-error
                   "_BOLD__RED_clipboard-copy_RESET_: _RED_Copying new zfield!_RESET_ _BOLD__RED_Fixme:_RESET_ Probably protocol breakdown; Restart Firefox and TeXmacs.")
                  (tree-assign zfield
                               (stree->tree
                                '(strong "{?? New Citation ??}")))
                  (clear-tree-pointer zfd)
                  (set-document-new-zfield-zfd! #f))))))
         c-zfields)
      (clipboard-set which copy-t))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-copy_WHITE_:_GREEN_returning._RESET_")
    ))


(tm-define (clipboard-cut which)
  (:require (and (in-tm-zotero-style?)
                 (in-text?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (has-zfields? (selection-tree))))
  (with-fluids
      ((fluid/is-during-tm-zotero-clipboard-cut? #t))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-cut_WHITE_:_GREEN_called_RESET_, which => ~s" which)
    (let* (;;(dummy (tm-zotero-format-debug "clipboard-cut:documentID..."))
           (documentID (get-documentID))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:zfd-ht..."))
           (zfd-ht (get-document-zfield-zfd-ht documentID))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:zfd-ls..."))
           (zfd-ls (get-document-zfield-zfd-ls documentID))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:zb-zfd-ls..."))
           (zb-zfd-ls (get-document-zbibliography-zfd-ls documentID))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:new-zfield-zfd..."))
           (new-zfield-zfd (get-document-new-zfield-zfd documentID))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:selection-t..."))
           (selection-t (selection-tree))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:selection-t => ~s" (tree->stree selection-t)))
           ;;(dummy (tm-zotero-format-debug "clipboard-cut:zfields..."))
           (zfields (tm-search selection-t is-zfield?)))
      (map (lambda (zfield)
             (let* ((zfieldID (zfield-zfieldID zfield))
                    (zfd (hash-ref zfd-ht zfieldID #f)))
               (cond
                 ((and zfd (not (eq? zfd new-zfield-zfd)))
                  ;;(tm-zotero-format-debug "clipboard-cut:zfd => ~s (~s)" zfd zfieldID)
                  (hash-remove! zfd-ht zfieldID)
                  (set-document-zfield-zfd-ls! documentID
                                               (list-filter zfd-ls
                                                            (lambda (elt)
                                                              (not (eq? elt zfd)))))
                  (when (is-zbibliography? zfield)
                    ;;(tm-zotero-format-debug "clipboard-cut:is-zbibliography? => #t")
                    (set-document-zbiblioraphy-zfd-ls! documentID
                                                       (list-filter zb-zfd-ls
                                                                    (lambda (elt)
                                                                      (not (eq? elt zfd))))))
                  ;; TODO Experiment + UTSL to find out if I need to do this
                  ;;      with tree-pointer. I am not sure if it sticks to the
                  ;;      tree while the tree is detached, or if it gets left
                  ;;      at the place where the selection tree has been cut
                  ;;      from. Also is it GC'd, or would it leak?
                  (clear-tree-pointer zfd)
                  (unintern-ztHrefFromCiteToBib-for-cut documentID zfield))
                 (zfd
                  (let ((tp (tree-pointer zfd))) ; is the new one???
                    ;;
                    ;; How? This can happen only when the protocol between
                    ;; Zotero and TeXmacs has failed for some reason, usually
                    ;; due to a bug in this program causing it to be
                    ;; interrupted, leaving the new-zfield in the document.
                    ;;
                    (tm-zotero-format-error
                     "_BOLD__RED_clipboard-cut_RESET_: _RED_Cutting new zfield!_RESET_ _BOLD__RED_Fixme:_RESET_ Probably protocol breakdown; Restart Firefox and TeXmacs.")
                    (tree-assign zfield
                                 (stree->tree
                                  '(strong "{?? New Citation ??}")))
                    (clear-tree-pointer zfd)
                    (set-document-new-zfield-zfd! #f))))))
           zfields)
      (cpp-clipboard-cut "none")
      (when (not (== which "none"))
        (clipboard-set which selection-t)))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-cut_WHITE_:_GREEN_returning._RESET_")
    ))


;;;;;;
;;;
;;; When pasting in a tree that contains any zfields, it is important to give
;;; each zfield a new zfieldID. Otherwise, Zotero will change every one of them
;;; that has the same id to have the same text!
;;;
;;; This does not need to do anything to maintain the in-document-order list of
;;; zfields, since tm-zotero-ext:ensure-zfield-interned! will automatically
;;; pick it up after this returns, and the typesetter finds the newly pasted
;;; zfield(s).
;;;
(tm-define (clipboard-paste which)
  (:require (and (in-tm-zotero-style?)
                 (not (focus-is-zfield?))
                 (in-text?)
                 (has-zfields? (clipboard-get which))))
  ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-paste_WHITE_:_GREEN_called..._RESET_")
  (let ((clipboard-t (clipboard-get which)))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-paste_WHITE_:  _GREEN_before_RESET_: _BOLD__YELLOW_clipboard-t =>_RESET_\n~y"
    ;;                         (tree->stree clipboard-t))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-paste_WHITE_:  _GREEN_before_RESET_: _BOLD__YELLOW_zfieldID's =>_RESET_ ~s"
    ;;                         (map (lambda (zfield) (zfield-zfieldID zfield))
    ;;                              (tm-search clipboard-t is-zfield?)))
    ;;(insert (tree-ref clipboard-t 1) 1)
    (insert (tree-ref clipboard-t 1) 0) ;; ? cursor in front after ?
    (map (lambda (zfield)
           (set! (zfield-zfieldID zfield) (get-new-zfieldID)))
         (tm-search clipboard-t is-zfield?))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-paste_WHITE_:  _GREEN_after_RESET_: _BOLD__YELLOW_zfieldID's =>_RESET_ ~s"
    ;;                         (map (lambda (zfield) (zfield-zfieldID zfield))
    ;;                              (tm-search clipboard-t is-zfield?)))
    ;; (tm-zotero-format-debug "_BOLD__RED_clipboard-paste_RESET_:  _GREEN_after_RESET_: _BOLD__YELLOW_clipboard-t =>_RESET_\n~y"
    ;;                         (tree->stree clipboard-t))
    ))


;;}}}


;;{{{ Preferences and Settings (with-like, global, document-wide)

(texmacs-modes
  (in-tm-zotero-style% (style-has? "tm-zotero-dtd"))
  (focus-is-zcite% (tree-is? (focus-tree) 'zcite) in-tm-zotero-style%)
  (focus-is-zbibliography% (tree-is? (focus-tree) 'zbibliography) in-tm-zotero-style%)
  (focus-is-zfield% (or (focus-is-zcite?) (focus-is-zbibliography?)))
  (focus-is-ztHref% (tree-is? (focus-tree) 'ztHref) in-tm-zotero-style%))

;;;
;;; TODO Invent a good naming convention for the below preferences and
;;;      settings... There must be a differentiation between editor-wide
;;;      preferences, document-wide ones, and ones that have either an explicit
;;;      or implicit document-wide default that can be overrided locally by
;;;      using a with-wrapper. Further, there are some that are not to be
;;;      exposed to the end user, and others that are.
;;;
;;;  Idea: Make ones that are to be hidden have a special naming convention to
;;;        make it easier to implement the below functions which are used to
;;;        determine what to show in the toolbar menus.
;;;
;;; TODO See (utils base environment), extend that logic-table with the ones
;;;      for this? Can those be contextually overloaded? I guess it doesn't
;;;      matter. It's just a variable identifier to description string mapping.
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
  (tm-zotero-set-message (string-append "Setting zt-debug-trace? to " val "."))
  (set! zt-debug-trace? (== val "on")))


(define-preferences
  ("zt-debug-trace?" "off" zt-notify-debug-trace))

;;; these need to be per-document preferences, not TeXmacs-wide ones.
  ;; ("zt-pref-in-text-hrefs-as-footnotes"         "on"  ignore)
  ;; ("zt-pref-in-text-hlinks-have-href-footnotes" "on"  ignore))

;;}}}


;;;;;;
;;;
;;; This handles the DocumentData that is saved inside the document, but is
;;; (mostly) managed by Juris-M / Zotero. It is the data that is set when you
;;; select the “Zotero > Set Document Prefs” menu item. The same dialog is also
;;; automatically presented for a fresh document when the DocumentData has not
;;; been set yet.
;;;
;;{{{ DocumentData (from Zotero, saved, parsed -> document initial environment

;;;;;;
;;;
;;; AFAIK the only pref that this program needs access to is noteType, and that
;;; access is read-only. The noteType is a document-wide setting, since it goes
;;; with the CSL stylesheet chosen... But it is also passed to
;;; Document_insertField, Document_convert (?), and Field_convert, so really it
;;; could be a per-field setting...?  I choose to make it document-wide. I
;;; suspect that the reason it's like that is that the protocol is only evolved
;;; to it's version 3 form... Hmmm. Maybe sometimes it makes sense to have some
;;; of them be an endnote and others be a footnote, while still others can be
;;; inline? Not today.
;;;
;;;   enum noteType
;;;
(define-public zotero-NOTE_IN_TEXT  0)
(define-public zotero-NOTE_FOOTNOTE 1)
(define-public zotero-NOTE_ENDNOTE  2)

;;;;;;
;;;
;;; The rest of the DocumentData settings are "opaque" from the viewpoint of
;;; this interface. They control Zotero, not TeXmacs.
;;;
;;; The DocumentData is meant to be saved with the document, and so it is
;;; stored inside the document's initial variable value resolution environment.
;;;
;;; All of them are set via the zotero controlled dialog. That dialog is
;;; displayed automatically when the document does not yet have
;;; zoteroDocumentData set, because at the start of the transaction, Zotero
;;; will call tm-zotero-Document_getDocumentData, which returns null to Zotero
;;; unless it's been set. After setting it, the next thing Zotero sends is a
;;; tm-zotero-Document_setDocumentData message. It can also be invoked by
;;; sending a tm-zotero-setDocPrefs message, which will call
;;; tm-zotero-Document_getDocumentData, then let you edit that in Zotero's
;;; dialog, and send it back with tm-zotero-Document_setDocumentData. So from
;;; here, we never need to write the prefs by any means other than having
;;; Zotero set it.
;;;
;;; TODO Perhaps a future iteration could provide initial hints based on the
;;;      language of the document being editted? But that's sort of a global
;;;      thing anyway, and setting the language takes only a few clicks.
;;;
;;; TODO I think that the LibreOffice plugin (java) actually parses and
;;;      re-emits the xml that this program is parsing below... If the xml sent
;;;      back is acceptable to Zotero, then this can be used to set some of
;;;      those settings via the editor (client side) interface. Maybe there are
;;;      some kinds of things that it makes more sense to have the interface to
;;;      changing the setting or whatever be on the client side?  Or maybe I
;;;      could use it to sneak information into a monkey-patch in
;;;      propachi-texmacs... But since I've got control of both sides via the
;;;      ability to monkey-patch... I may as well work out a few extensions to
;;;      the protocol (with it in mind that other editor client programs may
;;;      want to utilize the same wire protocol or output formats).
;;;
;;;   tm-Guile: (get-env "zotero-pref-noteType")
;;;               returns a <string>
;;;         or: (texmacs-exec '(value "zotero-pref-noteType"))
;;;               returns a <tree>
;;;
;;;   TeXmacs: <value|zotero-pref-noteType>
;;;
;;{{{ sample sxml representation of DocumentData
;;;;;
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
;;}}}

(define (get-env-zoteroDocumentData)
  (get-env "zoteroDocumentData"))

(define (set-env-zoteroDocumentData! str_dataString)
  (set-init-env "zoteroDocumentData" str_dataString)
  (set-init-env-for-zotero-document-prefs str_dataString))


(define (set-init-env-for-zotero-document-prefs str_dataString)
  (let ((set-init-env-for-zotero-document-prefs-sub
         (lambda (prefix attr-list)
           (let loop ((attr-list attr-list))
             (cond
               ((null? attr-list) #t)
               (else
                 (let ((var (symbol->string (caar attr-list))))
                   (set-init-env (string-append prefix var)
                                 (cadar attr-list))
                   (when (and (== prefix "zotero-style-")
                              (== var "id"))
                     (let ((psd (tm-zotero-get-citation-layout-prefix-delimiter-suffix (cadar attr-list))))
                       (set-init-env (string-append prefix "citation-layout-prefix") (car psd))
                       (set-init-env (string-append prefix "citation-layout-delimiter") (cadr psd))
                       (set-init-env (string-append prefix "citation-layout-suffix") (caddr psd))))
                   (loop (cdr attr-list)))))))))
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
           ;;;
           ;; The TeXmacs style language case statements can not test an
           ;; environment variable that is a string against any other
           ;; string... the string it's set to has to be "true" or "false"
           ;; to make boolean tests work. It can not check for "equals 0",
           ;; "equals 1", etc.
           ;;;
           (set-init-env "zotero-pref-noteType0" "false")
           (set-init-env "zotero-pref-noteType1" "false")
           (set-init-env "zotero-pref-noteType2" "false")
           (set-init-env (string-append "zotero-pref-noteType"
                                        (sxml-attr (car sxml) 'value)) "true"))
         (loop (cdr sxml)))))))


;; zotero-style-id => "http://juris-m.github.io/styles/jm-indigobook-in-text"

;; To split a zcite field, find out the citation layout separator and suffix.
;; Only split where the "formattedCitation" has separator characters, but know
;; that for legal citations, when there are parallel citations, the split of
;; the "citationItems" array must happen according to the count of
;; \ztHrefFromCiteToBib{#zbibSysIDnnnn}, so that the multiple ones prior to the
;; separator become part of the first new zcite and the remainder become part
;; of the second new zcite, splitting the original one on the separator
;; character.
;;
;; The examples below are for a single zcite citation cluster with a parallel
;; legal citation with two publication sources followed by a citation to a
;; book. The first is with the "Store references in document" pref turned off,
;; and the second with it turned on, adding an "itemData" element to each of
;; the "citationItems". For the purposes of splitting a zcite field into two,
;; or merging two of them into one, the contents of each of the "citationItems"
;; is irrelevant; all that matters as that the "citationItems" appear in the
;; same order and number as the \ztHrefFromCiteToBib do in the
;; "formattedCitation", and that a zcite is splittable only at the locations of
;; the separator character in the "formattedCitation".
;;
;; {
;;     "properties" :
;;     {
;;         "plainCitation" : "(concat (zttextit (concat (ztHrefFromCiteToBib \"#zbibSysID3187\" \"https://scholar.google.com/scholar_case?case=13669261267335843060\" \"Adam\") \"s \" (abbr \"v.\") \" State\")) \", 2005 UT \" (ztHrefFromCiteToBib \"#zbibSysID3187\" \"https://scholar.google.com/scholar_case?case=13669261267335843060\" \"62\") \", 123 \" (ztHrefFromCiteToBib \"#zbibSysID3564\" \"https://scholar.google.com/scholar_case?case=13669261267335843060\" \"P.3d\") \" 400 (2005); \" (zttextsc (concat (ztHrefFromCiteToBib \"#zbibSysID1711\" \"\" \"Chri\") \"stopher \" (abbr \"E.\") \" Smith\")) \", \" (zttextsc \"Courts and Trials: A Reference Handbook\") \" (Contemporary World Issues, 2003).\")",
;;         "formattedCitation" : "{\\rtf \\zttextit{\\ztHrefFromCiteToBib{#zbibSysID3187}{\\path{https://scholar.google.com/scholar\\_case?case=13669261267335843060}}{Adam}s v. State}, 2005 UT \\ztHrefFromCiteToBib{#zbibSysID3187}{\\path{https://scholar.google.com/scholar\\_case?case=13669261267335843060}}{62}, 123 \\ztHrefFromCiteToBib{#zbibSysID3564}{\\path{https://scholar.google.com/scholar\\_case?case=13669261267335843060}}{P.3d} 400 (04#@UtahUtahX-X-X 01#@Sup. Ct.Sup. Ct.X-X-X 2005); \\zttextsc{\\ztHrefFromCiteToBib{#zbibSysID1711}{\\path{\\ztDefaultCiteURL}}{Chri}stopher E. Smith}, \\zttextsc{Courts and Trials: A Reference Handbook} (Contemporary World Issues, 2003).}"
;;     },
;;     "citationID" : "1Sku5kh8",
;;     "citationItems" : [
;;         {
;;             "uris" : ["http://zotero.org/users/226074/items/3XE57ZJ9"],
;;             "id" : 3187,
;;             "uri" : ["http://zotero.org/users/226074/items/3XE57ZJ9"]
;;         },
;;         {
;;             "uris" : ["http://zotero.org/users/226074/items/EQFFTEMX"],
;;             "id" : 3564,
;;             "uri" : ["http://zotero.org/users/226074/items/EQFFTEMX"]
;;         },
;;         {
;;             "uris" : ["http://zotero.org/users/226074/items/CN4MK7FX"],
;;             "id" : 1711,
;;             "uri" : ["http://zotero.org/users/226074/items/CN4MK7FX"]
;;         }]
;; }
;;
;; {
;;     "properties" :
;;     {
;;         "plainCitation" : "(concat (zttextit (concat \"Adams \" (abbr \"v.\") \" State\")) \", 2005 UT \" (ztHrefFromCiteToBib \"#zbibSysID3187\" \"https://scholar.google.com/scholar_case?case=13669261267335843060\" \"62\") \", 123 \" (ztHrefFromCiteToBib \"#zbibSysID3564\" \"https://scholar.google.com/scholar_case?case=13669261267335843060\" \"P.3d\") \" 400 (2005); \" (zttextsc (concat (ztHrefFromCiteToBib \"#zbibSysID1711\" \"\" \"Chri\") \"stopher \" (abbr \"E.\") \" Smith\")) \", \" (zttextsc \"Courts and Trials: A Reference Handbook\") \" (Contemporary World Issues, 2003).\")",
;;         "formattedCitation" : "{\\rtf \\zttextit{Adams v. State}, 2005 UT \\ztHrefFromCiteToBib{#zbibSysID3187}{\\path{https://scholar.google.com/scholar\\_case?case=13669261267335843060}}{62}, 123 \\ztHrefFromCiteToBib{#zbibSysID3564}{\\path{https://scholar.google.com/scholar\\_case?case=13669261267335843060}}{P.3d} 400 (04#@UtahUtahX-X-X 01#@Sup. Ct.Sup. Ct.X-X-X 2005); \\zttextsc{\\ztHrefFromCiteToBib{#zbibSysID1711}{\\path{\\ztDefaultCiteURL}}{Chri}stopher E. Smith}, \\zttextsc{Courts and Trials: A Reference Handbook} (Contemporary World Issues, 2003).}"
;;     },
;;     "citationID" : "LgFZYRcC",
;;     "schema" : "https://github.com/citation-style-language/schema/raw/master/csl-citation.json",
;;     "citationItems" : [
;;         {
;;             "itemData" :
;;             {
;;                 "issued" :
;;                 {
;;                     "raw" : "September 23, 2005"
;;                 },
;;                 "type" : "legal_case",
;;                 "authority" : "Supreme Court",
;;                 "page" : "62",
;;                 "volume" : "2005",
;;                 "note" : "mlzsync1:0054{\"type\":\"case\",\"extrafields\":{\"jurisdiction\":\"us:ut\"}}",
;;                 "title" : "Adams v. State",
;;                 "URL" : "https://scholar.google.com/scholar_case?case=13669261267335843060",
;;                 "container-title" : "UT"
;;             },
;;             "uris" : ["http://zotero.org/users/226074/items/3XE57ZJ9"],
;;             "id" : 3187,
;;             "uri" : ["http://zotero.org/users/226074/items/3XE57ZJ9"]
;;         },
;;         {
;;             "itemData" :
;;             {
;;                 "issued" :
;;                 {
;;                     "raw" : "September 23, 2005"
;;                 },
;;                 "type" : "legal_case",
;;                 "authority" : "Supreme Court",
;;                 "page" : "400",
;;                 "volume" : "123",
;;                 "note" : "mlzsync1:0054{\"type\":\"case\",\"extrafields\":{\"jurisdiction\":\"us:ut\"}}{:jurisdiction: Utah}",
;;                 "title" : "Adams v. State",
;;                 "URL" : "https://scholar.google.com/scholar_case?case=13669261267335843060",
;;                 "container-title" : "P. 3d"
;;             },
;;             "uris" : ["http://zotero.org/users/226074/items/EQFFTEMX"],
;;             "id" : 3564,
;;             "uri" : ["http://zotero.org/users/226074/items/EQFFTEMX"]
;;         },
;;         {
;;             "itemData" :
;;             {
;;                 "author" : [
;;                     {
;;                         "given" : "",
;;                         "isInstitution" : 1,
;;                         "family" : "Christopher E. Smith"
;;                     }],
;;                 "issued" :
;;                 {
;;                     "raw" : "2003"
;;                 },
;;                 "type" : "book",
;;                 "publisher" : "ABC-CLIO",
;;                 "collection-title" : "Contemporary World Issues",
;;                 "ISBN" : "978-1-57607-933-1",
;;                 "note" : "00005",
;;                 "title" : "Courts and Trials: A Reference Handbook"
;;             },
;;             "uris" : ["http://zotero.org/users/226074/items/CN4MK7FX"],
;;             "id" : 1711,
;;             "uri" : ["http://zotero.org/users/226074/items/CN4MK7FX"]
;;         }]
;; }
;;

(define (no-error-string-load str)
  (catch #t
    (lambda ()
      (string-load str))
    (lambda args
      #f)))

(define (tm-zotero-get-style-as-sxml style-id)
  (let* ((style-id (if (string-index style-id (lambda (c) (eqv? c #\/)))
                       (substring style-id
                                  (string-index-right style-id
                                                      (lambda (c) (eqv? c #\/)))
                                  (string-length style-id))
                       style-id))
         (style-file-url (string-append tm-zotero-csl-styles-base-directory "/" style-id ".csl")))
    (let* ((str (no-error-string-load style-file-url))
           (sxml (or (and str (parse-xml str))
                     '())))
      ;;(tm-zotero-format-debug "tm-zotero-get-style-as-sxml:style-id => ~s" style-id)
      ;;(tm-zotero-format-debug "tm-zotero-get-style-as-sxml:sxml =>\n~s" sxml)
      ;;(tm-zotero-format-debug "tm-zotero-get-style-as-sxml:(car sxml) =>\n~s" (cadr sxml))
      sxml)))


(define (tm-zotero-get-citation-layout-prefix-delimiter-suffix-from-csl style-id)
  (let loop ((sxml (tm-zotero-get-style-as-sxml style-id)))
    ;;(tm-zotero-format-debug "_GREEN_tm-zotero-get-citation-layout-prefix-delimiter-suffix_RESET_: top of outer loop, (car sxml) => ~s" (car sxml))
    (cond
      ((null? sxml) '("" "" ""))        ; default
      ((or (string? (car sxml))
           (and (pair? (car sxml))
                (eq? (caar sxml) '*PI*)
                (eq? (caar sxml) 'bibliography)
                (eq? (caar sxml) 'info)
                (eq? (caar sxml) 'locale)
                (eq? (caar sxml) 'macro)
                (eq? (caar sxml) '@))
           (eq? (car sxml) '*TOP*))
       (loop (cdr sxml)))
      ((and (pair? (car sxml))
            (eq? (caar sxml) 'style))
       ;;(tm-zotero-format-debug "_BOLD__GREEN_tm-zotero-get-citation-layout-prefix-delimiter-suffix_RESET_: found style, (cdar sxml) => ~s" (cdar sxml))
       (loop (cdar sxml)))
      ((eq? (caar sxml) 'citation)
       ;;(tm-zotero-format-debug "_BOLD__GREEN_tm-zotero-get-citation-layout-prefix-delimiter-suffix_RESET_: found citation, (cdar sxml) => ~s" (cdar sxml))
       (loop (cdar sxml)))
      ((eq? (caar sxml) 'layout)
       ;;(tm-zotero-format-debug "_BOLD__GREEN_tm-zotero-get-citation-layout-prefix-delimiter-suffix_RESET_: _BOLD__YELLOW_found citation layout_RESET_, @ => (cadar sxml) => ~s" (cadar sxml))
       (let innerloop ((@ (or (and (pair? (cadar sxml))
                                   (eq? (caadar sxml) '@)
                                   (cadar sxml))
                              '()))
                       (prefix "")
                       (delimiter "")
                       (suffix ""))
         ;;(tm-zotero-format-debug "_BOLD__GREEN_tm-zotero-get-citation-layout-prefix-delimiter-suffix_RESET_: top of innerloop, @ => ~s" @)
         (cond
           ((null? @) (list prefix delimiter suffix))
           ((or (eq? (car @) '@)
                (string? (car @)))
            (innerloop (cdr @) prefix delimiter suffix))
           ((and (pair? (car @))
                 (eq? (caar @) 'prefix))
            (innerloop (cdr @) (cadar @) delimiter suffix))
           ((and (pair? (car @))
                 (eq? (caar @) 'delimiter))
            (innerloop (cdr @) prefix (cadar @) suffix))
           ((and (pair? (car @))
                 (eq? (caar @) 'suffix))
            (innerloop (cdr @) prefix delimiter (cadar @)))
           (else
             (innerloop (cdr @) prefix delimiter suffix)))))
      (else
        (loop (cdr sxml))))))


(define (tm-zotero-get-cache-of-citation-layout-prefix-delimiter-suffix)
  (map (lambda (style-id)
         (cons style-id
               (tm-zotero-get-citation-layout-prefix-delimiter-suffix-from-csl style-id)))
       (map symbol->string
            '(academy-of-management-review acm-sigchi-proceedings acm-sigchi-proceedings-extended-abstract-format acm-siggraph acm-sig-proceedings acm-sig-proceedings-long-author-list acs-nano acta-anaesthesiologica-scandinavica acta-anaesthesiologica-taiwanica acta-naturae acta-neurochirurgica acta-ophthalmologica acta-palaeontologica-polonica acta-pharmaceutica acta-pharmaceutica-sinica-b acta-philosophica acta-polytechnica acta-psychiatrica-scandinavica acta-societatis-botanicorum-poloniae acta-universitatis-agriculturae-sueciae administrative-science-quarterly advanced-engineering-materials advanced-functional-materials advanced-materials advances-in-alzheimers-disease advances-in-complex-systems african-journal-of-emergency-medicine african-zoology aging-cell aging aids aix-marseille-universite-departement-d-etudes-asiatiques alexandria-journal-of-medicine allergology-international allergy alternatives-to-animal-experimentation ambio ameghiniana american-anthropological-association american-association-for-cancer-research american-association-of-petroleum-geologists american-chemical-society-author-date american-chemical-society american-chemical-society-page-first american-chemical-society-with-titles american-chemical-society-with-titles-doi-no-et-al american-chemical-society-with-titles-no-et-al american-chemical-society-with-titles-page-first american-chemical-society-with-titles-sentence-case american-chemical-society-with-titles-sentence-case-doi american-fisheries-society american-geophysical-union american-heart-association american-institute-of-aeronautics-and-astronautics american-institute-of-physics american-journal-of-agricultural-economics american-journal-of-archaeology american-journal-of-botany american-journal-of-climate-change american-journal-of-clinical-pathology american-journal-of-epidemiology american-journal-of-health-behavior american-journal-of-medical-genetics american-journal-of-neuroradiology american-journal-of-orthodontics-and-dentofacial-orthopedics american-journal-of-plant-sciences american-journal-of-political-science american-journal-of-primatology american-journal-of-respiratory-and-critical-care-medicine american-journal-of-science american-journal-of-surgical-pathology american-journal-of-translational-research american-marketing-association american-medical-association-alphabetical american-medical-association american-medical-association-no-et-al american-medical-association-no-url american-meteorological-society american-mineralogist american-physics-society american-physiological-society american-phytopathological-society american-political-science-association american-society-for-microbiology american-society-for-pharmacology-and-experimental-therapeutics american-society-of-civil-engineers american-society-of-mechanical-engineers american-sociological-association american-veterinary-medical-association amphibia-reptilia anabases analytical-sciences anesthesia-and-analgesia anesthesiology angewandte-chemie angiologia animal-migration animal-welfare annalen-des-naturhistorischen-museums-in-wien annales annals-of-applied-biology annals-of-biomedical-engineering annals-of-botany annals-of-neurology annals-of-oncology annals-of-the-association-of-american-geographers annual-review-of-astronomy-and-astrophysics annual-review-of-medicine annual-review-of-nuclear-and-particle-science annual-reviews-alphabetical annual-reviews-author-date annual-reviews annual-reviews-without-titles antarctic-science anticancer-research antiquity apa-5th-edition apa-annotated-bibliography apa apa-cv apa-fr-provost apa-fr-universite-de-montreal apa-no-ampersand apa-no-doi-no-issue apa-old-doi-prefix apa-single-spaced apa-tr applied-spectroscopy aquatic-conservation aquatic-invasions aquatic-living-resources arachne arachnology archaeometry archeologie-medievale archives-of-physical-medicine-and-rehabilitation archiv-fur-die-civilistische-praxis archiv-fur-geschichte-der-philosophie archivos-de-la-sociedad-espanola-de-oftalmologia archivum-latinitatis-medii-aevi arctic-antarctic-and-alpine-research artery-research art-history arthritis-and-rheumatism arzneimitteltherapie asa-cssa-sssa asaio-journal asia-and-the-pacific-policy-studies asian-studies-review associacao-brasileira-de-normas-tecnicas associacao-brasileira-de-normas-tecnicas-eceme associacao-brasileira-de-normas-tecnicas-ipea associacao-brasileira-de-normas-tecnicas-note associacao-brasileira-de-normas-tecnicas-ufjf associacao-brasileira-de-normas-tecnicas-ufmg-face-full associacao-brasileira-de-normas-tecnicas-ufmg-face-initials associacao-brasileira-de-normas-tecnicas-ufpr associacao-brasileira-de-normas-tecnicas-ufrgs associacao-brasileira-de-normas-tecnicas-ufs associacao-brasileira-de-normas-tecnicas-usp-fmvz associacao-nacional-de-pesquisa-e-ensino-em-transportes association-de-science-regionale-de-langue-francaise association-for-computational-linguistics association-for-computing-machinery atencion-primaria ausonius-editions austral-ecology australian-critical-care australian-guide-to-legal-citation australian-historical-studies australian-journal-of-earth-sciences australian-journal-of-grape-and-wine-research australian-veterinary-journal austrian-legal avian-conservation-and-ecology avian-diseases avian-pathology aviation-space-and-environmental-medicine babes-bolyai-university-faculty-of-orthodox-theology beltz-padagogik bibtex bioarchaeology-international bioarchaeology-of-the-near-east biochemical-journal biochemical-society-transactions biochemistry biochimica-et-biophysica-acta bioelectromagnetics bioessays bioinformatics biologia biological-and-pharmaceutical-bulletin biological-journal-of-the-linnean-society biological-psychiatry biological-reviews biology-of-reproduction biomed-central biomed-research-international biometrics bioorganic-and-medicinal-chemistry-letters biophysical-journal biopolymers bioresources biosocieties biotechniques biotechnology-and-bioengineering biotropica biuletyn-polskiego-towarzystwa-jezykoznawczego blood bluebook2 bluebook-inline bluebook-law-review bmj body-and-society boreal-environment-research brain brazilian-journal-of-infectious-diseases briefings-in-bioinformatics british-ecological-society british-journal-of-anaesthesia british-journal-of-cancer british-journal-of-dermatology british-journal-of-haematology british-journal-of-industrial-relations british-journal-of-pharmacology british-journal-of-political-science british-journal-of-surgery budownictwo-i-architektura-pl building-structure bulletin-de-la-societe-entomologique-de-france bulletin-de-la-societe-prehistorique-francaise bulletin-of-faculty-of-pharmacy-cairo-university bulletin-of-marine-science byzantina-symmeikta cahiers-d-ethnomusicologie cahiers-du-centre-gustave-glotz california-agriculture campus-adventiste-du-saleve-faculte-adventiste-de-theologie canadian-geotechnical-journal canadian-journal-of-dietetic-practice-and-research canadian-journal-of-earth-sciences canadian-journal-of-economics canadian-journal-of-fisheries-and-aquatic-sciences canadian-journal-of-physics canadian-journal-of-public-health canadian-journal-of-soil-science canadian-public-policy carcinogenesis cardiocore catholic-biblical-association cell cell-numeric cell-research cell-transplantation cellular-and-molecular-bioengineering cellular-reprogramming centaurus centre-de-recherche-sur-les-civilisations-de-l-asie-orientale cerebral-cortex ceska-zemedelska-univerzita-v-praze-fakulta-agrobiologie-potravinovych-a-prirodnich-zdroju changer-d-epoque chemical-and-pharmaceutical-bulletin chemical-senses chest chicago-annotated-bibliography chicago-author-date-basque chicago-author-date chicago-author-date-de chicago-author-date-fr chicago-figures chicago-fullnote-bibliography chicago-fullnote-bibliography-fr chicago-fullnote-bibliography-no-ibid chicago-library-list chicago-note-bibliography chicago-note-biblio-no-ibid chimia chinese-gb7714-1987-numeric chinese-gb7714-2005-author-date chinese-gb7714-2005-numeric chinese-journal-of-aeronautics chroniques-des-activites-archeologiques-de-l-ecole-francaise-de-rome circulation cirugia-cardiovascular cirugia-espanola cladistics clara-architecture-recherche clay-minerals clays-and-clay-minerals clinica-e-investigacion-en-arteriosclerosis clinical-gastroenterology-and-hepatology clinical-hemorheology-and-microcirculation clinical-infectious-diseases clinical-journal-of-sport-medicine clinical-nuclear-medicine clinical-orthopaedics-and-related-research clinical-otolaryngology clinical-pharmacology-and-therapeutics clio-medica cns-and-neurological-disorders-drug-targets cold-spring-harbor-laboratory-press collection-de-l-ecole-francaise-de-rome-full-note collection-de-l-ecole-francaise-de-rome-note collection-du-centre-jean-berard collections-electroniques-de-l-inha-author-date collections-electroniques-de-l-inha-full-note college-montmorency colombian-journal-of-anesthesiology comision-economica-para-america-latina-y-el-caribe communication-et-langages comparative-population-studies computer-und-recht conservation-biology conservation-letters conservation-physiology copeia copernicus-publications coral-reefs corrosion council-of-science-editors-alphabetical council-of-science-editors-author-date council-of-science-editors cranfield-university-numeric creativity-and-innovation-management critical-care-medicine cuadernos-de-filologia-clasica cultivos-tropicales cultural-studies-of-science-education culture-medicine-and-psychiatry current-gene-therapy current-opinion current-pharmaceutical-design current-proteomics current-protocols currents-in-biblical-research current-topics-in-medicinal-chemistry cytometry database data-science-journal de-buck decision-sciences demographic-research der-moderne-staat deutsche-gesellschaft-fur-psychologie deutsche-medizinische-wochenschrift deutsches-archaologisches-institut deutsche-sprache development-policy-review diagnostico-prenatal dialisis-y-trasplante diatom-research die-bachelorarbeit-samac-et-al-in-text die-bachelorarbeit-samac-et-al-note din-1505-2-alphanumeric din-1505-2 din-1505-2-numeric-alphabetical din-1505-2-numeric diplo disability-and-rehabilitation discovery-medicine dna-research documents-d-archeologie-francaise drug-development-research drugs-of-today drug-testing-and-analysis ear-and-hearing early-christianity early-medieval-europe earthquake-engineering-and-structural-dynamics earth-surface-processes-and-landforms ecclesial-practices ecole-pratique-des-hautes-etudes-sciences-historiques-et-philologiques ecological-entomology ecology-and-society ecology ecology-letters ecology-of-freshwater-fish economic-commission-for-latin-america-and-the-caribbean economie-et-statistique ecoscience ecosistemas ecosystems elementa el-profesional-de-la-informacion elsevier-harvard2 elsevier-harvard elsevier-harvard-without-titles elsevier-vancouver elsevier-without-titles elsevier-with-titles-alphabetical elsevier-with-titles embo-reports emerald-harvard emu-austral-ornithology endocrine-press endocrinologia-y-nutricion endoscopia eneuro enfermeria-clinica enfermeria-intensiva engineering-in-life-sciences ens-de-lyon-centre-d-ingenierie-documentaire entomologia-experimentalis-et-applicata entomological-society-of-america environmental-and-engineering-geoscience environmental-chemistry environmental-conservation environmental-health-perspectives environmental-microbiology environmental-toxicology-and-chemistry environment-and-planning environment-and-urbanization environnement-risques-et-sante epidemiologie-et-sante-animale equine-veterinary-education equine-veterinary-journal ergoscience escuela-nacional-de-antropologia-e-historia-author-date escuela-nacional-de-antropologia-e-historia-full-note escuela-nacional-de-antropologia-e-historia-short-note ethics-book-reviews ethnobiology-and-conservation ethnologie-francaise ets-ecole-de-technologie-superieure europace european-cells-and-materials european-journal-of-clinical-microbiology-and-infectious-diseases european-journal-of-emergency-medicine european-journal-of-endocrinology european-journal-of-human-genetics european-journal-of-immunology european-journal-of-information-systems european-journal-of-international-law european-journal-of-neuroscience european-journal-of-ophthalmology european-journal-of-paediatric-neurology european-journal-of-pain european-journal-of-political-research european-journal-of-soil-science european-journal-of-surgical-oncology european-journal-of-ultrasound european-journal-of-vascular-and-endovascular-surgery european-respiratory-journal european-retail-research european-society-of-cardiology european-union-interinstitutional-style-guide evidence-based-complementary-and-alternative-medicine evolution-and-development evolutionary-anthropology evolutionary-ecology-research evolution exercer experimental-dermatology eye fachhochschule-kiel-fachbereich-medien fachhochschule-vorarlberg-author-date fachhochschule-vorarlberg-note facial-plastic-surgery-clinics-of-north-america family-business-review ferdinand-porsche-fern-fachhochschule fertility-and-sterility finanzarchiv fine-focus first-monday fish-and-fisheries flavour-and-fragrance-journal florida-entomologist foerster-geisteswissenschaft fold-and-r food-and-agriculture-organization-of-the-united-nations forensic-science-review forest-science free-radical-research freie-universitat-berlin-geographische-wissenschaften french1 french2 french3 french4 french-politics freshwater-biology freshwater-science friedrich-schiller-university-jena-faculty-of-medicine frontiers frontiers-in-ecology-and-the-environment frontiers-in-optics frontiers-in-physics frontiers-medical-journals future-science-group g3 gallia gastroenterology gastrointestinal-endoscopy-clinics-of-north-america gastrointestinal-intervention geistes-und-kulturwissenschaften-heilmann geneses genes-to-cells genetics-and-molecular-biology genetics genome-biology-and-evolution geoarchaeology geobiology geochemical-perspectives-letters geochimica-et-cosmochimica-acta geochronometria geografie-sbornik-cgs geological-magazine geology geopolitics georg-august-universitat-gottingen-institut-fur-ethnologie-und-ethnologische-sammlung gesellschaft-fur-popularmusikforschung gewerblicher-rechtsschutz-und-urheberrecht global-change-biology global-ecology-and-biogeography glossa gost-r-7-0-5-2008 gost-r-7-0-5-2008-numeric-alphabetical gost-r-7-0-5-2008-numeric grasas-y-aceites hamburg-school-of-food-science hand harvard1 harvard7de harvard-anglia-ruskin-university harvard-bournemouth-university harvard-cape-peninsula-university-of-technology harvard-cardiff-university-biosi harvard-cardiff-university harvard-cite-them-right harvard-coventry-university harvard-cranfield-university harvard-deakin-university harvard-de-montfort-university harvard-dublin-city-university harvard-dundalk-institute-of-technology harvard-durham-university-business-school harvard-edge-hill-university harvard-european-archaeology harvard-fachhochschule-salzburg harvard-gesellschaft-fur-bildung-und-forschung-in-europa harvard-imperial-college-london harvard-institut-fur-praxisforschung-de harvard-kings-college-london harvard-leeds-beckett-university harvard-leeds-metropolitan-university harvard-limerick harvard-london-south-bank-university harvard-manchester-business-school harvard-manchester-metropolitan-university harvard-melbourne-polytechnic harvard-newcastle-university harvard-north-west-university harvard-oxford-brookes-university-faculty-of-health-and-life-sciences harvard-pontificia-universidad-catolica-del-ecuador harvard-southampton-solent-university harvard-staffordshire-university harvard-stellenbosch-university harvard-swinburne-university-of-technology harvard-theologisches-seminar-adelshofen harvard-the-university-of-melbourne harvard-the-university-of-northampton harvard-the-university-of-sheffield-school-of-east-asian-studies harvard-the-university-of-sheffield-town-and-regional-planning harvard-universiti-teknologi-malaysia harvard-universiti-tunku-abdul-rahman harvard-university-for-the-creative-arts harvard-university-of-abertay-dundee harvard-university-of-bath harvard-university-of-birmingham harvard-university-of-brighton-school-of-environment-and-technology harvard-university-of-cape-town harvard-university-of-exeter-geography harvard-university-of-gloucestershire harvard-university-of-greenwich harvard-university-of-kent harvard-university-of-leeds harvard-university-of-sunderland harvard-university-of-technology-sydney harvard-university-of-the-west-of-england harvard-university-of-the-west-of-scotland harvard-university-of-westminster harvard-university-of-wolverhampton harvard-university-of-worcester harvard-york-st-john-university haute-ecole-pedagogique-fribourg hawaii-international-conference-on-system-sciences-proceedings health-and-social-care-in-the-community health-economics health-economics-policy-and-law health-education-research health-policy-and-planning health-reform-observer-observatoire-des-reformes-de-sante health-services-research heart-failure-clinics heart-rhythm hematology-oncology-clinics-of-north-america hepatology heredity herpetologica high-altitude-medicine-and-biology hiob-ludolf-centre-for-ethiopian-studies hiob-ludolf-centre-for-ethiopian-studies-with-url-doi hipertension-y-riesgo-vascular histoire-at-politique histoire-et-mesure histopathology history-and-theory history-of-the-human-sciences hochschule-fur-wirtschaft-und-recht-berlin hong-kong-journal-of-radiology human-brain-mapping human-mutation human-reproduction human-reproduction-update human-resource-management-journal human-wildlife-interactions humboldt-state-university-environmental-resources-engineering hydrobiologia hydrological-processes hydrological-sciences-journal hypertension-research hypotheses-in-the-life-sciences ices-journal-of-marine-science idojaras-quarterly-journal-of-the-hungarian-meteorological-service ie-comunicaciones ieee ieee-with-url igaku-toshokan iica-catie im-gesprach immunological-reviews indian-journal-of-medical-research indoor-air infectio infectious-disease-clinics-of-north-america inflammatory-bowel-diseases infoclio-de infoclio-fr-nocaps infoclio-fr-smallcaps information-systems-journal ingenieria-agricola institute-for-operations-research-and-the-management-sciences institute-of-physics-harvard institute-of-physics-numeric institut-national-de-la-recherche-scientifique-sciences-sociales institut-national-de-sante-publique-du-quebec-napp institut-national-de-sante-publique-du-quebec-topo instituto-de-pesquisas-tecnologicas instituto-superior-de-teologia-de-las-islas-canarias institut-teknologi-bandung-tesis-magister interaction-design-and-architectures interdisziplinare-zeitschrift-fur-technologie-und-lernen international-atomic-energy-agency international-brazilian-journal-of-urology international-conference-on-information-systems-development international-development-policy international-journal-of-audiology international-journal-of-cancer international-journal-of-climatology international-journal-of-electronic-commerce international-journal-of-epidemiology international-journal-of-exercise-science international-journal-of-food-science-and-technology international-journal-of-geriatric-psychiatry international-journal-of-humanoid-robotics international-journal-of-infectious-diseases international-journal-of-lexicography international-journal-of-nuclear-security international-journal-of-obstetric-anesthesia international-journal-of-occupational-medicine-and-environmental-health international-journal-of-oral-and-maxillofacial-surgery international-journal-of-osteoarchaeology international-journal-of-plant-sciences international-journal-of-quantum-chemistry international-journal-of-radiation-oncology-biology-physics international-journal-of-spatial-data-infrastructures-research international-journal-of-sports-medicine international-journal-of-wildland-fire international-labour-organization international-microbiology international-organization international-pig-veterinary-society-congress-proceedings international-studies-association international-union-of-crystallography inter-research-science-center inter-ro invertebrate-biology investigative-radiology invisu ios-press-books irish-historical-studies iso690-author-date-cs iso690-author-date-en iso690-author-date-es iso690-author-date-fr iso690-author-date-fr-no-abstract iso690-author-date-sk iso690-full-note-sk iso690-note-cs iso690-note-fr iso690-numeric-brackets-cs iso690-numeric-cs iso690-numeric-en iso690-numeric-fr iso690-numeric-lt iso690-numeric-sk israel-medical-association-journal ithaque ius-ecclesiae jacc-cardiovascular-imaging jahrbuch-der-osterreichischen-byzantinischen-gesellschaft jahrbuch-fur-evangelikale-theologie javnost-the-public jm-chicago-fullnote-bibliography jm-chicago-fullnote-bibliography-polyglot jm-chinese-gb7714-2005-numeric jm-diritto-pubblico-comparato-ed-europeo jm-harvard-australian-national-university jm-indigobook-catsort-bib jm-indigobook jm-indigobook-law-review jm-oscola jm-taylor-and-francis-chicago-author-date jm-turabian-fullnote-bibliography-eu-multi jm-turabian-fullnote-bibliography-nl-multi journal-for-the-history-of-astronomy journal-fur-kunstgeschichte journalistica journal-of-adolescent-health journal-of-aerosol-medicine-and-pulmonary-drug-delivery journal-of-alzheimers-disease journal-of-analytical-toxicology journal-of-animal-physiology-and-animal-nutrition journal-of-animal-science journal-of-antimicrobial-chemotherapy journal-of-applied-animal-science journal-of-applied-clinical-medical-physics journal-of-applied-ecology journal-of-applied-entomology journal-of-applied-philosophy journal-of-archaeological-research journal-of-atrial-fibrillation journal-of-avian-biology journal-of-basic-microbiology journal-of-biogeography journal-of-biological-chemistry journal-of-biomedical-materials-research-part-a journal-of-bone-and-mineral-research journal-of-brachial-plexus-and-peripheral-nerve-injury journal-of-burn-care-and-research journal-of-business-logistics journal-of-cardiothoracic-and-vascular-anesthesia journal-of-cellular-and-molecular-medicine journal-of-chemistry-and-chemical-engineering journal-of-chemometrics journal-of-child-and-adolescent-psychopharmacology journal-of-clinical-oncology journal-of-clinical-rheumatology journal-of-clinical-sleep-medicine journal-of-combinatorics journal-of-common-market-studies journal-of-computational-chemistry journal-of-consumer-research journal-of-crohns-and-colitis journal-of-crohns-and-colitis-supplements journal-of-dairy-science journal-of-dental-research journal-of-elections-public-opinion-and-parties journal-of-endodontics journal-of-european-public-policy journal-of-evolutionary-biology journal-of-experimental-botany journal-of-field-ornithology journal-of-finance journal-of-fish-biology journal-of-fish-diseases journal-of-food-protection journal-of-forensic-sciences journal-of-frailty-and-aging journal-of-genetic-engineering-and-biotechnology journal-of-geriatric-psychiatry-and-neurology journal-of-glaciology journal-of-hearing-science journal-of-hospital-infection journal-of-human-evolution journal-of-hypertension journal-of-industrial-and-engineering-chemistry journal-of-industrial-ecology journal-of-infection journal-of-infectious-diseases journal-of-information-technology journal-of-institutional-and-theoretical-economics journal-of-instrumentation journal-of-integrated-omics journal-of-interactive-marketing journal-of-internal-medicine journal-of-international-business-studies journal-of-international-economic-law journal-of-investigative-dermatology journal-of-korean-neurosurgical-society journal-of-lipid-research journal-of-magnetic-resonance-imaging journal-of-mammalogy journal-of-management journal-of-management-information-systems journal-of-materials-research journal-of-mechanical-science-and-technology journal-of-medical-genetics journal-of-medical-internet-research journal-of-minimally-invasive-gynecology journal-of-molecular-endocrinology journal-of-morphology journal-of-music-technology-and-education journal-of-nanoscience-and-nanotechnology journal-of-natural-history journal-of-neurochemistry journal-of-neurological-disorders journal-of-neurophysiology journal-of-neuroscience-and-neuroengineering journal-of-neurosurgery journal-of-neurotrauma journal-of-nutrition journal-of-oral-and-maxillofacial-surgery journal-of-orthopaedic-research journal-of-orthopaedics-trauma-and-rehabilitation journal-of-orthopaedic-trauma journal-of-paleontology journal-of-peace-research journal-of-peptide-science journal-of-perinatal-medicine journal-of-petrology journal-of-pharmacy-and-pharmacology journal-of-phycology journal-of-physical-therapy-science journal-of-plant-nutrition-and-soil-science journal-of-pollination-ecology journal-of-polymer-science-part-a-polymer-chemistry journal-of-product-innovation-management journal-of-psychiatric-and-mental-health-nursing journal-of-psychiatry-and-neuroscience journal-of-reconstructive-microsurgery journal-of-retailing journal-of-rheumatology journal-of-roman-archaeology-a journal-of-roman-archaeology-b journal-of-science-and-medicine-in-sport journal-of-separation-science journal-of-shoulder-and-elbow-surgery journal-of-simulation journal-of-sleep-research journal-of-social-archaeology journal-of-spinal-disorders-and-techniques journal-of-sport-and-health-science journal-of-strength-and-conditioning-research journal-of-studies-on-alcohol-and-drugs journal-of-systematic-palaeontology journal-of-the-air-and-waste-management-association journal-of-the-american-academy-of-audiology journal-of-the-american-academy-of-orthopaedic-surgeons journal-of-the-american-association-of-laboratory-animal-science journal-of-the-american-ceramic-society journal-of-the-american-college-of-cardiology journal-of-the-american-college-of-surgeons journal-of-the-american-society-of-brewing-chemists journal-of-the-american-society-of-nephrology journal-of-the-american-water-resources-association journal-of-the-association-for-information-systems journal-of-the-brazilian-chemical-society journal-of-the-electrochemical-society journal-of-thermal-spray-technology journal-of-the-royal-anthropological-institute journal-of-thrombosis-and-haemostasis journal-of-tropical-ecology journal-of-tropical-life-science journal-of-universal-computer-science journal-of-urban-and-environmental-engineering journal-of-vegetation-science journal-of-vertebrate-paleontology journal-of-visualized-experiments journal-of-water-sanitation-and-hygiene-for-development journal-of-wildlife-diseases journal-of-zoo-and-wildlife-medicine journal-of-zoo-biology journal-of-zoology juristische-schulung juristische-zitierweise karger-journals-author-date karger-journals kidney-international kidney-research-and-clinical-practice kindheit-und-entwicklung knee-surgery-sports-traumatology-arthroscopy kolner-zeitschrift-fur-soziologie-und-sozialpsychologie kommunikation-und-recht korean-journal-of-anesthesiology kritische-ausgabe kth-royal-institute-of-technology-school-of-computer-science-and-communication kth-royal-institute-of-technology-school-of-computer-science-and-communication-sv land-degradation-and-development landes-bioscience-journals language language-in-society latin-american-perspectives latin-american-research-review law-and-society-review leidraad-voor-juridische-auteurs le-mouvement-social les-journees-de-la-recherche-avicole les-journees-de-la-recherche-porcine le-tapuscrit-author-date le-tapuscrit-note lethaia lettres-et-sciences-humaines-fr leviathan limnology-and-oceanography liver-international lluelles lluelles-no-ibid london-south-bank-university-numeric lund-university-school-of-economics-and-management macromolecular-reaction-engineering magnetic-resonance-in-medicine magnetic-resonance-materials-in-physics-biology-and-medicine maison-de-l-orient-et-de-la-mediterranee mammal-review manchester-university-press mastozoologia-neotropical mathematics-and-computers-in-simulation mcdonald-institute-monographs mcgill-en mcgill-fr medecine-sciences media-culture-and-society medical-dosimetry medical-history medical-physics medicinal-research-reviews medicine-and-science-in-sports-and-exercise medicinskiy-akademicheskiy-zhurnal melbourne-school-of-theology memorias-do-instituto-oswaldo-cruz mercatus-center metallurgical-and-materials-transactions meteoritics-and-planetary-science meteorological-applications methods-in-ecology-and-evolution methods-of-information-in-medicine metropolitiques microbial-drug-resistance microscopy-and-microanalysis middle-east-critique mind-and-language mineralogical-magazine mis-quarterly modern-humanities-research-association-author-date modern-humanities-research-association modern-language-association-6th-edition-note modern-language-association-8th-edition modern-language-association modern-language-association-underline modern-language-association-with-url modern-phytomorphology mohr-siebeck-recht molecular-and-cellular-proteomics molecular-biology-and-evolution molecular-biology-of-the-cell molecular-ecology molecular-metabolism molecular-microbiology molecular-plant-microbe-interactions molecular-plant-pathology molecular-psychiatry-letters molecular-therapy mondes-en-developpement moore-theological-college moorlands-college multidisciplinary-digital-publishing-institute multiple-sclerosis-journal mutagenesis mycologia myrmecological-news nano-biomedicine-and-engineering national-archives-of-australia national-institute-of-health-research national-institute-of-technology-karnataka national-library-of-medicine-grant-proposals national-science-foundation-grant-proposals nature nature-neuroscience-brief-communications nature-no-et-al nature-no-superscript nature-publishing-group-vancouver navigation nccr-mediality nehet nephrology-dialysis-transplantation neue-juristische-wochenschrift neuroimaging-clinics-of-north-america neurologia-argentina neurologia neurology neurology-india neuropsychopharmacology neurorehabilitation-and-neural-repair neuroreport neurosurgery-clinics-of-north-america new-harts-rules-the-oxford-style-guide new-phytologist new-solutions new-testament-studies new-zealand-plant-protection new-zealand-veterinary-journal nordic-pulp-and-paper-research-journal norma-portuguesa-405 northeastern-naturalist nowa-audiofonologia nuclear-receptor-signaling nucleic-acids-research obesity occupational-medicine oikos oncotarget onderstepoort-journal-of-veterinary-research ophthalmology optometry-and-vision-science oral-diseases organization ornitologia-neotropical orthopedic-clinics-of-north-america oryx oscola oscola-no-ibid osterreichische-zeitschrift-fur-politikwissenschaft owbarth-verlag oxford-art-journal oxford-centre-for-mission-studies-harvard oxford-studies-in-ancient-philosophy oxford-studies-on-the-roman-economy oxford-the-university-of-new-south-wales oxford-university-press-humsoc oxford-university-press-scimed-author-date oxford-university-press-scimed-numeric padagogische-hochschule-fachhochschule-nordwestschweiz padagogische-hochschule-heidelberg padagogische-hochschule-vorarlberg pain palaeontologia-electronica palaeontology palaeovertebrata palaios paleobiology parasitology past-and-present pediatric-allergy-and-immunology pediatric-anesthesia pediatric-blood-and-cancer pediatric-physical-therapy pediatric-research peerj periodontology-2000 permafrost-and-periglacial-processes perspectives-on-politics petit-chicago-author-date pharmacoepidemiology-and-drug-safety philippika philosophia-scientiae philosophiques phycological-research phyllomedusa physiological-and-biochemical-zoology pisa-university-press plant-and-cell-physiology plant-biology plant-cell-and-environment plant-pathology plant-physiology plos pnas podzemna-voda polish-legal political-studies politische-vierteljahresschrift pontifical-athenaeum-regina-apostolorum pontifical-biblical-institute pontifical-gregorian-university population population-space-and-place postepy-higieny-i-medycyny-doswiadczalnej poultry-science pour-reussir-note preslia presses-universitaires-de-rennes-archeologie-et-culture presses-universitaires-de-rennes primary-care-clinics-in-office-practice proceedings-of-the-royal-society-b proinflow protein-engineering-design-and-selection protein-science proteomics psychiatric-clinics-of-north-america psychiatric-services psychiatry-and-clinical-neurosciences psychological-medicine psychosomatic-medicine psychosomatics public-health-nutrition quaderni-degli-avogadro-colloquia radiochimica-acta radiographics radiography radiologic-clinics-of-north-america radiology radiopaedia r-and-d-management rapid-communications-in-mass-spectrometry recent-patents-on-drug-delivery-and-formulation reports-of-practical-oncology-and-radiotherapy reproduction reproduction-in-domestic-animals restoration-ecology reviews-of-modern-physics-with-titles revista-argentina-de-antropologia-biologica revista-brasileira-de-ciencia-do-solo revista-chilena-de-derecho-y-tecnologia revista-ciencias-tecnicas-agropecuarias revista-da-sociedade-brasileira-de-medicina-tropical revista-de-biologia-tropical revista-de-filologia-espanola revista-fave-seccion-ciencias-agrarias revista-latinoamericana-de-recursos-naturales revista-noesis revista-virtual-de-quimica revue-archeologique revue-archeologique-du-centre-de-la-france revue-de-medecine-veterinaire revue-des-nouvelles-technologies-de-l-information revue-dhistoire-moderne-et-contemporaine revue-francaise-de-sociologie rhodora risk-analysis rofo romanian-humanities rose-school rossiiskii-fiziologicheskii-zhurnal-imeni-i-m-sechenova royal-society-of-chemistry rtf-scan sage-harvard sage-vancouver-brackets sage-vancouver scandinavian-journal-of-infectious-diseases scandinavian-journal-of-medicine-and-science-in-sports scandinavian-journal-of-rheumatology scandinavian-journal-of-work-environment-and-health scandinavian-political-studies science-and-technology-for-the-built-environment scienceasia science-china-life-sciences science science-translational-medicine science-without-titles scientia-agriculturae-bohemica scrinium seminaire-saint-sulpice-ecole-theologie seminars-in-pediatric-neurology service-medical-de-l-assurance-maladie sexual-development sexual-health sheffield-hallam-university-history sist02 small sociedade-brasileira-de-computacao societe-archeologique-de-bordeaux societe-nationale-des-groupements-techniques-veterinaires society-for-american-archaeology society-for-general-microbiology society-for-historical-archaeology society-for-laboratory-automation-and-screening society-of-automotive-engineers-technical-papers-numeric society-of-biblical-literature-fullnote-bibliography socio-economic-review sociology-of-health-and-illness soil-biology-and-biochemistry soil-science-and-plant-nutrition solutions south-african-journal-of-animal-science south-african-journal-of-enology-and-viticulture south-african-journal-of-geology soziale-welt sozialpadagogisches-institut-berlin-walter-may sozialwissenschaften-heilmann soziologie spandidos-publications spanish-legal spectroscopy-letters spie-bios spie-journals spie-proceedings spip-cite springer-basic-author-date springer-basic-author-date-no-et-al springer-basic-brackets springer-basic-brackets-no-et-al-alphabetical springer-basic-brackets-no-et-al springer-fachzeitschriften-medizin-psychologie springer-humanities-author-date springer-humanities-brackets springer-lecture-notes-in-computer-science-alphabetical springer-lecture-notes-in-computer-science springer-mathphys-author-date springer-mathphys-brackets springer-physics-author-date springer-physics-brackets springerprotocols springer-socpsych-author-date springer-socpsych-brackets springer-vancouver-author-date springer-vancouver-brackets springer-vancouver statistika-statistics-and-economy-journal stavebni-obzor stem-cells-and-development stem-cells st-patricks-college strategic-management-journal stroke structural-control-and-health-monitoring studia-bas studii-teologice stuttgart-media-university style-manual-australian-government sunway-college-johor-bahru surgical-clinics-of-north-america surgical-neurology-international surgical-pathology-clinics svensk-exegetisk-arsbok swedish-legal systematic-and-applied-microbiology systematic-biology taxon taylor-and-francis-chicago-author-date taylor-and-francis-chicago-f taylor-and-francis-council-of-science-editors-author-date taylor-and-francis-harvard-x taylor-and-francis-national-library-of-medicine taylor-and-francis-numeric-q technische-universitat-dresden-betriebswirtschaftslehre-logistik-author-date technische-universitat-dresden-erziehungswissenschaften-author-date technische-universitat-dresden-finanzwirtschaft-und-finanzdienstleistungen-author-date technische-universitat-dresden-finanzwirtschaft-und-finanzdienstleistungen-author-date-with-short-titles technische-universitat-dresden-finanzwirtschaft-und-finanzdienstleistungen-note technische-universitat-dresden-historische-musikwissenschaft-note technische-universitat-dresden-kunstgeschichte-note technische-universitat-dresden-medienwissenschaft-und-neuere-deutsche-literatur-note technische-universitat-dresden-medizin technische-universitat-dresden-wirtschaftswissenschaften technische-universitat-munchen-controlling technische-universitat-munchen-unternehmensfuhrung technische-universitat-wien teologia-catalunya terra-nova tgm-wien-diplom the-accounting-review the-american-journal-of-cardiology the-american-journal-of-gastroenterology the-american-journal-of-human-genetics the-american-journal-of-pathology the-american-journal-of-psychiatry the-american-naturalist the-astrophysical-journal the-auk the-biological-bulletin the-bone-and-joint-journal the-british-journal-of-cardiology the-british-journal-of-psychiatry the-british-journal-of-sociology the-chemical-society-of-japan the-company-of-biologists the-design-journal the-egyptian-heart-journal the-embo-journal the-faseb-journal the-febs-journal the-geological-society-of-america the-geological-society-of-london the-historical-journal the-holocene the-institute-of-electronics-information-and-communication-engineers the-institution-of-engineering-and-technology the-international-journal-of-developmental-biology the-international-journal-of-psychoanalysis the-isme-journal the-journal-of-adhesive-dentistry the-journal-of-clinical-investigation the-journal-of-comparative-neurology the-journal-of-eukaryotic-microbiology the-journal-of-hellenic-studies the-journal-of-immunology the-journal-of-infection-in-developing-countries the-journal-of-juristic-papyrology the-journal-of-neuropsychiatry-and-clinical-neurosciences the-journal-of-neuroscience the-journal-of-pain the-journal-of-parasitology the-journal-of-peasant-studies the-journal-of-physiology the-journal-of-pure-and-applied-chemistry-research the-journal-of-roman-studies the-journal-of-the-acoustical-society-of-america the-journal-of-the-torrey-botanical-society the-journal-of-urology the-journal-of-wildlife-management the-journals-of-gerontology-series-a the-lancet the-lichenologist the-national-medical-journal-of-india the-neuroscientist the-new-england-journal-of-medicine theologie-und-philosophie the-oncologist the-open-university-a251 the-open-university-harvard the-open-university-m801 the-open-university-numeric the-open-university-numeric-superscript the-optical-society theory-culture-and-society the-plant-cell the-plant-journal the-review-of-financial-studies the-rockefeller-university-press the-saudi-journal-for-dental-research the-scandinavian-journal-of-clinical-and-laboratory-investigation the-world-journal-of-biological-psychiatry thieme-e-journals-vancouver thomson-reuters-legal-tax-and-accounting-australia thrombosis-and-haemostasis tissue-engineering toxicological-sciences traces traffic traffic-injury-prevention transactions-of-the-american-philological-association transactions-of-the-materials-research-society-of-japan transboundary-and-emerging-diseases transplantation transportation-research-record tree-physiology trends-journals triangle tropical-animal-health-and-production turabian-fullnote-bibliography ugeskrift-for-laeger ultrasound-in-medicine-and-biology unified-style-linguistics united-nations-conference-on-trade-and-development universidad-autonoma-cidudad-juarez-estilo-latino-humanistico universidade-de-sao-paulo-instituto-de-matematica-e-estatistica universidade-estadual-do-oeste-do-parana-programa-institucional-de-bolsas-de-iniciacao-cientifica universidad-evangelica-del-paraguay universita-cattolica-del-sacro-cuore universita-di-bologna-lettere universita-pontificia-salesiana universita-pontificia-salesiana-it universitas-negeri-semarang-fakultas-matematika-dan-ilmu-pengetahuan-alam universitat-bremen-institut-fur-politikwissenschaft universitat-freiburg-geschichte universitat-heidelberg-historisches-seminar universitat-mainz-geographisches-institut universitatsmedizin-gottingen universite-de-liege-histoire universite-de-picardie-jules-verne-ufr-de-medecine universite-de-sherbrooke-departement-de-geomatique universite-de-sherbrooke-faculte-d-education universite-du-quebec-a-montreal universiteit-utrecht-onderzoeksgids-geschiedenis universite-laval-departement-des-sciences-historiques universite-laval-departement-dinformation-et-de-communication universite-laval-faculte-de-theologie-et-de-sciences-religieuses universite-libre-de-bruxelles-histoire university-college-dublin-school-of-history-and-archives university-college-lillebaelt-apa university-college-lillebaelt-harvard university-college-lillebaelt-vancouver university-of-helsinki-faculty-of-theology university-of-new-england-australia-note university-of-south-australia-harvard-2011 university-of-south-australia-harvard-2013 university-of-zabol uppsala-universitet-historia uppsala-universitet-institutionen-for-biologisk-grundutbildning urban-habitats urban-studies urological-science u-schylku-starozytnosci user-modeling-and-user-adapted-interaction us-geological-survey uspekhi-gerontologii utah-geological-survey vancouver-author-date vancouver-brackets vancouver-brackets-no-et-al vancouver-brackets-only-year-no-issue vancouver vancouver-fr-ca vancouver-imperial-college-london vancouver-superscript-brackets-only-year vancouver-superscript vancouver-superscript-only-year veterinary-medicine-austria veterinary-pathology veterinary-radiology-and-ultrasound veterinary-record victoria-university-harvard vienna-legal vietnam-ministry-of-education-and-training-en vietnam-ministry-of-education-and-training-vi vigiliae-christianae vilnius-gediminas-technical-university vingtieme-siecle vodohospodarske-technicko-ekonomicke-informace vodohospodarske-technicko-ekonomicke-informace-en water-alternatives waterbirds water-environment-research water-science-and-technology weed-science-society-of-america west-european-politics wetlands wheaton-college-phd-in-biblical-and-theological-studies who-europe-harvard who-europe-numeric wiley-vch-books wireless-communications-and-mobile-computing wirtschaftsuniversitat-wien-handel-und-marketing wirtschaftsuniversitat-wien-master-management wirtschaftsuniversitat-wien-wirtschaftspadagogik wissenschaftlicher-industrielogistik-dialog world-applied-sciences-journal world-congress-on-engineering-asset-management world-journal-of-gastroenterology world-mycotoxin-journal world-organisation-for-animal-health-scientific-and-technical-review world-politcs worlds-poultry-science-journal xenotransplantation yeast zastosowania-komputerow-w-elektrotechnice zdravniski-vestnik zeitschrift-fur-deutsche-philologie zeitschrift-fur-internationale-beziehungen zeitschrift-fur-kunstgeschichte zeitschrift-fur-medienwissenschaft zeitschrift-fur-padagogik zeitschrift-fur-religionswissenschaft-author-date zeitschrift-fur-religionswissenschaft-note zeitschrift-fur-soziologie zeitschrift-fur-theologie-und-kirche zeszyty-prawnicze-bas zookeys zoological-journal-of-the-linnean-society zootaxa zwitscher-maschine))))

;; Run this from a Scheme session inside a document like:
;; (use-modules (tm-zotero))
;; (tm-zotero-save-cache-of-citation-layout-prefix-delimiter-suffix)
;; ;; there will be a long wait...
;;
;; Now move the new cache-of-citation-layout-prefix-delimiter-suffix.json to
;; the prog directory next to this tm-zotero.scm.
;;
(define-public (tm-zotero-save-cache-of-citation-layout-prefix-delimiter-suffix)
  (let* ((cache-alist (tm-zotero-get-cache-of-citation-layout-prefix-delimiter-suffix))
         (cache-ht (make-hash-table))
         (outfile (open-file "cache-of-citation-layout-prefix-delimiter-suffix.json" "w")))
    (do ((al cache-alist (cdr al)))
        ((null? al))
      ;;(tm-zotero-format-debug "_BOLD__RED_tm-zotero-save-cache-of-citation-layout-prefix-delimiter-suffix_RESET_: ~s ~s" (caar al) (cdar al))
      (hash-set! cache-ht (caar al) (cdar al)))
    (scm->json cache-ht outfile #:pretty #t)
    (close outfile)))

;; Because it's next to this scm file, it's in the scheme %load-path.
;;
(define-public (tm-zotero-read-cache-of-citation-layout-prefix-delimiter-suffix)
  (let* ((infile (open-file
                  (%search-load-path
                   "cache-of-citation-layout-prefix-delimiter-suffix.json")
                  "r"))
         (ht (json->scm infile)))
    ht))


;;;;;;
;;;
;;; hash table, style-id => (prefix delimiter suffix), three strings,
;;; i.e., "jm-indigobook" => '("" "; " ".")
;;;
(define tm-zotero-cache-of-citation-layout-prefix-delimiter-suffix-ht #f)

;;;;;;
;;;
;;; Called from set-init-env-for-zotero-document-prefs.
;;;
(define-public (tm-zotero-get-citation-layout-prefix-delimiter-suffix style-id)
  (when (not tm-zotero-cache-of-citation-layout-prefix-delimiter-suffix-ht)
    (set! tm-zotero-cache-of-citation-layout-prefix-delimiter-suffix-ht
          (tm-zotero-read-cache-of-citation-layout-prefix-delimiter-suffix)))
  (let ((style-id (if (string-index style-id (lambda (c) (eqv? c #\/)))
                      (substring style-id
                                 (string-index-right style-id
                                                     (lambda (c) (eqv? c #\/)))
                                 (string-length style-id))
                      style-id)))
    (or (hash-ref tm-zotero-cache-of-citation-layout-prefix-delimiter-suffix-ht style-id #f)
        (tm-zotero-get-citation-layout-prefix-delimiter-suffix-from-csl style-id))))

;;}}}


;;;;;;
;;;
;;; These are for accessing parts of the static source tree that are saved as
;;; part of the document. They deal with actual document trees.
;;;
;;{{{ zfield tags, trees, inserters, and tree-ref based accessors
;;{{{   Documentation Notes for zfield layout
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
;;;            <tuple|3|<raw-data|fieldCode>|"false"|<raw-data|"origText">>
;;;
;;;       0. That tuple's first child (tree-ref fieldCode 0) is the fieldCode
;;;          layout version number, 3.
;;;
;;;       1. The second child (tree-ref fieldCode 1) is a raw-data containing
;;;          the UTF-8 fieldCode string sent by Juris-M / Zotero.
;;;
;;;       2. The third child (tree-ref fieldCode 2) is a boolean flag that
;;;          tells whether the fieldText was editted or not. The only way to do
;;;          that is to disactivate the tag, edit the text, then reactivate the
;;;          tag.
;;;
;;;       3. The fourth child (tree-ref fieldCode 3) is the original formatted
;;;          string, inside of a raw-data, mainly to hide it and make it
;;;          uneditable.
;;;
;;; NOT SURE YET  v.4 (will be?) is the same as v.3, adding:
;;;
;; ;;;       4. The fifth child (tree-ref fieldCode 4) is the flag that shows
;; ;;;          whether or not the zfield has been interned or not. When the
;; ;;;          document is first loaded, it's meaningless since it's saved with
;; ;;;          the document.
;;;
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

;;{{{ zfield tag definitions, insert-new-zfield

(define-public zfield-tags '(zcite zbibliography))
(define-public ztHref*-tags '(ztHrefFromCiteToBib ztHrefFromBibToURL))
(define-public ztHref-tags '(ztHref))

(define-public ztRefsList-ensure-interned-tags '(zcite ztbibItemText))

(define tm-zotero-bbl-formats-tags
  '(zttextit zttexts zttextup zttextsc zttextnormal zttextbf zttextmd
    ;; underline textsuperscript textsubscript ; not ours
    ztbibItemText ztNewBlock ztLeftMargin ztRightInline ztbibIndent
    ztShowId
    ;; ztHref ztDefaultCiteURL ; wrong category
    ))

;;; Inside of a zcite's "with" environment is defined:
;;;   zt-zfieldID which is bound to the fieldID.


;;;;;;
;;;
;;; If any one of these is-*? => #t, then t is a zfield tree.
;;;
(define-public (is-zcite? t)
  (tm-is? t 'zcite))

(define-public (is-zbibliography? t)
  (tm-is? t 'zbibliography))

(define-public (is-zfield? t)
  (tm-in? t zfield-tags))


;;;;;;
;;;
;;; These are needed for e.g., clipboard-cut and clipboard-paste.
;;;
(define-public (has-zfields? t)
  (tm-find t is-zfield?))

(define-public (is-ztHref*? t)
  (tm-in? t ztHref*-tags))

(define-public (is-ztHref? t)
  (tm-in? t ztHref-tags))

(define-public (is-tm-zotero-tag t)
  (or (tm-in? t tm-zotero-bbl-formats-tags)
      (tm-in? t ztHref*-tags)
      (tm-in? t ztHref-tags)
      (tm-in? t zfield-tags)))


;;;;;;
;;;
;;; This is the top-half of new zfield insertion.
;;;
;;; This always happens at the cursor position. After the insert, the cursor is
;;; at the right edge of the newly inserted zfield, just inside the light-blue
;;; box around it. focus-tree with the cursor there returns the zfield tree.
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
      (let* ((documentID (get-documentID))
             (new-zfieldID (get-new-zfieldID))
             (new-zfd (make-instance <zfield-data>))
             (new-zfield
              (stree->tree
               `(,tag
                 ,new-zfieldID
                 (tuple "3" (raw-data "") "false" (raw-data ""))
                 ,placeholder))))
        ;;
        ;; Inserting the tree is magical. It connects the tree with a buffer
        ;; and thus with an editor. That makes the tree->tree-pointer work
        ;; right.
        ;;
        (insert new-zfield 1)
        (set! (zfd-tree new-zfd) new-zfield)
        (set-document-new-zfield-zfd! documentID new-zfd))
      ;;
      (begin ;; focus-is-zfield? => #t
        ;;
        ;; TODO This ought to be a dialog if it actually happens much...
        ;;      Alternatively, perhaps it could move the cursor out of the
        ;;      zfield, arbitrarily to the right or left of it, then proceed
        ;;      with inserting the new zfield... Or perhaps it ought to convert
        ;;      it into an editCitation rather than an insertCitation?
        ;;
        (tm-zotero-format-error "_BOLD__RED_ERR: insert-new-zfield_RESET_ ~s : focus-tree is a ~s\n"
                                tag (tree-label (focus-tree)))
        #f)))

;;}}}

;;{{{ zfield trees and tree-ref based accessors

;;;;;;
;;;
;;; The zfieldID is the string identifier for the field that is used as it's
;;; handle inside of Zotero and thus inside of citeproc.js there. When the tag
;;; is disactivated, the user could change it by hand, but there's not really
;;; any reason to do so. When a field is cut or copied and then pasted, it gets
;;; updated because if there is more than one field with the same zfieldID,
;;; Zotero will update both of them to be the same value!
;;;
(define zfield-zfieldID-t
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree-ref zfield 0))
   ;; set!
   (lambda (zfield t)
     (let ((zfieldID-t (tree-ref zfield 0)))
       (tree-assign zfieldID-t t)))))


(define zfield-zfieldID
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (let* ((zfieldID-t (zfield-zfieldID-t zfield))
            (zfieldID (tree->stree zfieldID-t)))
       ;; (tm-zotero-format-debug "zfield-zfieldID: zfield => ~s" (tree->stree zfield))
       ;; (tm-zotero-format-debug "zfield-zfieldID: zfieldID-t => ~s" (tree->stree zfieldID-t))
       ;; (tm-zotero-format-debug "zfield-zfieldID: zfieldID => ~s" zfieldID)
       zfieldID))
   ;; set!
   (lambda (zfield str)
     (let ((zfieldID-t (zfield-zfieldID-t zfield)))
       (tree-assign zfieldID-t
                    (stree->tree str))))))


;;;;;;
;;;
;;; This contains the version number for the zfield, as explained above. There
;;; is no setter, since it's hard-coded where the zfield is inserted into the
;;; document, and where it automatically upgrades the zfield from older
;;; documents to the newer format.
;;;
(define (zfield-Code-v zfield)
  (let ((code-t (tree-ref zfield 1)))
    (cond
      ((tm-func? code-t 'tuple)           ; >= v.3
       (string->integer (object->string (tree-ref code-t 0))))
      ((tm-func? code-t 'raw-data) 2)
      (else 1))))


;;;;;;
;;;
;;; This holds the code that Zotero's integration sends. It has a short string
;;; that identifies whether this is a citation or a bibliography, and also may
;;; contain the CSL-JSON data for the field's citations, when the
;;; document-prefs checkbox is checked for storing that information inside of
;;; the document.
;;;
;;;  The string begins with "ITEM " followed by a stringified JSON dictionary,
;;;  when "Store references inside document" is not checked, and when it is,
;;;  the string begins with "ITEM CSL_CITATION ".
;;;
(define zfield-Code-code-t
  (letrec ((ref-impl
            (lambda (zfield)
              (let ((code (tree-ref zfield 1)))
                (cond
                  ((tm-func? code 'tuple) ; >= v.3
                   ;; <tuple|3|<raw-data|THIS>|"false"|<raw-data|"origText">>
                   (tree-ref code 1 0))
                  ((tm-func? code 'raw-data) ; v.2
                   (tree-assign code
                                (stree->tree
                                 `(tuple "3" ; update to v.3
                                         ,(tree->stree code)
                                         "false"
                                         (raw-data ""))))
                   (ref-impl zfield) ; tail-call
                   )
                  ((tm-atomic? code)    ; v.1
                   (tree-assign code
                                (stree->tree
                                 `(tuple "3" ; to v.3
                                         (raw-data ,(tree->stree code))
                                         "false"
                                         (raw-data ""))))
                   (ref-impl zfield) ; tail-call
                   )
                  (else ; ? I don't think this can really happen.
                    (tree-assign code
                                 (stree->tree
                                  `(tuple "3" ; to v.3
                                          (raw-data "")
                                          "false"
                                          (raw-data ""))))
                    (ref-impl zfield) ; tail-call
                    )))))
           (set!-impl
            (lambda (zfield t)
              (let ((code-t (ref-impl zfield)))
                (tree-assign code-t t))))
           )
    (make-procedure-with-setter ref-impl set!-impl)))


(define zfield-Code-code
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree->stree (zfield-Code-code-t zfield)))
   ;; set!
   ;;
   ;; TODO maybe it can do more again like it tried to before wrt parsing the
   ;;      JSON and using the information within it... It will need to use that
   ;;      information in order to implement the zcite citation cluster
   ;;      splitting feature in the other todo item in this program.
   ;;
   (lambda (zfield str)
     (let ((code-t (zfield-Code-code-t zfield)))
       (tree-assign code-t
                    (stree->tree str))))))


;;;;;;
;;;
;;; This is set by notify-activated when the user has modified the
;;; zfield-Text-t. It is used inside of the tm-zotero.ts in order to decide
;;; what color flag to display, green for not modified, red for
;;; user-modified. Zotero will also notice if the user modified it, but through
;;; a different mechanism, and will notify and ask if you really want to have
;;; it update the field despite having modified it.
;;;
(define zfield-Code-is-modified?-flag-t
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree-ref zfield 1 2)) ; assume >= v.3
   ;; set!
   (lambda (zfield t)
     (let ((is-modified?-flag-t (tree-ref zfield 1 2)))
       (tree-assign is-modified?-flag-t t)))))


(define zfield-Code-is-modified?-flag
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree->stree (zfield-Code-is-modified?-flag-t zfield)))
   ;; set!
   (lambda (zfield str-bool)
     (let ((is-modified?-flag-t (zfield-Code-is-modified?-flag-t zfield)))
       (tree-assign is-modified?-flag-t
                    (stree->tree str-bool))))))

;;;;;;
;;;
;;; This holds a copy of the zfield-Text, and is set only right after getting a
;;; result from Zotero, and is kept for comparison when finding out if the user
;;; has modified the contents of zfield-Text-t, which can only be done while
;;; the zfield is disactivated.
;;;
(define zfield-Code-origText-t
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree-ref zfield 1 3 0))
   ;; set!
   (lambda (zfield t)
     (let ((origText-t (tree-ref zfield 1 3 0)))
       (tree-assign origText-t t)))))

(define zfield-Code-origText
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree->stree (zfield-Code-origText-t zfield)))
   ;; set!
   (lambda (zfield t)
     (let ((origText-t (zfield-Code-origText-t zfield)))
       (tree-assign origText-t
                    (format #f "~s" (tree->stree t)))))))

;;;;;;
;;;
;;; This field is set automatically with the result of converting the rich-text
;;; that Zotero sends back into a TeXmacs tree.
;;;
(define zfield-Text-t
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree-ref zfield 2))
   ;; set!
   (lambda (zfield t)
     (let ((text-t (tree-ref zfield 2)))
       (tree-assign text-t t)))))

;;;;;;
;;;
;;; This is used to convert the zfield-Text texmacs tree into a string so that
;;; Zotero's mechanism for determining if the user has editted the zfield-Text
;;; by hand can have something it can work with. It is used to store the
;;; original text in the <zfield-data> for the zfield, and to create the
;;; comparison string from the current value of the zfield-Text.
;;;
(define (zfield-Text zfield)
  (if (and zfield (tree? zfield))
      (format #f "~s" (tree->stree (zfield-Text-t zfield)))
      ""))

;;;
;;; There is no setter for zfield-Text.

;;}}}

;;{{{ ztHref* trees and tree-ref based accessors

;;;;;;
;;;
;;; ztHrefFromCiteToBib and ztHrefFromBibToURL are both emitted by the
;;; variableWrapper that is installed by propachi-texmacs in boostrap.js. The
;;; reason for setting them there is that it's the thing that knows which is
;;; the first field of a citation item or bibliographic entry, no matter which
;;; CSL style is in use. I want only the first four glyphs of each item to
;;; become a hyperlink because of the limitation of TeXmacs that a hyperlink
;;; locus is not line-broken, and so if it's too long, it will stick out into
;;; the right margin. (I say "glyphs" because when the group ends with a letter
;;; combination that the typesetter will turn into a ligature (ff fi fl ffi
;;; ffl), those must be inside of the link's "display" so they don't get
;;; separated, or it won't look right. That is handled by the variableWrapper.)
;;;
;;;   ztHrefFromCiteToBib hashLabel url display
;;;
;;;   ztHrefFromBibToURL  hashLabel url display
;;;
;;;   ztHref url display
;;;

(define zciteID-string-prefix "zciteID")
(define zciteID-string-prefix-length
  (string-length zciteID-string-prefix))

(define ztbibItemRef-hashlabel-string-prefix "#zbibSysID")
(define ztbibItemRef-hashlabel-string-prefix-length
  (string-length ztbibItemRef-hashlabel-string-prefix))


(define ztHref*-hashLabel-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree-ref ztHref* 0))
   ;; set!
   (lambda (ztHref* t)
     (let ((hashLabel-t (tree-ref ztHref* 0)))
       (tree-assign hashLabel-t t)))))

(define ztHref*-hashLabel
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree->stree (ztHref*-hashLabel-t ztHref*)))
   ;; set!
   (lambda (ztHref* str)
     (let ((hashLabel-t (ztHref*-hashLabel-t ztHref*)))
       (tree-assign hashLabel-t
                    (stree->tree str))))))


(define ztHref*-sysID
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (string-tail (ztHref*-hashLabel ztHref*)
                  ztbibItemRef-hashlabel-string-prefix-length))
   ;; set!
   (lambda (ztHref* sysID)
     (set! (ztHref*-hashLabel ztHref*)
           (string-append ztbibItemRef-hashlabel-string-prefix sysID)))))


(define (ztHrefFromCiteToBib-reflabel zfieldID sysID)
  (string-append
   zciteID-string-prefix zfieldID
   ztbibItemRef-hashlabel-string-prefix sysID))


(define ztHref*-url-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree-ref ztHref* 1))
   ;; set!
   (lambda (ztHref* t)
     (let ((url-t (tree-ref ztHref* 1)))
       (tree-assign hashLabel-t t)))))

(define ztHref*-url
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree->stree (ztHref*-url-t ztHref*)))
   ;; set!
   (lambda (ztHref* str)
     (let ((url-t (ztHref*-url-t ztHref*)))
       (tree-assign url-t
                    (stree->tree str))))))


(define ztHref*-display-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree-ref ztHref* 2))
   ;; set!
   (lambda (ztHref* t)
     (let ((display-t (tree-ref ztHref* 2)))
       (tree-assign display-t t)))))



(define ztHref-url-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref)
     (tree-ref ztHref 0))
   ;; set!
   (lambda (ztHref t)
     (let ((url-t (tree-ref ztHref 0)))
       (tree-assign url-t t)))))

(define ztHref-url
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref)
     (tree->stree (ztHref-url-t ztHref)))
   ;; set!
   (lambda (ztHref str)
     (let ((url-t (ztHref-url-t ztHref)))
       (tree-assign url-t
                    (stree->tree str))))))


(define ztHref-display-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref)
     (tree-ref ztHref 1))
   ;; set!
   (lambda (ztHref t)
     (let ((display-t (tree-ref ztHref 1)))
       (tree-assign display-t t)))))


;;}}}

;;{{{ ztBibItemText trees and tree-ref based accessors

;;;;;;
;;;
;;; Each item in the zbibliography is represented by a ztBibItemText.
;;;
;;; ztBibItemText sysID refsList citekey body
;;;
;;;   Each one's expansion sets a reference label just before including the
;;;   expansion of body, defined by: (merge "zbibSysID" (arg sysID))
;;;   that will be the target of a hyperlink from cite to bib.
;;;

;;}}}

;;}}}

;;;;;;
;;;
;;; These things are for tm-zotero program state that is *not* saved with the
;;; document. These are scheme data structures, not in-document trees.
;;;
;;{{{ State data for document and zfields, <zfield-data>, <document-data>
;;{{{ R&D Notes pertaining to maintaining this per-document state data
;;;;;
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
;;;
;;{{{ define-class for <zfield-data>

;;; TODO these could have a common subclass

(define-class-with-accessors-keywords <zfield-data> ()
  ;;;;;
  ;;
  ;; Here will be gathered, by tm-zotero-ext:ensure-zfield-interned!
  ;; information that will ultimately be quickly sent to Zotero in response to
  ;; Document_getFields. Also, in the style sheet, notice that the call to the
  ;; tm-zotero-ext-ensure-zfield-interned! is not placed prior to the setting
  ;; of the zfield's noteIndex binding... but the noteIndex can change during
  ;; the course of editting the document in ways that it would not be easy to
  ;; recieve a notification of... and so retrieval of the noteIndex must be
  ;; done at the last minute so they are fresh and correct when sent to Zotero
  ;; in the response to the Document_getFields editor command.
  ;;
  ;; It is assumed that nothing will call for the value of the
  ;; zfd-zfield-noteIndex slot of a <zfield-data> prior to the time that the
  ;; zfieldID slot has been set. Since the only thing that needs to care
  ;; about noteIndex is citeproc, this works very well, since it won't get sent
  ;; any information about zfields that are not part of the actual visible
  ;; document and so none of the <zfield-data> that it will process will have
  ;; an empty zfieldID.
  ;;
  ;;;;;
  (the-zfieldID-of #:init-value "")
  ;; (zfd-zfield-Code #:init-value "") ; redundant, just look in in the zfield
  ;; for it.
  ;; (zfd-zfield-noteIndex #:allocation #:virtual
  ;;                       #:slot-ref (lambda (zfd)
  ;;                                    (zfield-NoteIndex
  ;;                                     (slot-ref zfd 'zfieldID)))
  ;;                       #:slot-set!
  ;;                       (lambda (zfd val) ; run-time programmer error
  ;;                         (texmacs-error
  ;;                          "tm-zotero:<zfield-data>"
  ;;                          "No setter for read-only #:virtual zfd-zfield-noteIndex")))
  ;;;;;
  ;;
  ;; The remainder is information required internally by this plugin.
  ;;
  ;;;;;
  ;;
  ;; TeXmacs tree-pointer, locating the zfield's tree, for fast access with no
  ;; searching the document. They are of type <observer>.
  ;;
  (%tree-pointer #:init-value #f)
  (tree-pointer #:allocation #:virtual
                #:slot-ref (lambda (zfd)
                            (slot-ref zfd '%tree-pointer))
                #:slot-set! (lambda (zfd tp)
                              (if (and tp (observer? tp))
                                  (begin
                                    (tp-guardian tp)
                                    (slot-set! zfd '%tree-pointer tp))
                                  (begin
                                    (slot-set! zfd '%tree-pointer #f)))))
  (zfd-tree #:allocation #:virtual
            #:slot-ref (lambda (zfd)
                         (let ((tp (tree-pointer zfd)))
                           (if (and tp
                                    (observer? tp))
                               (tree-pointer->tree tp)
                               #f)))
            #:slot-set! (lambda (zfd t)
                          (if (and t (tree? t))
                              (begin
                                (set! (tree-pointer zfd) (tree->tree-pointer t))
                                (slot-set! zfd 'the-zfieldID-of (zfield-zfieldID t)))
                              (begin
                                (set! (tree-pointer zfd) #f)
                                (slot-set! zfd 'the-zfieldID-of #f)))))
  ;; String, original unmodified text for comparison
  (zfd-orig-text #:init-value "")
  ;;
  ;; Hash table, the result of safe-json-string->scm of the zfield-Code-code
  (%zfd-Code-code-ht #:init-value #f)
  (zfd-Code-code-ht #:allocation #:virtual
                    #:slot-ref (lambda (zfd)
                                 (let ((ht (slot-ref zfd %zfd-Code-code-ht)))
                                   (if ht ht
                                       (let ((new-ht (zfield-Code-code->scm
                                                      (zfield-Code-code (zfd-tree zfd)))))
                                         (slot-set! zfd %zfd-Code-code-ht new-ht)))))
                    ;;
                    ;; Access the hashtable via:
                    ;;
                    ;;  (let ((ht (zfd-Code-code-ht zfd)))...
                    ;;
                    ;; ... then modify it via hash-set! or whatever, then set
                    ;; it back with:
                    ;;
                    ;;  (set! (zfd-Code-code-ht zfd) ht)
                    ;;
                    ;; ... to cause the modifications to be serialized as the
                    ;; zfield code that is sent to Juris-M / Zotero during the
                    ;; editor integration protocol exchange.
                    ;;
                    ;; It should not be set by this means unless you want the
                    ;; zfield-Code-code to be updated. e.g., for when a
                    ;; multiple citation zcite is split into multiple zcite's,
                    ;; so that the zfield-Code-code is properly house-kept
                    ;; along with that editor-managed modification so that the
                    ;; interface with Zotero will not become confused.
                    ;;
                    ;; Other useful information available here is, for each
                    ;; citation, the prefix string, suffix string, locator
                    ;; string, the suppress author flag, and the suppress
                    ;; trailing punctuation flag.
                    ;;
                    #:slot-set! (lambda (zfd new-ht)
                                  (let ((ht (slot-ref zfd %zfd-Code-code-ht))
                                        (has-stored-reference? (hash-ref new-ht "has-stored-reference?" #f)))
                                    (when has-stored-reference?
                                      (hash-remove! new-ht "has-stored-reference?"))
                                    (slot-set! zfd %zfd-Code-code-ht new-ht)
                                    (when ht
                                      (set! (zfield-Code-code (zfd-tree zfd))
                                            (string-append
                                             (if has-stored-reference?
                                                 "ITEM CSL_CITATION "
                                                 "ITEM ")
                                             (safe-scm->json-string new-ht)))))))
  ;;;
  ;;
  ;; TODO Do these need time stamps for change-dependency trigger/track?
  ;;
  ;;;
  )

(define-method (clear-tree-pointer (zfd <zfield-data>))
  (set! (tree-pointer zfd) #f))


(define (zfield-Code-code->scm code_str)
  (unless (== "TEMP" code_str)
    (let* ((has-item? (string-prefix? "ITEM " code_str))
           (has-stored-reference? (not (not (string-prefix? "ITEM CSL_CITATION " code_str))))
           (code_str (substring code_str (if has-stored-reference? 18 5) (string-length code_str)))
           (scm (safe-json-string->scm code_str)))
      (when has-stored-reference?
        (hash-set! scm "has-stored-reference?" #t))
      (tm-zotero-format-debug "_BOLD__GREEN_zfield-Code-code->scm_RESET_:scm =>\n~a\n" (safe-scm->json-string scm #:pretty #t))
      scm)))

;;}}}
;;{{{ define-class for <ztHrefFromCiteToBib-data>

;;; We need the tree-pointer for maintaining lists of these in in-document
;;; order using merge.
;;;
(define-class-with-accessors-keywords <ztHrefFromCiteToBib-data> ()
  (the-zfieldID-of #:init-value "")
  (the-sysID-of #:init-value "")
  ;; TeXmacs tree-pointer
  (%tree-pointer #:init-value #f)
  (tree-pointer #:allocation #:virtual
                #:slot-ref (lambda (zhd)
                            (slot-ref zhd '%tree-pointer))
                #:slot-set! (lambda (zhd tp)
                              (if (and tp (observer? tp))
                                  (begin
                                    (tp-guardian tp)
                                    (slot-set! zhd '%tree-pointer tp))
                                  (begin
                                    (slot-set! zhd '%tree-pointer #f)))))
  (zhd-tree #:allocation #:virtual
            #:slot-ref (lambda (zhd)
                         (let ((tp (tree-pointer zhd)))
                           (if (and tp (observer? tp))
                               (tree-pointer->tree tp)
                               #f)))
            #:slot-set! (lambda (zhd t)
                          (if (and t (tree? t))
                              (set! (tree-pointer zhd) (tree->tree-pointer t))
                              (set! (tree-pointer zhd) #f)))))


(define-method (the-ref-label-of (zhd <ztHrefFromCiteToBib-data>))
  (ztHrefFromCiteToBib-reflabel (the-zfieldID-of zhd) (the-sysID-of zhd)))


(define-method (clear-tree-pointer (zhd <ztHrefFromCiteToBib-data>))
  (set! (tree-pointer zhd) #f))

;;}}}
;;{{{ define-class for <document-data>

(define-class-with-accessors-keywords <document-data> ()
  ;;
  ;; TODO This undo marking thing needs to be looked over because in
  ;;      zotero/chrome/content/zotero/xpcom/integration.js inside of
  ;;      Zotero.Integration.Fields.prototype.addEditCitation at the end, if it
  ;;      fails, it deletes the citation field.
  ;;
  ;;           Q: But what about all the other changes that might get made to
  ;;              the document during the addCitation or editCitation? If those
  ;;              only happen when the addCitation or editCitation are
  ;;              successful, then that's fine... and this undo marking isn't
  ;;              needed. It's also possible that the normal undo that already
  ;;              happens is sufficient... And so maybe this ought to be a
  ;;              boolean, or maybe a hashtable with transaction numbers, so
  ;;              that concurrent calls can be made, as for the getFields that
  ;;              the comment says can happen during the same time that the
  ;;              setDocPrefs dialog is open.
  ;;
  ;; is a zotero command active? If so, then a modification undo mark (I think
  ;; it's a number from the code in (utils library tree) try-modification) is
  ;; stored here.
  ;;
  (document-active-mark-nr #:init-value #f) ; #f | value returned by (mark-new)
  ;;
  ;; one new zfield at a time per document
  ;;
  (document-new-zfield-zfd #:init-value #f)     ; #f | <zfield-data>
  ;;
  ;; in-document-order list of <zfield-data>
  ;;
  (document-zfield-zfd-ls #:init-thunk list)
  ;;
  ;; hash-table of zfieldID => <zfield-data>
  ;;
  (document-zfield-zfd-ht #:init-thunk make-hash-table)
  ;;
  ;; If the document has any zbibliographies, then they are listed here in
  ;; document order. As of the time of writing this, it doesn't really make
  ;; sense to put more than one zbibliography into your document. This is a
  ;; list anyway, because I have tentative plans for how to support multiple
  ;; bibliographies in the future, utilizing an update to the integration
  ;; protocol, as well as adding information to the zbibliography hidden data
  ;; that will be passed to citeproc...
  ;;
  (document-zbibliography-zfd-ls #:init-thunk list)
  ;;
  ;; List of <ztHrefFromCiteToBib-data>, for referencing to the zcite location
  ;; for each citation to a zbibliography entry.
  ;;
  ;; Compute them once, memoized, and so when the in-document tag is actually
  ;; expanded, the operation is a fast hashtable lookup returning the
  ;; pre-computed list. The typesetter is run very often while using TeXmacs,
  ;; and so if the full computation had to be run each time the tag is
  ;; re-typeset (e.g. the user is typing on the page just above the
  ;; zbibliography) it would be very slow.
  ;;
  ;; hash-ref by:
  ;;   "sysID" => (list <ztHrefFromCiteToBib-data> ...) in document order.
  ;;   (the-ref-label-of zfd) => <ztHrefFromCiteToBib-data>
  ;;
  (document-ztbibItemRefs-ht #:init-thunk make-hash-table)
  ;;
  ;; Anything else?
  ;;
  )
;;}}}

;;{{{ <document-data>-ht, get-<document-data>, set-<document-data>!

;;;
;;; Reloading the zotero.scm module will cause this to be reinitialized to an
;;; empty hash table. That's fine. It will also get cleared when the
;;; document-part-mode changes.
;;;
;;; TODO I see a possible "memory leak" of tree-pointer's... they are attached
;;;      to trees in the buffer. So to clear the document-data, there must be a
;;;      single point of control in a function that calls tree-pointer-detach
;;;      for each of them before releasing everything via assignment of a fresh
;;;      hash-table to <document-data>-ht... actually, rather than assign a
;;;      fresh one, use hash-clear since it clears it without triggering a
;;;      resize... and it's already ballooned out to it's needed size once it's
;;;      been used once.
;;;

(define <document-data>-ht (make-hash-table)) ;; documentID => <document-data>


(define (set-<document-data>! documentID document-data)
  (hash-set! <document-data>-ht documentID document-data))


(define (get-<document-data> documentID)
  (or (hash-ref <document-data>-ht documentID #f)
      (let ((dd (make-instance <document-data>)))
        (set-<document-data>! documentID dd)
        dd)))


;;; TODO Guardians and after-gc-hook? UTSL and find out if those tree-pointers
;;;      automatically get cleaned up or do I need to detach them like this?
;;;      Maybe when I let go of the reference to the tree-pointer, it gets
;;;      cleaned up and detached? If that's the case then this function gets a
;;;      lot simpler.
;;;

(define (clear-<document-data>! documentID)
  (let* ((dd (get-<document-data> documentID))
         (new-zfield-zfd (and dd (document-new-zfield-zfd dd)))
         (zfd-ls (and dd (document-zfield-zfd-ls dd)))
         (zhd-ht (and dd (get-document-ztbibItemRefs-ht documentID)))
         ;; see: tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!
         (zhd-ls (and zhd-ht
                      (map (cut hash-ref zhd-ht <> #f)
                           (pick (lambda (key)
                                   (string-prefix? zciteID-string-prefix key))
                                 (hash-map->list (lambda (key elt)
                                                   key)
                                                 zhd-ht))))))
    (when new-zfield-zfd
      (clear-tree-pointer new-zfield-zfd)
      (set! (document-new-zfield-zfd dd) #f))
    (when zfd-ls
      (map clear-tree-pointer zfd-ls)) ; TODO Guardians and after-gc-hook
    (when zhd-ls
      (map clear-tree-pointer zhd-ls)) ; TODO Guardians and after-gc-hook
    (set-<document-data>! documentID (make-instance <document-data>))))

;;}}}

;;{{{ document-active-mark-nr

(define-method (get-document-active-mark-nr (documentID <string>))
  (document-active-mark-nr (get-<document-data> documentID)))

(define (set-document-active-mark-nr! documentID val)
  (set! (document-active-mark-nr (get-<document-data> documentID)) val))


;;}}}

;;{{{ get-new-zfieldID, get-document-new-zfield-zfd, etc.

(define get-new-zfieldID create-unique-id)

(tm-define (tm-zotero-ext:create-unique-id)
  (:secure)
  (create-unique-id))

;; (define-method (get-document-new-zfield-zfd)
;;   (get-document-new-zfield-zfd (get-documentID)))

(define-method (get-document-new-zfield-zfd (documentID <string>))
  (document-new-zfield-zfd (get-<document-data> documentID)))

(define-method (set-document-new-zfield-zfd! (documentID <string>) zfd-or-f)
  (if (or (== (class-of zfd-or-f) <zfield-data>)
          (== zfd-or-f #f))
      (set! (document-new-zfield-zfd (get-<document-data> documentID))
            zfd-or-f)
      (texmacs-error "set-document-new-zfield-zfd!: Wrong type argument, expecting <zfield-data> or #f: zfd-or-f => ~s" zfd-or-f)))


(define (zfield-is-document-new-zfield? documentID zfield-or-zfieldID)
  (let* ((zfieldID (or (and (string? zfield-or-zfieldID)
                            zfield-or-zfieldID)
                       (zfield-zfieldID zfield-or-zfieldID)))
         (zfd (get-document-new-zfield-zfd documentID))
         (new-zfieldID (or (and zfd
                                (the-zfieldID-of zfd))
                           "")))
    ;; (tm-zotero-format-debug "zfield-is-document-new-zfield?: new-zfieldID => ~s, zfieldID => ~s" new-zfieldID zfieldID)
    (if zfd
        (string=? zfieldID new-zfieldID)
        #f)))

;;;
;;; Called from the document-mark-cancel-error-cleanup-hook
;;;
(define (cleanup-document-new-zfieldID! documentID)
  (and-with zfd (get-document-new-zfield-zfd documentID)
    (hash-remove! (get-document-zfield-zfd-ht documentID)
                  (the-zfieldID-of zfd))
    (document-remove!-<zfield-data> zfd)
    (when (is-zbibliography? (zfd-tree zfd))
      (document-remove!-zbibliography-zfd zfd))
    (set-document-new-zfield-zfd! documentID #f)))

;;}}}

;;{{{ document-zfield-zfd-ls

(define-method (get-document-zfield-zfd-ls (documentID <string>))
  (document-zfield-zfd-ls (get-<document-data> documentID)))

(define (set-document-zfield-zfd-ls! documentID ls)
  (set! (document-zfield-zfd-ls (get-<document-data> documentID)) ls))

;;}}}
;;{{{ document-zfield-zfd-ht

(define-method (get-document-zfield-zfd-ht (documentID <string>))
  (document-zfield-zfd-ht (get-<document-data> documentID)))

(define (reset-document-zfield-zfd-ht! documentID)
  (set! (document-zfield-zfd-ht (get-<document-data> documentID))
        (make-hash-table)))

(define (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
  (hash-ref (get-document-zfield-zfd-ht documentID) zfieldID #f))


;;}}}
;;{{{ document-zbibliography-zfd-ls

(define-method (get-document-zbibliography-zfd-ls (documentID <string>))
  (document-zbibliography-zfd-ls (get-<document-data> documentID)))

(define-method (set-document-zbiblioraphy-zfd-ls! (documentID <string>) ls)
  (set! (document-zbibliography-zfd-ls (get-<document-data> documentID))
        ls))

(define (reset-document-zbibliography-zfd-ls! documentID)
  (set! (document-zbibliography-zfd-ls (get-<document-data> documentID))
        (list)))

(define (document-merge!-zbibliography-zfd zfd)
  (let* ((documentID (get-documentID))
         (zbl (get-document-zbibliography-zfd-ls documentID)))
    (set! (document-zbibliography-zfd-ls (get-<document-data> documentID))
          (merge! zbl (list zfd) <*-data>-less?))))

(define (document-remove!-zbibliography-zfd zfd)
  (let* ((documentID (get-documentID))
         (zbl (get-document-zbibliography-zfd-ls documentID)))
    (set! (document-zbibliography-zfd-ls (get-<document-data> documentID))
          (list-filter zbl (lambda (elt)
                             (not (eq? zfd elt)))))))

;;}}}
;;{{{ document-ztbibItemRefs-ht

(define-method (get-document-ztbibItemRefs-ht (documentID <string>))
  (document-ztbibItemRefs-ht (get-<document-data> documentID)))

(define (reset-ztbibItemRefs-ht! documentID)
  (set! (document-ztbibItemRefs-ht documentID) (make-hash-table)))

(define (document-merge!-ztbibItemRefs-ls zhd)
  (let* ((documentID (get-documentID))
         (zhd-ht (get-document-ztbibItemRefs-ht documentID))
         (sysID (the-sysID-of zhd))
         (zhd-ls (hash-ref zhd-ht sysID '())))
    (hash-set! zhd-ht sysID (merge! zhd-ls (list zhd) <*-data>-less?))))

(define (document-get-ztbibItemRefs-ls sysID)
  (let* ((documentID (get-documentID))
         (zhd-ht (get-document-ztbibitemrefs-ht documentID)))
    (hash-ref zhd-ht sysID '())))

;; TODO needs to have consistent interface vis-a-vis documentID and :refactor:
;; whether to pass the documentID argument in or not.

(define (document-remove!-ztbibItemRefs-ls zhd)
  (let* ((documentID (get-documentID))
         (zhd-ht (get-document-ztbibItemRefs-ht documentID))
         (sysID (the-sysID-of zhd))
         (zhd-ls (hash-ref zhd-ht sysID '())))
    (hash-set! zhd-ht sysID
               (list-filter zhd-ls (lambda (elt)
                                     (not (eq? zhd elt)))))))

;;}}}

;;{{{ get-document-*-by-zfieldID

(define (get-document-<zfield-data>-by-zfieldID documentID zfieldID)
  (hash-ref (get-document-zfield-zfd-ht documentID) zfieldID #f))


(define (get-document-zfield-tree-pointer-by-zfieldID documentID zfieldID)
  (let ((zfd (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))
    (when zfd
      (tree-pointer zfd))))

(define (set-document-zfield-tree-pointer-by-zfieldID! documentID zfieldID tp)
  (let ((zfd (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))
    (when zfd
      (set! (tree-pointer zfd) tp))))


(define (get-document-zfield-by-zfieldID documentID zfieldID)
  (tree-pointer->tree
   (get-document-zfield-tree-pointer-by-zfieldID documentID zfieldID)))

(define (go-to-document-zfield-by-zfieldID documentID zfieldID)
  (tree-go-to (get-document-zfield-by-zfieldID documentID zfieldID) 1))


(define (get-document-zfield-orig-text-by-zfieldID documentID zfieldID)
  (let ((zfd (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))
    (if zfd
        (zfd-orig-text zfd)
        "")))

(define (set-document-zfield-orig-text-by-zfieldID! documentID zfieldID str)
  (set! (zfd-orig-text (get-document-<zfield-data>-by-zfieldID documentID zfieldID))
        str))

;;}}}

;;{{{ document-zfield-text-user-modified?

(define (document-zfield-text-user-modified? documentID zfieldID)
  (let* ((zfield        (get-document-zfield-by-zfieldID documentID zfieldID))
         (zfield-Tt (and zfield (zfield-Text-t zfield)))
         ;; from the document tree itself
         (text          (or (and zfield-Tt
                                 (zfield-Text zfield))
                            ""))
         ;; from the <zfield-data>
         (orig-text (or (get-document-zfield-orig-text-by-zfieldID documentID zfieldID)
                        "")))
    ;; See: definition for activate and disactivate...
    (not
     (string=? text orig-text))))         ; => #t if text was modified by user.

;;}}}

;;{{{ document-merge!-<zfield-data>, document-remove!-<zfield-data>
;;;
;;; This adds a <zfield-data> to the <zfield-data>-ls.
;;;
;;; TODO Should these take a documentID argument instead of looking it up here?
;;;      I need to think about it sometime, and when doing so, think about
;;;      things like a future version of TeXmacs with multiple threads maybe
;;;      the way the Chrome browser does it or whatever? This is a longer term
;;;      study project for me... right now I need to get this to work so I can
;;;      use it.
;;;
(define (document-merge!-<zfield-data> zfd)
  (let* ((documentID (get-documentID))
         (zfl (get-document-zfield-zfd-ls documentID)))
    (set! (document-zfield-zfd-ls (get-<document-data> documentID))
          (merge! zfl (list zfd) <*-data>-less?))))

;;;
;;; This removes a <zfield-data> from the <zfield-data>-ls.
;;;
(define (document-remove!-<zfield-data> zfd)
  (let* ((documentID (get-documentID))
         (zfl (get-document-zfield-zfd-ls documentID)))
    (set-document-zfield-zfd-ls! documentID
                                 (list-filter zfl (lambda (elt)
                                                    (not (eq? zfd elt)))))))

;;}}}

;;}}}


;;{{{ :secure tm-zotero-ext:* functions called from tm-zotero.ts style

;;;;;;
;;;
;;; These must be tm-define'd and carry the (:secure) attribute so that they
;;; can be called from within macros in the style package.
;;;
;;;;;;
;;;
;;; TODO instead of walking the document-zfield-zfd-ls to build the output form
;;;      in tm-zotero-Document_insertField, the data structure that it sends to
;;;      Zotero there can be built by tm-zotero-ext:ensure-zfield-interned!,
;;;      and maintained in clipboard-cut, etc... That way, the work it does can
;;;      be amortized across many calls, rather than doing that work all at
;;;      once.
;;;
;;; TODO Explore the idea of using a hook function to manage the changes to the
;;;      zfield: tm-zotero-notify-zfield-Text-update-hook Just make sure that
;;;      the hook functions for it are given everything they need so they don't
;;;      have to compute too much.
;;;
;;;;;;
;;;
;;; Lazy interning of <zfield-data>, triggered by the typesetter.
;;;
;;; When the <zcite|...> or <zbibliography|...> are typeset, the expansion
;;; calls on this routine.
;;;
;;; The fast path will hopefully be a fast path, so this won't slow it down by
;;; being called often.
;;;
;;; When the zfield is first interned, and when the text is set by interaction
;;; with Zotero, but not necessary when the tag is activated after having been
;;; disactivated and the visible text potentially modified since the zfieldID
;;; and the sysID for each citation won't change in that case...
;;;
;;;  * For each ztHrefFromCiteToBib inside of the citation cluster (since those
;;;    are in the source document because they are emitted by the
;;;    variableWrapper) (and in the case of a parallel legal citation where
;;;    there can be more than one linking to the identical bibliography entry,
;;;    just skip the subsequent identical ones; they should occur only in
;;;    groups with no intervening other sysID's);
;;;
;;;    * Using the hashLabel argument, get the sysID of the bibliography entry
;;;      it is linked to. Now we have both the zfieldID and the sysID.
;;;
;;; TODO Why is this a hashLabel rather than simply the plain sysID? What will
;;;      happen to existing documents if I change this? Will it require an
;;;      update to propachi-texmacs?
;;;
;;;      It's a hashlabel because that's what I thought about it like
;;;      then... also so that an external reference into the document can be
;;;      resolved...? Not sure it needs the # prepended.
;;;
;;;
;;; TODO This seemed faster at first, but I think that it's getting called way
;;;      too often, like every time I type anything, and so it's actually
;;;      slowing the editor down even more than the previous version did. How
;;;      can I make it faster, or make it only happen when it needs to; only
;;;      the first time it's typeset and only thereafter when it's changed?
;;;      Self-modifying document? A case or if inside the macro, shortcutting
;;;      in a faster way there, perhaps with a flag like I did for the
;;;      "is-modified" flag, so it does not call into Guile every time I type
;;;      anything? Will Guile-2 speed it up any? I think that it will, but not
;;;      enough.
;;;
;;;      Perhaps the "thunking" back and forth from C++ <--> Scheme is too
;;;      slow?  How does Swig do it? Is it any faster? Does that matter? I
;;;      think avoiding the jumping into scheme for this will be the best
;;;      speedup no matter what... Or... would general purpose support for
;;;      this, perhaps through a new kind of Observer, is what it needs?
;;;
;;;      Branch cuts?
;;;
;;;      UPDATE Weird. It's not my code that's the slowdown. When I ran it in
;;;      an Emacs shell buffer to capture the timings to show the slowdown, it
;;;      failed to be slow... It's quick the way it's supposed to be. I wonder
;;;      what it was?
;;;
;;;
;;;      * Hang the new item on the list for it's sysID with the merge function.
;;;
(tm-define (tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  (:secure)
  ;; (tm-zotero-format-debug "_BOLD__YELLOW_tm-zotero-ext:ensure-zfield-interned!_RESET_:called, zfieldID => ~s"
  ;;                         (or (and zfieldID-t
  ;;                                  (tree? zfieldID-t)
  ;;                                  (tree->stree zfieldID-t))
  ;;                             "{?? undefined ??}"))
  (%tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  "")

(tm-define (%tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!_WHITE_:  _BOLD__RED_Default_RESET_")
  #t)

(tm-define (%tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  (:require (and (in-tm-zotero-style?)
                 (is-during-tm-zotero-clipboard-cut?)))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!_WHITE_:  _BOLD__RED_is-during-tm-zotero-clipboard-cut_RESET_")
  #t)

(tm-define (%tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  (:require (and (in-tm-zotero-style?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (tree? zfieldID-t)
                 (not (inside-shown-part? zfieldID-t))))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!_WHITE_:  _BOLD__RED_zfield not inside shown part_RESET_")
  #t)

(tm-define (%tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  (:require (and (in-tm-zotero-style?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (tree? zfieldID-t)
                 (inside-shown-part? zfieldID-t)
                 (inside-inactive? zfieldID-t)))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!_WHITE_:  _BOLD__RED_zfield inactive_RESET_")
  #t)


(tm-define (%tm-zotero-ext:ensure-zfield-interned! zfieldID-t)
  (:require (and (in-tm-zotero-style?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (tree? zfieldID-t)
                 (inside-shown-part? zfieldID-t)
                 (not (inside-inactive? zfieldID-t))))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!_WHITE_:  _BOLD__RED_Interning!_RESET_")
  (let* ((documentID (get-documentID))
         (zfieldID (tree->stree zfieldID-t))
         ;;         fail if this is the new-zfield not yet finalized by
         ;;         Document_insertField.
         (is-new? (zfield-is-document-new-zfield? documentID zfieldID)))
    ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!_RESET_:    zfieldID => ~s, is-new? => ~a" zfieldID is-new?)
    (if is-new?
        (begin
          ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!:    _BOLD__RED_returning, is-new? => #t_RESET_")
          "")
        (let ((zfd (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))
          (if zfd
              (begin
                ;; then we're done, it's already interned.
                ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!:    _BOLD__RED_returning, zfd was already interned_RESET_")
                "")
              ;;
              ;; else...
              ;;
              ;; This is designed to be called only from inside of the zcite or
              ;; zbibliography expansion, during typesetting of the enclosing
              ;; zfield tag. This tree-search-upwards will terminate very
              ;; quickly because it will never be very deeply nested inside of
              ;; the zfield. Remember, this happens during typesetting.
              ;;
              (begin
                (and-with zfield (tree-search-upwards zfieldID-t zfield-tags)
                  (set! zfd (make-instance <zfield-data> #:zfd-tree zfield
                                           #:%zfd-Code-code-ht (zfield-Code-code->scm
                                                                (zfield-Code-code zfield))))
                  (hash-set! (get-document-zfield-zfd-ht documentID) zfieldID zfd)
                  (document-merge!-<zfield-data> zfd)
                  (when (is-zbibliography? zfield)
                    (document-merge!-zbibliography-zfd zfd)))
                ;; (tm-zotero-format-debug
                ;;  "_YELLOW_%tm-zotero-ext:ensure-zfield-interned!:    _RED_returning. _BOLD_Interned new zfield._RESET_")
                ""))))))


;;;;;;
;;;
;;; TODO redundent comments need cleaning up; functionality first today.
;;;
;;; At the end of a bibliography entry can appear a ztbibItemRefs-t, emitted
;;; during the expansion of the ztbibItemText macro. (The feature can be
;;; switched off by putting the cursor just inside of the zbibliography and
;;; using the wrench menu to uncheck it.)
;;;
;;; When enabled, it is a list of pagerefs (or refs; the format is user
;;; defineable via the preamble or user style sheet, with the default being
;;; defined in tm-zotero.ts) where the keys are the reference binding labels
;;; that look like:
;;;
;;;   zciteID+bPIlREAP9snjum#zbibSysID4112
;;;
;;; … which indicates that in zciteID +bPIlREAP9snjum there is a citation to
;;; Juris-M or Zotero SysID4112, which is also found inside the
;;; ztHrefFromCiteToBib that is inside of that zcite, as it's first argument,
;;; #zbibSysID4112, which is actually the label of the bibliography entry...
;;;
;;; In the particular document that I'm taking this example from, I find inside
;;; the references collection the entry:
;;;
;;;   <associate|zciteID+bPIlREAP9snjum#zbibSysID4112|<tuple|1.2.<with|font|<quote|palatino>|\<#00B6\>><space|0.2spc>1|17>>
;;;
;;; … which indicates that a ref will show the text "1.2.¶1", and a pageref
;;; will show the text "17". Those reference bindings are updated when the menu
;;; for “Document… > Update… > Buffer” is activated. Above it, inside the
;;; document itself, I find (abridged here with […]):
;;;
;;;   <zcite|+bPIlREAP9snjum|
;;;          <tuple|3|<#[…]>|false|<#>>|
;;;          <zttextit|<ztHrefFromCiteToBib|#zbibSysID4112|https[…]|Caus>e of action>,…
;;;
;;; … and in the bibliography is found:
;;;
;;;   <ztbibItemText|4112||sysID4112|
;;;                  <zttextit|<ztHrefFromBibToURL|#zbibSysID4112|<slink|https://www.law.cornell.edu/uscode/text/42/14141>|Caus>e
;;;                            of action>, 42 US Code <SectionSignGlyph|><space|0.5spc>14141.>
;;;
;;; The destination URL is enclosed in a slink. To prevent the LaTeX to TeXmacs
;;; conversion from mangling the URL, it is transmitted by the variableWrapper
;;; inside of a \path{} macro. During macro expansion inside of tm-zotero.ts
;;; it's destination will be pulled out and used as the destination for an href
;;; macro.
;;;
;;; So the purpose of the ztHrefFromBibToURL is to provide, when defined in the
;;; Juris-M or Zotero reference entry, the external URL or DOI for that item,
;;; so that the bibliography entry can link to there when that setting is
;;; enabled. Otherwise, there would be no need for it, since the sysID is
;;; already part of the ztbibItemText macro.
;;;
;;;   The hashLabel argument to ztHrefFromBibToURL is not used. I think I put
;;;   it in there so that it would have the same arity and layout as
;;;   ztHrefFromCiteToBib. That might change before this program hits beta
;;;   readiness. In this context the # prefix isn't needed, but for the links
;;;   from citations to bibliography entries, the # is there for the same
;;;   reason it's there when you link to an anchor in an HTML page.
;;;
;;; The macro expansion of ztbibItemText creates a label like "zbibSysID4112",
;;; with no hash mark (#) prepended. Following the body is a ztbibItemRefsList
;;; which receives sysID (the first argument, 4112) as it's sole argument.
;;;
;;; The ztbibItemText macro has as it's second argument "refsList". That is for
;;; a possible future update where the citeproc-js generates it in order to
;;; simplify this program, since it already possesses that information. It
;;; will be an in-document-order list of zfieldID's for each zfield where the
;;; bibliography item is referenced from. See tm-zotero.ts.
;;;

;;;;;;
;;;
;;; TODO Code cleanup, variable name fixing... Evolution is happening, Ok?
;;;
;;; This does a similar thing for the ztHrefFromCiteToBib and ztbibItemText
;;; macros, since their data is needed to gather the ztbibItemRefsList
;;; trees. (ztHrefFromCiteToBib isn't needed for this)
;;;
;;; The variableWrapper installed into the Juris-M / Zotero citeproc by
;;; propachi-texmacs always emits the \ztHrefFromBibToURL and
;;; \ztHrefFromCiteToBib macros. tm-zotero can decide what to do with
;;; them. They can be switched on and off as hyperlinks, but are always needed
;;; when the ztbibItemRefsList is displayed. When it's not, this won't really
;;; cost much time anyway, so it will always intern these.
;;;
;;; So that means that the list of tags that this or other similar "lazy
;;; interning" functions needs to cover must be kept in sync with what's
;;; actually being output via the propachi-texmacs citeproc-js output format
;;; definition this uses, and with the variableWrapper function that's defined
;;; by a monkey patch you will find inside the bootstrap.js for
;;; propachi-texmacs.
;;;                                 hashLabel       url                                                                              display
;;; \zttextsc{\ztHrefFromCiteToBib{#zbibSysID3410}{\path{http://ctan.mirror.ac.za/macros/latex2e/contrib/biblatex/doc/biblatex.pdf}}{Phil}ipp Lehman \& P. Kime}, \zttextsc{The Biblatex Package} (2006).
;;;
;;;                SysID refsList citekey  body
;;; \ztbibItemText{2332}{}{sysID2332}{\ztHrefFromBibToURL{#zbibSysID2332}{\path{http://heinonlinebackup.com/hol-cgi-bin/get\_pdf.cgi?handle=hein.journals/helr17\&section=14}}{Wilk}ins, T.A., 1993. Mootness doctrine and the post-compliance pursuit of civil penalties in environmental citizen suits. Harv. Envtl. L. Rev. 17, 389.}%
;;;
;;; In the above example the refsList is empty. Later it will contain a string
;;; as described elsewhere.
;;;

;;; There is always going to be a ztHrefFromCiteToBib coming from inside of the
;;; variableWrapper function set into place by propachi-texmacs boostrap.js.
;;;
;;; The link, reference, or pageref target label marking the document location
;;; of the expansion of a ztHrefFromCiteToBib tag is produced by the expansion
;;; of this tag is formed by something like:
;;;
;;;   <label|<merge|zciteID|<value|zt-citeID>|hashLabel>>
;;;
;;; ... where "zciteID" is a hard string-only-tree, and zt-citeID is bound just
;;; befor the call in to the rendering macro for the zcite, causing it to be
;;; dynamically bound inside of the environment of the ztHrefFromCiteToBib
;;; macro, which uses it as you see here. So the labels look like:
;;;
;;;          zt-zfieldID              sysID of citation within cluster.
;;;   zciteID+OUb13F8TlKQgjB#zbibSysID3410
;;;
;;; So this label represents the location where the links in the list shown in
;;; the bibliography will reference, or "point to".
;;;
;;; An invariant that this counts on is that the ztHrefFromCiteToBib is never
;;; encountered prior to the interning of the zcite that it is inside of. That
;;; means that a <zfield-data> has already been interned for this...
;;;
;;; TODO It seems like this macro ought to get handed the zfieldID as an
;;;      argument, but it does not, since it's sent by the variableWrapper, and
;;;      I did not know how to get the zfieldID from inside of there. It's
;;;      probably possible, but I don't know the way around in Javascript very
;;;      well.
;;;
;;; What I don't like about this is the having to look up the zfield, then the
;;; zfieldID, then the <zfield-data>, which is necessary for keeping these in
;;; in-document order with the merge. The main problem is, I think, is that
;;; tree-search-upwards, and the lookups... must happen each time that this tag
;;; is encountered... then in order to see if it's already interned, it has to
;;; look through the list of references to this sysID with member...
;;;
;;; Maybe doing this from inside of the zfield interning function is better?
;;; Yes, since that avoids several lookups.
;;;
;;; In order to keep these little lists in in-document order.

;;
;; TODO zfield => #f happened but I don't know how... No guards so I can try
;;      and find it if it happens again.
;;

(tm-define (tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  (:secure)
  ;; (tm-zotero-format-debug "_BOLD__YELLOW_tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!_RESET_:called, zfieldID => ~s, hashLabel => ~s"
  ;;                         (tree->stree zfieldID-t) (tree->stree hashLabel-t))
  (%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  "")


(tm-define (%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!_WHITE_:  _BOLD__RED_Default_RESET_")
  #t)


(tm-define (%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  (:require (and (in-tm-zotero-style?)
                 (is-during-tm-zotero-clipboard-cut?)))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!_WHITE_:  _BOLD__RED_inside-tm-zotero-clipboard-cut_RESET_")
  #t)


(tm-define (%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  (:require (and (in-tm-zotero-style?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (inside-inactive? zfieldID-t)))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!_WHITE_:  _BOLD__RED_zfield inactive_RESET_")
  #t)


(tm-define (%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  (:require (and (in-tm-zotero-style?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (inside-inactive? zfieldID-t)
                 (not (inside-shown-part? zfieldID-t))))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!_WHITE_:  _BOLD__RED_zfield not inside shown part_RESET_")
  #t)


(tm-define (%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned! zfieldID-t hashLabel-t)
  (:require (and (in-tm-zotero-style?)
                 (not (is-during-tm-zotero-clipboard-cut?))
                 (tree? zfieldID-t)
                 (not (inside-inactive? zfieldID-t))
                 (inside-shown-part? zfieldID-t)))
  ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!:  _BOLD__RED_Interning..._RESET_")
  (let* ((zfieldID (tree->stree zfieldID-t))
         (ztHref* (tree-up hashLabel-t))
         ;;(dummy (tm-zotero-format-debug "tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!: ztHref* => ~s" ztHref*))
         (sysID (string-tail (tree->stree hashLabel-t) ztbibItemRef-hashlabel-string-prefix-length))
         ;;(dummy (tm-zotero-format-debug "tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!: sysID => ~s" sysID))
         (documentID (get-documentID))
         (zhd-ht (get-document-ztbibItemRefs-ht documentID))
         (ref-label (ztHrefFromCiteToBib-reflabel zfieldID sysID)))
    ;; Inside of legal case parallel citations the secondary citations have the
    ;; same sysID as the first one does. Only intern the first one.
    (if (not (hash-ref zhd-ht ref-label #f))
        (let ((zhd (make-instance <ztHrefFromCiteToBib-data>
                                  #:the-zfieldID-of zfieldID
                                  #:the-sysID-of sysID
                                  #:zhd-tree ztHref*)))
          ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!:    _BOLD__RED_Interned new:_RESET_ ~a"
          ;;                         ref-label)
          ;; handles for later clearing of the tree pointers in these
          (hash-set! zhd-ht ref-label zhd)
          (document-merge!-ztbibItemRefs-ls zhd))
        (begin
          ;; (tm-zotero-format-debug "_YELLOW_%tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!:    _BOLD__RED_Already interned:_RESET_ ~a"
          ;;                         ref-label)
          #t))))

;;;;;;
;;;
;;; Return the ztbibItemRefsList for this zfieldID.
;;;
(tm-define (tm-zotero-ext:get-ztbibItemRefsList sysID-t)
  (:secure)
  (let* ((sysID (tree->stree sysID-t))
         (documentID (get-documentID))
         (zhd-ht (get-document-ztbibItemRefs-ht documentID))
         (zhd-ls (hash-ref zhd-ht sysID '()))
         (ref-labels-ls (map the-ref-label-of zhd-ls)))
    ;; (tm-zotero-format-debug "tm-zotero-ext:_BOLD_get-ztbibItemRefsList:_RESET_ _GREEN_ref-labels-ls_RESET_ => ~s" ref-labels-ls)
    `(zt-ref-sep ,@ref-labels-ls)))

;;;;;;
;;;
;;; This won't return the real true result until the zbibliography zfield is
;;; typeset, thereby calling on tm-zotero-ext:ensure-zfield-interned!, which
;;; adds it to the list this checks. So when the zbibliography is at the end of
;;; the document, anything that has conditional presentation or whatever based
;;; on the value returned by this ext function will be affected the first time
;;; the typesetter runs the document, as when it is first loaded or the
;;; document-part-mode has just been changed, triggering resetting of the
;;; <document-data> and <zfield-data> etc.
;;;
;;; The second time the typesetter runs though, this will return the correct
;;; result... it runs pretty often as the document is editted, so no worries.
;;;
(tm-define (tm-zotero-ext:document-has-zbibliography?)
  (:secure)
  (let ((zbibliography-zfd-ls (get-document-zbibliography-zfd-ls (get-documentID))))
    (if (and (pair? zbibliography-zfd-ls)
             (nnull? zbibliography-zfd-ls))
        "true"
        "false")))



;;;;;;
;;;
;;; The arguments to these are always of class <tree>, because it is handed to
;;; these ext functions by the typesetter, and that's what it works with and
;;; what it gives us.
;;;
(tm-define (tm-zotero-ext:is-zcite? zfieldID-t)
  (:secure)
  (if (is-zcite?
       (get-document-zfield-by-zfieldID (tree->stree zfieldID-t)))
      "true"
      "false"))

(tm-define (tm-zotero-ext:is-zbibliography? zfieldID-t)
  (:secure)
  (if (is-zbibliography?
       (get-document-zfield-by-zfieldID (tree->stree zfieldID-t)))
      "true"
      "false"))

(tm-define (tm-zotero-ext:is-zfield? zfieldID-t)
  (:secure)
  (if (is-zfield?
       (get-document-zfield-by-zfieldID (tree->stree zfieldID-t)))
      "true"
      "false"))



(tm-define (tm-zotero-ext:inside-footnote? zfieldID-t)
  (:secure)
  (if (inside-footnote? zfieldID-t)
      "true"
      "false"))

(tm-define (tm-zotero-ext:inside-endnote? zfieldID-t)
  (:secure)
  (if (inside-endnote? zfieldID-t)
      "true"
      "false"))

(tm-define (tm-zotero-ext:inside-note? zfieldID-t)
  (:secure)
  (if (inside-note? zfieldID-t)
      "true"
      "false"))



(tm-define (tm-zotero-ext:inside-zcite? t)
  (:secure)
  (if (inside-zcite? t)
      "true"
      "false"))

(tm-define (tm-zotero-ext:inside-zbibliography? t)
  (:secure)
  (if (inside-zbibliography? t)
      "true"
      "false"))

(tm-define (tm-zotero-ext:not-inside-zbibliography? t)
  (:secure)
  (if (inside-zbibliography? t)
      "false"
      "true"))

(tm-define (tm-zotero-ext:inside-zfield? t)
  (:secure)
  (if (inside-zfield? t)
      "true"
      "false"))


;;{{{ Older stuff below from past experiments, not really in use, but
;;; referenced from the style sheet, so leave them here for now in case a
;;; revisit of the experiments discover a use for them.

;;; ztShowID
;;;
;;; I don't think this one will ever really show up, but just in case, I've
;;; defined it, so it will be at least possible to observe it when it occurs.
;;;
;;; "<span class=\"" + state.opt.nodenames[cslid] + "\" cslid=\"" + cslid + "\">" + str + "</span>"
;;;
;;; "\\ztShowID{#{state.opt.nodenames[cslid]}}{#{cslid}}{#{str}}"
;;;
;; (tm-define (tm-zotero-ext:ztShowID node cslid body)
;;   (:secure)
;;   ;; (tm-zotero-format-debug "zt-ext-ztShowID: ~s ~s ~s" node clsid body)
;;   '(concat ""))


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
;;; object (with the... the... bell rang or something and I never finished what
;;; I'd been saying there...)
;;;
;; (tm-define (tm-zotero-ext:zbibCitationItemID sysID)
;;   (:secure)
;;   ;; (tm-zotero-format-debug "STUB:zt-ext-zbibCitationItemID: ~s" sysID)
;;   "")

;; (tm-define (tm-zotero-ext:bibitem key)
;;   (:secure)
;;   ;; (tm-zotero-format-debug "STUB:zt-ext-bibitem: ~s" key)
;;   "")

;;}}}

;;}}}



;;{{{ Wire protocol between TeXmacs and Zotero

;;;;;;
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
;;;;;;

(define (close-tm-zotero-socket-port!)
  (if (and (port? tm-zotero-socket-port)
           (not (port-closed? tm-zotero-socket-port)))
      (begin
        (close-port tm-zotero-socket-port)
        (set! tm-zotero-socket-port #f))))

;;;;;;
;;;
;;; Idempotency: If this is reloaded while TeXmacs is running, close the port
;;; on reload.  I often reload this file during development by having
;;; developer-mode turned on: (set! developer-mode? #t) is in
;;; ~/.TeXmacs/progs/my-init-texmacs.scm and then using the Debug -> Execute ->
;;; Evaluate scheme expression... menu to execute:
;;;
;;;    (load-from-path "tm-zotero.scm")
;;;
;;; (Actually, now that I think about it, I more often just run texmacs from a
;;; terminal where I can watch the debugging output, and use Ctrl-c to kill it,
;;; push up-arrow to recall the last command, and then enter to run it
;;; again. In another terminal I run firefox from the commandline also, so that
;;; I can use Ctrl-c to kill it, up-arrow, enter to run it again. The
;;; turnaround time is reasonably fast between changes to the code to fix the
;;; crash that prints to the terminal window.
;;;
(when (defined? 'tm-zotero-socket-port)
  (close-tm-zotero-socket-port!))       ;; free the IP port for re-use

(define tm-zotero-socket-port #f)


;;;;;;
;;;
;;; This is flag that will be set while the clipboard-cut operation is taking
;;; place, in an attempt to try and prevent the ztHrefFromCiteToBib fields
;;; inside of a selection about to be cut from being interned again right after
;;; uninterning them just prior to actually cutting the text out of the
;;; document.
;;;
(define fluid/is-during-tm-zotero-clipboard-cut? (make-fluid))
(fluid-set! fluid/is-during-tm-zotero-clipboard-cut? #f)

(tm-define (is-during-tm-zotero-clipboard-cut?)
  (fluid-ref fluid/is-during-tm-zotero-clipboard-cut?))

;;;;;;
;;;
;;; Allow the operating system to dynamically allocate the this-end port
;;; number, in case of multiple instances of TeXmacs running at the same time!
;;; (!define tm-zotero-socket-inet-texmacs-port-number 23117)
;;;
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


;;; TODO Mac OS-X and Windows HELP WANTED: Clone this on github, send me a pull
;;;      request... I do not own a computer with Windows nor do I own a Mac. I
;;;      can not easily develop this part. It should be fairly simple... and
;;;      there is actually a possibility that it will already just work without
;;;      any modifications. Somebody needs to test it... and please, if it
;;;      works, open a github "issue" with the positive report so I will
;;;      know. Thanks.
;;;
;;;
;;; !! Try this first !!
;;;
;;; Looking at the LibreOffice Integration plugin, I see that it's what opens
;;; up the TCP port that this talks to on Linux. That code does not check what
;;; OS it's running on first, and so I think that it opens the same TCP port on
;;; both Mac OS-X and Windows and so on those platforms, this program may
;;; already just work with no further programming required.
;;;
;;;;;;
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
;;;;;;
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
        (tm-zotero-format-error "_BOLD__RED_ERR: Exception caught in get-tm-zotero-socket-port:_RESET_ ~s" args)
        (close-port tm-zotero-socket-port)
        (set! tm-zotero-socket-port #f)
      (set-document-active-mark-nr! documentID #f)
      (dialogue-window
       (zotero-display-alert
        documentID
        `(document
           (concat "Exception caught in: " (zttextbf (zttexttt "get-tm-zotero-socket-port")) "\n")
           (document
             (concat (zttextbf "System Error:") " " ,(caar (cdddr args)) "\n")
             (document
               (concat (zttextbf "Is Juris-M or Zotero running?")
                       " If so, then you may need to " (zttextit "restart") " Firefox or Zotero Standalone.\n"))))
        DIALOG_ICON_STOP
        DIALOG_BUTTONS_OK)
       (lambda (val)
         (noop))
       "System Error in get-tm-zotero-socket-port")
      #f))))


;; (sigaction SIGPIPE
;;   (lambda (sig)
;;     (tm-zotero-set-message "SIGPIPE on tm-zotero-socket-port!")
;;     (hash-for-each
;;      (lambda (key val)
;;        (set-document-active-mark-nr! key #f))
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
  (tm-zotero-format-debug "_BOLD__RED_tm-zotero-write_RESET_: tid => ~s, cmd => ~s"
                          tid cmd)
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
          (tm-zotero-format-error "_BOLD__RED_ERR: System error in tm-zotero-write:_RESET_ tid => ~s, cmd => ~s" tid cmd)
          (tm-zotero-format-error "_BOLD__RED_ERR: Exception caught_RESET_: ~s\n" args)
          (tm-zotero-format-error "_BOLD__RED_ERR: Closing Zotero port!_RESET_\n")
          (close-tm-zotero-socket-port!)
          (set-document-active-mark-nr! documentID #f)
          (dialogue-window
           (zotero-display-alert
            documentID
            `(document
               (with "par-mode" "center"
                 (document
                   "Exception caught in: " (zttexttt "tm-zotero-write") "\n"
                   "\n"
                   (zttextbf "System Error:") " Is Zotero running?\n"
                   "\n"
                   "If so, then you may need to " (zttextit "restart")
                   " Firefox or Zotero Standalone.\n"
                   "\n"
                   (zttextbf "Closing Zotero port.") "\n")))
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
        (tm-zotero-format-error "_BOLD__RED_ERR: Exception caught in tm-zotero-read:_RESET_ ~s\n" args)
        (list
         (or tid 0)
         (or len 666)
         (format #f
           "ERR: System error in tm-zotero-read: ~s" args)))))) ;; return to tm-zotero-listen


(define (safe-json-string->scm str)
  (catch 'json-invalid
    (lambda ()
      (json-string->scm str))
    (lambda args
      (tm-zotero-format-error "_BOLD__RED_ERR: Exception caught from json-string->scm:_RESET_ ~s\n" args)
      ;; return to tm-zotero-listen
      (list (format #f "ERR: Invalid JSON: ~s\n" str) '()))))


(define* (safe-scm->json-string scm #:key (escape #f) (pretty #f))
  (catch #t
    (lambda ()
      (scm->json-string scm #:escape escape #:pretty pretty))
    (lambda args
      (tm-zotero-format-error "_BOLD__RED_ERR: Exception caught from scm->json-string:_RESET_ ~s\n" args)
      (tm-zotero-format-error "_BOLD__RED_ERR:_RESET_ scm => ~s\n" scm)
      ;;
      ;; Return ERR: to caller, usually tm-zotero-write, so send to Zotero.  That
      ;; will cause Zotero to initiate an error dialog and reset back to
      ;; Document_complete state.
      ;;
      ;; No colorized string here! The first argument's "ERR:" prefix is a
      ;; signal to Zotero. It must not be wrapped with colorizing ANSI terminal
      ;; escape codes.
      (format #f (string-append "ERR: Error! "
                                "Exception caught from scm->json-string \n\n"
                                "Exception args: ~s\n\n"
                                "scm: ~s\n")
              args scm))))



;;;;;;
;;;
;;; TODO after-gc-hook, Guardians, for tree-pointer ?
;;;      Will it leak without this? Does it belong here?
;;;
;;;;;;


;;{{{ Cleanup hooks for the undo mark handling

;;;
;;; info:(guile-1.8) * Hooks. (§ 5.9.6)
;;;
(define document-mark-cancel-error-cleanup-hook (make-hook 1))

(define (document-mark-cancel-and-error-cleanup documentID)
  ;; (tm-zotero-format-debug "document-mark-cancel-and-error-cleanup called, documentID => ~s" documentID)
  (let ((mark-nr (get-document-active-mark-nr documentID)))
    (when mark-nr
      (mark-cancel mark-nr) ; causes undo to happen
      (set-document-active-mark-nr! documentID #f)
      (run-hook document-mark-cancel-error-cleanup-hook documentID))))


(define document-mark-end-cleanup-hook (make-hook 1))

(define (document-mark-end-and-cleanup documentID)
  ;; (tm-zotero-format-debug "document-mark-end-and-cleanup called, documentID => ~s" documentID)
  (let ((mark-nr (get-document-active-mark-nr documentID)))
    (when mark-nr
      (mark-end mark-nr) ; causes undo to happen
      (set-document-active-mark-nr! documentID #f)
      (run-hook document-mark-end-cleanup-hook documentID))))

;;;;;;
;;;
;;; add-hook! pushes them onto the front of the hook list unless passed an
;;; optional third argument of #t, in which case it appends the new hook
;;; function to the end of the hook list.
;;;
(add-hook! document-mark-cancel-error-cleanup-hook
           cleanup-document-new-zfieldID!)

;;}}}

;;;;;;
;;;
;;; The protocol is essentially "synchronous" from the point of view of the
;;; user, who expects to wait while it finishes before doing anything
;;; else... But once an integration command has been sent, some of the
;;; "internal" parts of the protocol might be able to happen
;;; concurrently... See to-do item below.
;;;
;;; When this is entered, one of the Integration commands has just been sent to
;;; Juris-M / Zotero. Zotero will call back and begin a word processing command
;;; sequence, culminating with Document_complete.
;;;
;;;
;;; The document-active-mark-nr must only be set by
;;; call-zotero-integration-command, and only canceled (undo) or ended (keep) here.
;;;
;;;;;;
;;;
;;; TODO See if this can be improved to allow possible concurrency.
;;;
;;; In zotero/chrome/content/zotero/xpcom/integration.js, there is a comment
;;; inside of Zotero.Integration.Document.prototype.setDocPrefs that says
;;; "// Can get fields while dialog is open".
;;;
;;;;;;

(define (tm-zotero-listen cmd)
  ;; cmd is only used for set-message and debug display.
  (tm-zotero-format-debug "tm-zotero-listen:called:cmd => ~a" cmd)
  (let* ((documentID (get-documentID))
         (mark-nr (get-document-active-mark-nr documentID)))
    (tm-zotero-set-message
     (string-append "Waiting for response for " cmd "..."))
    (with (counter wait) '(40 10)
      (delayed
        (:while (get-document-active-mark-nr documentID))
        (:pause ((lambda () (inexact->exact (round wait)))))
        (:do (set! wait (min (* 1.01 wait) 2500)))
        ;; Only run when data is ready to be read...
        (when (char-ready? tm-zotero-socket-port)
          (with (tid len cmdstr) (tm-zotero-read)
            (tm-zotero-format-debug "_BOLD__RED_tm-zotero-listen_WHITE_:_GREEN_delayed read_WHITE_:_RESET_tid => ~s, len => ~s, cmdstr => ~s"
                                    tid len cmdstr)
            (if (> len 0)
                ;; then
                (with (editCommand args) (safe-json-string->scm cmdstr)
                  ;; (tm-zotero-format-debug "~s" (list editCommand (cons tid args)))
                  (cond
                    ((and (>= (string-length editCommand) 4)
                          (string=? (string-take editCommand 4) "ERR:"))
                     ;; editCommand is really an error string this time.
                     ;; (tm-zotero-format-debug "tm-zotero-listen:~s" editCommand)
                     ;;
                     ;; TODO Verify that this is correct protocol:
                     ;;
                     ;; Send the error (back) to Zotero !!! Huh? It just sent
                     ;; the error to us. Why send it back? Is this code
                     ;; incorrect?  Am I supposed to echo the error back to
                     ;; Zotero???
                     ;;
                     ;; Maybe Zotero resends an error after trying the
                     ;; Document_displayAlert first?
                     ;;
                     ;; Leaving it for now.
                     ;;
                     (tm-zotero-write tid editCommand)
                     ;; causes undo to happen
                     (document-mark-cancel-and-error-cleanup documentID)
                     ;;
                     ;; keep listening for Document_displayAlert and
                     ;; Document_complete.
                     ;;
                     (set! counter 40)
                     (set! wait 10)
                     wait
                     )
                    ((string=? editCommand "Document_complete") ; special case
                     ;; (tm-zotero-format-debug "tm-zotero-Document_complete:called...")
                     (tm-zotero-set-message-and-system-wait "Document complete!" soon-ready 2500)
                     (tm-zotero-write tid (scm->json-string '()))
                     ;; keep the changes unless already cancelled
                     (document-mark-end-and-cleanup documentID)
                     (set! wait 0)
                     wait
                     )
                    (else               ; We have an editCommand to process.
                      ;;
                      ;; (tm-zotero-set-message
                      ;;  (string-append "Processing command: " editCommand "..."))
                      ;;
                      ;; wrt document-active-mark-nr, it must not be altered
                      ;; here, since these are the intermediate edit commands
                      ;; that will culminate with Document_complete.
                      ;;
                      ;; Todo: This traps the event where there's a syntax or
                      ;;       other error in the zotero.scm program itself,
                      ;;       and send the ERR: message back to Zotero, and
                      ;;       set!  tm-zotero-active? #f, etc. in an attempt
                      ;;       to make it more robust, so that Firefox and
                      ;;       TeXmacs don't both have to be restarted when
                      ;;       this program doesn't work right?
                      ;;
                      ;; It did not work right. It just sits there and never
                      ;; prints the backtrace from the error to the terminal
                      ;; the way I expect, and so I can't debug it. Also,
                      ;; sending that ERR did not cause Juris-M to put up a
                      ;; dialog or anything so there's no indication of the
                      ;; error and the network protocol does not reset to the
                      ;; starting state anyway. Maybe the error condition needs
                      ;; to be noted and then handled with the next start of a
                      ;; command, so noted but tm-zotero-active? left #t until
                      ;; after the error handling?
                      ;;
                      ;; JavaScript error: file:///home/karlheg/.mozilla/firefox/yj3luajv.default/extensions/jurismOpenOfficeIntegration@juris-m.github.io/components/zoteroOpenOfficeIntegration.js, line 323: TypeError: can't access dead object
                      ;;
                      ;;(catch #t
                      ;;  (lambda ()
                      (apply (eval ;; to get the function itself
                              (string->symbol (string-append "tm-zotero-" editCommand))
                              (resolve-module '(tm-zotero)))
                             (cons tid args))
                      ;;  )
                      ;;  (lambda args
                      ;;    (tm-zotero-write tid (scm->json-string "ERR: TODO: Unspecified Error Caught."))
                      ;;    ...))
                      (set! counter 40)
                      (set! wait 10)
                      wait)))
                (begin ;; else (<= len 0) signals an error
                  ;;
                  ;; Sometimes when Firefox is stopped in the middle of it,
                  ;; char-ready? returns #t but tm-zotero-read does not read
                  ;; anything... Perhaps look for eof-object?
                  ;;
                  (set! counter (- counter 1))
                  (when (<= counter 0)
                    ;; causes undo to happen
                    (document-mark-cancel-and-error-cleanup documentID)
                    (close-tm-zotero-socket-port!)
                    (set! wait 0)
                    wait)))))))))

;;}}}

;;{{{ Integration-initiating commands: TeXmacs -> Zotero

;;;;;;
;;;
;;; These expect no immediate reply packet from Zotero. Zotero will connect
;;; back with Editor integration commands, while tm-zotero (this program) is
;;; sort-of "in" tm-zotero-listen (in the sense that a transaction is taking
;;; place between texmacs and zotero until the Document_complete is read.
;;;
;;;   See: init-tm-zotero.scm
;;;   See: tm-zotero-menu.scm
;;;   See: tm-zotero-kbd.scm
;;;
;;; All of the menu commands and keyboard commands that invoke tm-zotero are
;;; called via this function. This is where the transaction is initiated. The
;;; document-active-mark-nr is for the undo mechanism. The bottom half of the
;;; undo transaction support is of course found in tm-zotero-listen.
;;;
;;;   See: (utils library tree) try-modification
;;;
(define (call-zotero-integration-command cmd)
  (let ((documentID (get-documentID)))
    (when (not (get-document-active-mark-nr documentID)) ;; one at a time only
      (let ((zp (get-tm-zotero-socket-port))
            (mark-nr (mark-new)))       ; for "atomic undo" on failure
        (if (and (port? zp)
                 (catch 'system-error
                   (lambda ()
                     (tm-zotero-write 0 (safe-scm->json-string cmd))
                     #t)
                   (lambda arg
                     #f))) ;; Firefox or Zotero Standalone not running?
            (begin
              ;; Set up the "undo" transaction:
              (set-document-active-mark-nr! documentID mark-nr)
              (mark-start mark-nr)
              (archive-state)
              ;; Listen for incoming commands.
              (tm-zotero-listen cmd) ;; delayed, returns immediately.
              #t) ;; report successful initiation of integration command sequence
            (begin
              #f))))))



(define (tm-zotero-add str-kind)
  (let* ((documentID (get-documentID))
         (new-zfield-zfd (get-document-new-zfield-zfd documentID)))
    (unless new-zfield-zfd              ; one at a time only
      (cond
        ((== str-kind "citation")
         (insert-new-zfield 'zcite "{Citation}")
         (call-zotero-integration-command "addCitation"))
        ((== str-kind "bibliography")
         (insert-new-zfield 'zbibliography "{Bibliography}")
         (call-zotero-integration-command "addBibliography"))))))



;;; ---------

(define-public (tm-zotero-addCitation)
  (tm-zotero-add "citation"))

(define-public (tm-zotero-editCitation)
  (call-zotero-integration-command "editCitation"))

;;; ---------

(define-public (tm-zotero-addBibliography)
  (tm-zotero-add "bibliography"))

(define-public (tm-zotero-editBibliography)
  (call-zotero-integration-command "editBibliography"))


;;; ---------


(define-public (tm-zotero-refresh)
  (call-zotero-integration-command "refresh"))


;;; (define-public (tm-zotero-removeCodes)
;;;   (call-zotero-integration-command "removeCodes"))


;;; ---------


(define-public (tm-zotero-setDocPrefs)
  (call-zotero-integration-command "setDocPrefs"))

;;}}}

;;{{{ Word Processor commands: Zotero -> TeXmacs -> Zotero

;;;;;;
;;;
;;; Each sends: [CommandName, [Parameters,...]].
;;;
;;; The response is expected to be a JSON encoded payload, or the unquoted and
;;; unescaped string: ERR: Error string goes here
;;;
;;;;;;

;;{{{ Application_getActiveDocument
;;;
;;; Gets information about the client and the currently active
;;; document. documentID can be an integer or a string.
;;;
;;; ["Application_getActiveDocument", [int_protocolVersion]] -> [int_protocolVersion, documentID]
;;;
;;; For now it ignores the protocol version.
;;;
(define (tm-zotero-Application_getActiveDocument tid pv)
  (tm-zotero-set-message "Processing command: Application_getActiveDocument...")
  ;;(tm-zotero-format-debug "zotero-Application_getActiveDocument:called...")
  (tm-zotero-write tid (safe-scm->json-string (list pv (get-documentID)))))

;;}}}

;;{{{ Document_displayAlert

;;;
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


(tm-widget ((zotero-display-alert documentID stree_TextBody int_Icon int_Buttons) cmd)
  (centered
    (hlist ((icon (list-ref (map %search-load-path
                                 '("icon-stop-48.png"
                                   "icon-notice-48.png"
                                   "icon-caution-48.png"))
                            int_Icon)) (noop))
           >> (texmacs-output `(document
                                 (with "page-width" "8.0in"
                                   "margin-top"  "0.25in" "margin-bottom" "0.25in"
                                   "margin-left" "0.25in" "margin-right"  "0.25in"
                                   "par-mode" "justify" "par-hyphen" "professional"
                                   (surround "" (right-flush)
                                             (document ,stree_TextBody))))
                              `(style (tuple "generic" "tm-zotero")))))
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
;;;
;;; Shows an alert.
;;;
;;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;;
(define (tm-zotero-Document_displayAlert tid documentID str_dialogText int_icon
                                         int_buttons)
  (tm-zotero-set-message "Processing command: Document_displayAlert...")
  ;;(tm-zotero-format-debug "tm-zotero-Document_displayAlert:called...")
  (let ((stree_dialogText (tree->stree
                           (tm-zotero-UTF-8-str_text->texmacs str_dialogText #f #f))))
    (dialogue-window (zotero-display-alert documentID stree_dialogText int_icon int_buttons)
                     (lambda (val)
                       (tm-zotero-write tid (safe-scm->json-string val)))
                     "Zotero Alert!")))

;;}}}
;;{{{ Document_activate
;;;
;;; Brings the document to the foreground.
;;;  (For OpenOffice, this is a no-op on non-Mac systems.)
;;;
;;; ["Document_activate", [documentID]] -> null
;;;
(define (tm-zotero-Document_activate tid documentID)
  (tm-zotero-set-message "Processing command: Document_activate...")
  ;;(tm-zotero-format-debug "tm-zotero-Document_activate:called...")
  (wait-update-current-buffer)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_canInsertField
;;;
;;; Indicates whether a field can be inserted at the current cursor position.
;;;
;;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;;
(define (tm-zotero-Document_canInsertField tid documentID str_fieldType)
  (tm-zotero-set-message "Processing command: Document_canInsertField...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_canInsertField:called...")
  (let ((ret (not
              (not
               (and (in-text?)
                    (not (in-math?))
                    (if (focus-is-zfield?)
                        (let ((zfield (focus-tree)))
                          ;; (tm-zotero-format-debug
                          ;;  "tm-zotero-Document_canInsertField:focus-is-zfield? => #t, document-new-zfieldID => ~s, (focus-tree) => ~s"
                          ;;  (document-new-zfieldID documentID)
                          ;;  zfield)
                          ;; Ok if zfield is the newly being-inserted zfield.
                          (if (zfield-is-document-new-zfield? documentID zfield)
                              #t
                              #f))
                        #t))))))
    (tm-zotero-write tid (safe-scm->json-string ret))))

;;}}}
;;{{{ Document_getDocumentData
;;;
;;; Retrieves data string set by setDocumentData.
;;;
;;; ["Document_getDocumentData", [documentID]] -> str_dataString
;;;
(define (tm-zotero-Document_getDocumentData tid documentID)
  (tm-zotero-set-message "Processing command: Document_getDocumentData...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_getDocumentData:called...")
  (tm-zotero-write tid (safe-scm->json-string (get-env-zoteroDocumentData))))

;;}}}
;;{{{ Document_setDocumentData
;;;
;;; Stores a document-specific persistent data string. This data
;;; contains the style ID and other user preferences.
;;;
;;; ["Document_setDocumentData", [documentID, str_dataString]] -> null
;;;
(define (tm-zotero-Document_setDocumentData tid documentID str_dataString)
  (tm-zotero-set-message "Processing command: Document_setDocumentData...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_setDocumentData:called...")
  (set-env-zoteroDocumentData! str_dataString)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_cursorInField
;;;
;;; Indicates whether the cursor is in a given field. If it is, returns
;;; information about that field. Returns null, indicating that the cursor
;;; isn't in a field of this fieldType, or a 3 element array containing:
;;;
;;;   zfieldID, int or string, A unique identifier corresponding to this field.
;;;
;;;   fieldCode, UTF-8 string, The code stored within this field.
;;;
;;;   noteIndex, int, The number of the footnote in which this field resides,
;;;                   or 0 if the field is not in a footnote.
;;;
;;; ["Document_cursorInField", [documentID, str_fieldType]] -> null || [fieldID, fieldCode, int_noteIndex]
;;;
;;;   str_fieldType is ignored for now.
;;;
(define (tm-zotero-Document_cursorInField tid documentID str_fieldType)
  (tm-zotero-set-message "Processing command: Document_cursorInField...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_cursorInField:called...")
  (let ((ret (if (focus-is-zfield?)
                 (begin
                   ;; (tm-zotero-format-debug "tm-zotero-Document_cursorInField: focus-is-zfield? => #t")
                   (let* ((zfield (focus-tree))
                          (zfieldID (zfield-zfieldID zfield)))
                     (if (not (zfield-is-document-new-zfield? documentID zfieldID))
                         (begin
                           (let ((zfieldCode (zfield-Code-code zfield))
                                 (noteIndex (zfield-NoteIndex zfield)))
                             ;; (tm-zotero-format-debug
                             ;;  "tm-zotero-Document_cursorInField:id:~s:code:~s:ni:~s"
                             ;;  zfieldID zfieldCode noteIndex)
                             (list zfieldID zfieldCode noteIndex)))
                         '()))) ;; is the new field not finalized by Document_insertField
                 '()))) ;; focus is not a zfield.
    (tm-zotero-write tid (safe-scm->json-string ret))))

;;}}}
;;{{{ Document_insertField
;;;
;;; Inserts a new field at the current cursor position.
;;;
;;; Because there has to be time for the typesetting to run in order for it to
;;; create the footnote number and set the reference bindings for the
;;; noteIndex, by the time this routine is being called by Zotero, TeXmacs must
;;; have already inserted the new field (See: insert-new-zfield) in a pending
;;; state. That tentative new zfield is finalized by this function and promoted
;;; to a normal zfield, rather than the new one.
;;;
;;; tm-zotero cannot keep track of the noteIndex itself since it's not the only
;;; thing inserting footnotes. The user can insert them too, and so either this
;;; would have to keep track of those... but that's not necessary and is too
;;; costly... It naturally lets the typesetter run between insert-new-zfield
;;; and tm-zotero-Document_insertField due to the "delay" form in
;;; tm-zotero-listen, and so that typsetter run sets up the reference binding
;;; (by expanding the zcite macros when-where-in the set-binding calls will
;;; happen) so we can look up the noteIndex through the TeXmacs
;;; typesetter. See: get-refbinding, and zfield-NoteIndex-str.
;;;
;;;
;;;   str_fieldType, either "ReferenceMark" or "Bookmark"
;;;   int_noteType, NOTE_IN_TEXT, NOTE_FOOTNOTE, or NOTE_ENDNOTE
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
  (tm-zotero-set-message "Processing command: Document_insertField...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_insertField:called...")
  (let* ((new-zfield-zfd (get-document-new-zfield-zfd documentID))
         (new-zfieldID (and new-zfield-zfd
                            (the-zfieldID-of new-zfield-zfd)))
         (new-zfield (and new-zfield-zfd
                          (zfd-tree new-zfield-zfd)))
         (new-noteIndex (and new-zfieldID
                             (zfield-NoteIndex new-zfieldID))))
    ;; (describe new-zfield-zfd)
    (if new-zfield-zfd
        ;; then
        (begin
          ;; Just in case somebody moved the cursor during the GUI mainloop
          ;; while waiting for Zotero transactions to take place...
          (tree-go-to new-zfield 1)
          ;; clear document-new-zfield-zfd
          (set-document-new-zfield-zfd! documentID #f)
          ;;
          ;; Add it to the <document-data>-*
          ;;
          ;; This is done explicitly here rather than lazily by
          ;; tm-zotero-ext:ensure-zfield-interned! because in this case, the
          ;; <zfield-data> for this zfield already exists... and I want to be
          ;; sure that it's in the list for the about to happen call for the
          ;; list of fields via Document_getFields.
          ;;
          (hash-set! (get-document-zfield-zfd-ht documentID) new-zfieldID new-zfield-zfd)
          (document-merge!-<zfield-data> new-zfield-zfd)
          (when (is-zbibliography? new-zfield)
            (document-merge!-zbibliography-zfd new-zfield-zfd))
          ;; Report success to Zotero.
          (tm-zotero-write tid (safe-scm->json-string
                                (list new-zfieldID ""
                                      new-noteIndex)))
          )
        ;; else
        (tm-zotero-write tid (safe-scm->json-string "ERR:no new-zfield in tm-zotero-Document_insertField???")))))

;;}}}
;;{{{ Document_getFields

;;;
;;; Get all fields present in the document, in document order.
;;;
;;;   str_fieldType is the type of field used by the document, either
;;;                    ReferenceMark or Bookmark
;;;
;;; ["Document_getFields", [documentID, str_fieldType]] -> [[fieldID, ...], [fieldCode, ...], [noteIndex, ...]]
;;;
;;;
;;;  A protocol trace watching the traffic between Libreoffice and Zotero shows
;;;  that the BIBL field is also sent as one of the fields in this list.
;;;
(define (tm-zotero-Document_getFields tid documentID str_fieldType)
  (tm-zotero-set-message "Processing command: Document_getFields...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_getFields:called...")
  (let ((ret
         (let loop ((zfield-zfd-ls (get-document-zfield-zfd-ls documentID)) ; list of <zfield-data>.
                    (ids '()) (codes '()) (indx '()))
           ;; (tm-zotero-format-debug "tm-zotero-Document_getFields:zfield-zfd-ls => ~s" zfield-zfd-ls)
           ;; (tm-zotero-format-debug "tm-zotero-Document_getFields: ~s" (map (lambda (zfd)
           ;;                                                                     (if zfd
           ;;                                                                         (if (tree-pointer zfd)
           ;;                                                                             (list
           ;;                                                                              (the-zfieldID-of zfd)
           ;;                                                                              (zfield-zfieldID (zfd-tree zfd)))
           ;;                                                                             "No tree pointer in zfd?")
           ;;                                                                         "No zfd?"))
           ;;                                                                   zfield-zfd-ls))
           (cond
             ((null? zfield-zfd-ls) (if (nnull? ids)
                                        (list (reverse! ids)
                                              (reverse! codes)
                                              (reverse! indx))
                                        '((0) ("TEMP") (0))))
             (else
               (let* ((zfd (car zfield-zfd-ls))
                      (zfield (and zfd (eq? <zfield-data> (class-of zfd)) (zfd-tree zfd)))
                      (zfieldID (and zfield (zfield-zfieldID zfield)))
                      (code (and zfield (zfield-Code-code zfield)))
                      (noteIndex (and zfieldID (zfield-NoteIndex zfieldID))))
                 (if zfieldID
                     (loop (cdr zfield-zfd-ls)
                           (cons zfieldID ids)
                           (cons code codes)
                           (cons noteIndex indx))
                     (loop (cdr zfield-zfd-ls)
                           ids
                           codes
                           indx))))))))
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
  (tm-zotero-set-message "Processing command: Document_convert...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_convert:called...")
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
;;;;;;
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
;;;
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

(define (careful-divide a b)
  (let ((a (exact->inexact (round (* a 1000000))))
        (b (exact->inexact (round (* b 1000000)))))
    (/ a b)))

;;(define (careful

(define (tm-zotero-lineSpacing->tmlen meas)
  (let* ((meas (exact->inexact meas))
         (sep-mult (careful-divide (if (= meas 0) 240.0 meas)
                      240.0
                      ;; 480.0
                      )))
    (format #f "~,4f" sep-mult))) ;; times par-sep

(define (tm-zotero-entrySpacing->tmlen meas)
  (let* ((meas (exact->inexact meas))
         (sep-mult (careful-divide (if (= meas 0) 240.0 meas)
                     240.0)))
    (format #f "~,4f" sep-mult))) ;; times item-vsep

(define (tm-zotero-firstLineIndent->tmlen meas)
  (let* ((meas (exact->inexact meas))
         (indent-tabs (careful-divide meas 360.0))) ; can be zero
    (format #f "~,4ftab" indent-tabs)))

(define (tm-zotero-bodyIndent->tmlen meas)
  (let* ((meas (exact->inexact meas))
         (indent-tabs (careful-divide meas 360.0))) ; can be zero
    (format #f "~,4ftab" indent-tabs)))


(define (tm-zotero-tabstop-arrayList->tmlen-ls tab-ls)
  (let loop ((tab-ls tab-ls)
             (ret '()))
    (cond
     ((null? tab-ls)
      (stree->tree `(tuple ,@(reverse! ret))))
     (else
       (loop (cdr tab-ls)
             (cons (format #f "~,4ftab"
                           (careful-divide (exact->inexact (car tab-ls))
                              360.0))
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
  (tm-zotero-set-message "Processing command: Document_setBibliographyStyle...")
  ;;(tm-zotero-format-debug "tm-zotero-Document_setBibliographyStyle:called...")
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
;;;
;;; Not documented, but exists in CommMessage.java in LibreOffice side of the
;;; connector. It appears to do nothing there either.
;;;
(define (tm-zotero-Document_cleanup tid documentID)
  (tm-zotero-set-message "Processing command: Document_cleanup...")
  (tm-zotero-format-debug "STUB:tm-zotero-Document_cleanup: ~s" documentID)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_complete (see tm-zotero-listen)
;;;
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
;;;   (set-document-active-mark-nr! documentID #f)
;;;   ;; (close-tm-zotero-socket-port!)
;;;   )

;;}}}

;;{{{ Field_delete
;;;
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
  (tm-zotero-set-message
   (string-append "Processing command: Field_delete " zfieldID "..."))
  ;;(tm-zotero-format-debug "tm-zotero-Field_delete:called...")
  (let* ((zfd (hash-ref (get-document-zfield-zfd-ht documentID) zfieldID))
         (zfield (and zfd (zfd-tree zfd)))
         (code (and zfield (zfield-Code-code zfield)))
         (text (and zfield (zfield-Text zfield))))
    (when zfield
      (hash-remove! (get-document-zfield-zfd-ht documentID) zfieldID)
      (document-remove!-<zfield-data> zfd)
      (when (is-zbibliography? zfield)
        (document-remove!-zbibliography-zfd zfd))
      (clear-tree-pointer zfd)
      (tree-set! zfield "")))
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_select
;;;
;;; Moves the current cursor position to encompass a field.
;;;
;;; ["Field_select", [documentID, fieldID]] -> null
;;;
;;; I think that whether or not this works as expected depends on settings made
;;; by the drd-props macro. I think that I want the cursor to be inside of it's
;;; light blue box, after it.... (writing this comment prior to testing. FLW.)
;;;
(define (tm-zotero-Field_select tid documentID zfieldID)
  (tm-zotero-set-message
   (string-append "Processing command: Field_select " zfieldID "..."))
  ;;(tm-zotero-format-debug "tm-zotero-Field_select:called...")
  (go-to-document-zfield-by-zfieldID documentID zfieldID)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_removeCode
;;;
;;; ["Field_removeCode", [documentID, fieldID]] -> null
;;;
(define (tm-zotero-Field_removeCode tid documentID zfieldID)
  (tm-zotero-set-message
   (string-append "Processing command: Field_removeCode " zfieldID "..."))
  ;;(tm-zotero-format-debug "tm-zotero-Field_removeCode:called...")
  (set! (zfield-Code-code (get-document-zfield-by-zfieldID documentID zfieldID)) "")
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
  ;;(tm-zotero-format-debug "move-link-to-own-line:called, lnk => ~s" lnk)
  (let* ((pre-lnk-txt (tree-ref (tree-up lnk) (- (tree-index lnk) 1)))
         (pre-lnk-str (and pre-lnk-txt (tree->stree pre-lnk-txt)))
         (post-lnk-txt (tree-ref (tree-up lnk) (+ (tree-index lnk) 1)))
         (post-lnk-str (and post-lnk-txt (tree->stree post-lnk-txt)))
         (is-doi? (and (string? pre-lnk-str)
                       (or (string-suffix? "doi:" pre-lnk-str)
                           (string-suffix? "doi: " pre-lnk-str)
                           (string-suffix? "doi: " pre-lnk-str)
                           (string-suffix? "DOI:" pre-lnk-str)
                           (string-suffix? "DOI: " pre-lnk-str)
                           (string-suffix? "DOI: " pre-lnk-str)))))
    ;; (tm-zotero-format-debug "lnk before: ~s" lnk)
    ;; (tm-zotero-format-debug "pre-lnk-str: ~s" pre-lnk-str)
    ;; (tm-zotero-format-debug "post-lnk-str: ~s" post-lnk-str)
    (unless is-doi?
      ;; (tm-zotero-format-debug "is-doi? => #f")
      (when (string? pre-lnk-str)
        (cond
          ((and (string? post-lnk-str)  ;; translation error hack hack hack
                (string-suffix? "<" pre-lnk-str)
                (string-prefix? ">" post-lnk-str))
           (set! pre-lnk-str (substring pre-lnk-str
                                        0
                                        (- (string-length pre-lnk-str)
                                           1)))
           (tree-set! pre-lnk-txt (stree->tree pre-lnk-str))
           (set! post-lnk-str (substring post-lnk-str
                                         1
                                         (string-length post-lnk-str)))
           (tree-set! post-lnk-txt (stree->tree post-lnk-str))
           (tree-set! lnk (stree->tree
                           `(concat (next-line)
                                    (small (concat (less-than-sign) ,lnk (greater-than-sign)))))))
          ((and (string? post-lnk-str)
                (string-suffix? "<less>" pre-lnk-str)
                (string-prefix? "<gtr>" post-lnk-str))
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
                                    (small (concat (less-than-sign) ,lnk (greater-than-sign)))))))
          ((and (string? post-lnk-str)  ;; translation error hack hack hack
                (string-suffix? "<less>less<gtr>" pre-lnk-str)
                (string-prefix? "<less>gtr<gtr>" post-lnk-str))
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
                                    (small (concat (less-than-sign) ,lnk (greater-than-sign)))))))
          ((or (and (string-suffix? "DOI:http://doi.org/"     pre-lnk-str) "DOI:http://doi.org/")
               (and (string-suffix? "doi:http://doi.org/"     pre-lnk-str) "doi:http://doi.org/")
               (and (string-suffix? "http://doi.org/"         pre-lnk-str) "http://doi.org/")
               (and (string-suffix? "DOI:https://doi.org/"    pre-lnk-str) "DOI:https://doi.org/")
               (and (string-suffix? "doi:https://doi.org/"    pre-lnk-str) "doi:https://doi.org/")
               (and (string-suffix? "https://doi.org/"        pre-lnk-str) "https://doi.org/")
               (and (string-suffix? "DOI:http://dx.doi.org/"  pre-lnk-str) "DOI:http://dx.doi.org/")
               (and (string-suffix? "doi:http://dx.doi.org/"  pre-lnk-str) "doi:http://dx.doi.org/")
               (and (string-suffix? "http://dx.doi.org/"      pre-lnk-str) "http://dx.doi.org/")
               (and (string-suffix? "DOI:https://dx.doi.org/" pre-lnk-str) "DOI:https://dx.doi.org/")
               (and (string-suffix? "doi:https://dx.doi.org/" pre-lnk-str) "doi:https://dx.doi.org/")
               (and (string-suffix? "https://dx.doi.org/"     pre-lnk-str) "https://dx.doi.org/")
               )
           => (lambda (lnstr)
                ;; Keep link next to the prefix text.
                ;;(tm-zotero-format-debug "Keep link next to the prefix text.")
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
            ;;(tm-zotero-format-debug "Punctuation: '~s'" (car strs))
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
    ;; (tm-zotero-format-debug "move-link-to-own-line returning, lnk => ~s" lnk)
    )
  lnk)


;; (define (delete-one-space-to-left-of lnk)
;;   (tm-zotero-format-debug "delete-one-space-to-left-of called.")
;;   (let* ((pre-lnk-txt (tree-ref (tree-up lnk) (- (tree-index lnk) 1)))
;;          (pre-lnk-str (and pre-lnk-txt (tree->stree pre-lnk-txt))))
;;     (when (or (string-suffix? " " pre-lnk-str)
;;               (string-suffix? " " pre-lnk-str))
;;       (set! pre-lnk-str (substring pre-lnk-str
;;                                    0
;;                                    (- (string-length pre-lnk-str)
;;                                       1)))
;;       (tree-set! pre-lnk-txt (stree->tree pre-lnk-str)))))



(define (fixup-embedded-slink-as-url lnk)
  ;;(tm-zotero-format-debug "_GREEN_fixup-embedded-slink-as-url_WHITE_:called._RESET_ ~s" (tree->stree lnk))
  (cond
    ((and (tree-in? lnk '(ztHrefFromBibToURL ztHrefFromCiteToBib))
          (tree-in? (tree-ref lnk 1) '(slink verbatim)))
     (let ((slink-or-verbatim (tree-ref lnk 1)))
       (tree-set! slink-or-verbatim (tree-ref slink-or-verbatim 0))))
    ((and (tree-in? lnk '(ztHref))
          (tree-in? (tree-ref lnk 0) '(slink verbatim)))
     (let ((slink-or-verbatim (tree-ref lnk 0)))
       (tree-set! slink-or-verbatim (tree-ref slink-or-verbatim 0)))))
  ;;(tm-zotero-format-debug "_GREEN_fixup-embedded-slink-as-url_WHITE_:returning._RESET_ ~s" (tree->stree lnk))
  lnk)

;; (define (fixup-embedded-slink-as-url lnk)
;;   (when (match? lnk '((:or ztHrefFromBibToURL ztHrefFromCiteToBib) :%1 ((:or slink verbatim) :%1)))
;;     (tree-set! lnk `(,(tree-label lnk) ,(tree-ref lnk 0) ,(tree-ref lnk 1 0)))))

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

;;;;;;
;;;
;;; TODO Perhaps this ought to be configurable, by making it possible for the
;;;      user to put their own ones into a separate configuration file?
;;;
(define tm-zotero-regex-replace-clauses
  (map (lambda (elt)
         (cons (apply make-regexp `,(car elt))
               (cdr elt)))
       ;;
       ;; Remember that these execute one after the next, and are applied using regexp-substitute/global, so they must contain
       ;; 'post' as an element in order to have them work on the entire string.
       ;;
       `(
         ;; (("(\r\n)")
         ;;  pre "\n" post)
         ;; The standard "integration.js" sends RTF, which uses \r\n pairs. Turn them to \n only.
         ;;
         ;; Template
         ;;
         ;;(("")
         ;; pre "" post);; comment
         ;;
         (("(``)")
          pre "“" post)
         (("('')")
          pre "”" post)
         (("(<varspace>)")
          pre " " post)
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
         (("((.*)\\2X-X-X([  ]|\\hspace.[^}+].)?)") ;; RepeatRepeatX-X-X to delete. Hopefully won't affect sort-order much.
          pre post)
         (("(X-X-X([  ]|\\hspace.[^}]+.)?)")
          pre post)
         (("(([  ]|\\hspace.[^}]+.)?\\(([  ]|\\hspace.[^}]+.)*\\))") ;; empty parentheses and space before them (but NOT period or space after).
          pre post)
         (("(.*000000000@#(.ztbib[A-Za-z]+.*})}.*\\.?}%?)" ,regexp/newline)
          pre 2 post) ;; Category heading dummy entries. Replaces the entire line!
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
         ;; (("(<doi:)")
         ;;  pre "\\ztlt{}doi:" post)
         ;; (("(}>)")
         ;;  pre "}\\ztgt{}" post)
         ;;
         ;; Todo: Fix this in citeproc.js (bibliography for collapsed parallel citation) When a legal case is cited twice in a row
         ;; in a citation cluster, they are collapsed into a parallel citation. With Indigobook, the in-text citation looks perfect,
         ;; but for some reason the one in the bibliography has a ., between the two different reporters, rather than only a , so
         ;; this hack cleans that up.
         ;;
         ;; (("(\\.,)")    ;; No... e.g., 
         ;;  pre "," post)
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
         (("(Dr\\.|Mr\\.|Mrs\\.|Jr\\.|PhD\\.|Jd\\.|Md\\.|Inc\\.|Cir\\.|Sup\\.|Ct\\.|App\\.|U\\.|Const\\.)")
          pre "\\abbr{" 1 "}" post)
         (("([Aa]rt\\.|[Ss]ec\\.|[Cc]h\\.|[Pp]ara\\.|[Nn]o\\.|[Rr]ev\\.|[Ee]d\\.)")
          pre "\\abbr{" 1 "}" post)
         (("(Cal\\.|Kan\\.|Mass\\.)")
          pre "\\abbr{" 1 "}" post)
         (("(Envtl\\.|Loy\\.)")
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
  ;;(tm-zotero-format-debug "_BOLD__RED_tm-zotero-regex-transform_WHITE_:_GREEN_called_RESET_, str_text => ~s" str_text)
  (let ((text str_text))
    (do ((rc tm-zotero-regex-replace-clauses (cdr rc)))
        ((null? rc)
         ;;(tm-zotero-format-debug "_BOLD__RED_tm-zotero-regex-transform_WHITE_:_GREEN_returning_RESET_, text => ~s" text)
         text)
      ;; each is applied in turn, so later ones can modify results of earlier
      ;; ones if you like.
      ;;(tm-zotero-format-debug "_BOLD__RED_tm-zotero-regex-transform_WHITE_:  _GREEN_during_WHITE_:  _GREEN_text_RESET_: ~s" text)
      (set! text (apply regexp-substitute/global `(#f ,(caar rc) ,text ,@(cdar rc)))))))


;; (define (tm-zotero-regex-transform str_text)
;;   (let loop ((str_text str_text)
;;              (rc tm-zotero-regex-replace-clauses))
;;     (if (null? rc)
;;         str_text
;;         (loop
;;          (apply regexp-substitute/global `(#f ,(caar rc) ,str_text ,@(cdar rc)))
;;          (cdr rc)))))


(cond-expand
  (guile-2
   (define creturn #\return))
  (else
    (define creturn #\cr)))
(define cnewline #\newline)

;;;
;;; This runs for both in-text or note citations as well as for the
;;; bibliography.
;;;
;;;;;;
;;;
;;; TODO Instead of having zotero's xpcom/integration.js paste together all of
;;;      the bibliography entries into one long string that I then go and split
;;;      again here, for bbl output format, it can instead send back a JSON
;;;      representation of the list of bibliography entries. Why paste them
;;;      there then split them here when it can just pass the list?
;;;
;;;;;;
;;;
;;; TODO When it is receiving a list of bibliography items, when that list is
;;;      for the entire bibliography but only some parts of it have actually
;;;      changed, then it doesn't really need to re-run the regexp transform
;;;      and LaTeX parsing on the items that have not changed.
;;;
;;;        It knows which items have changed...  insert-new-zfield,
;;;        tm-zotero-Document_insertField, notify-activate,
;;;        clipboard-cut... (any others?) can keep that information up to
;;;        date.
;;;
;;;;;;
;;;
(define (tm-zotero-UTF-8-str_text->texmacs str_text is-note? is-bib?)
  ;; (tm-zotero-set-message-and-system-wait
  ;;  "Munging, transcoding, and parsing input..." please-wait)
  ;; (noop)
  (tm-zotero-set-message-and-system-wait ; try to force refresh so it is readable
   "Munging, transcoding, and parsing input..." please-wait)
  ;; (tm-zotero-format-debug "_GREEN_tm-zotero-UTF-8-str_text->texmacs_RESET_: called, str_text => ~s, is-note? => ~s, is-bib? => ~s" str_text is-note? is-bib?)
  ;;
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
         (strls (string-split str_text creturn))
         (strls (map (cut string-trim <> cnewline) strls))
         (strls (map tm-zotero-regex-transform strls))
         ;; Q: What advantage would there be to have parse-latex accept a
         ;; UTF-8, rather than Cork encoded, string?
         (str_text (string-convert
                    (string-join strls "\n")
                    "UTF-8" "Cork"))
         (t (latex->texmacs (parse-latex str_text)))
         (b (buffer-new)))
    ;;(tm-zotero-format-debug "_GREEN_tm-zotero-UTF-8-str_text->texmacs_RESET_: after let*. !!!")
    (buffer-set-body b t) ;; This is magical.
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    ;;
    ;; Used from inside tm-zotero.ts
    ;;
    ;; It turns out that tm-select will return these not in tree or document
    ;; order.  For this function, that's alright.
    ;;
    (map (lambda (lnk)
           (when (or is-note? is-bib?)
             (move-link-to-own-line lnk)))
         (select t '(:* (:or ztHref hlink href))))
    ;;
    ;; from propachi-texmacs/bootstrap.js monkeypatch VariableWrapper
    ;;
    (map (lambda (lnk)
           ;;(tm-zotero-format-debug "_GREEN_tm-zotero-UTF-8-str_text->texmacs_RESET_:_BOLD__YELLOW_fixup-slink-as-url_RESET_ lnk => ~s" (tree->stree lnk))
           (tree-set! lnk (fixup-embedded-slink-as-url lnk)))
         (select t '(:* (:or ztHrefFromBibToURL ztHrefFromCiteToBib ztHref))))
    ;; (tm-zotero-format-debug "_GREEN_tm-zotero-UTF-8-str_text->texmacs_RESET_:_BOLD_before tree-simplify_RESET_")
    (tree-simplify t)
    ;; (tm-zotero-format-debug "_GREEN_tm-zotero-UTF-8-str_text->texmacs_RESET_:_BOLD_after tree-simplify_RESET_")
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    (buffer-close b)
    (recall-message)
    ;;(tm-zotero-format-debug "_GREEN_tm-zotero-UTF-8-str_text->texmacs_RESET_:_BOLD__GREEN_returning_RESET_ => ~s" (tree->stree t))
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
  ;; (tm-zotero-format-debug "zfield-IsBib? called... zfield label:~s"
  ;;                  (tree-label zfield))
  (tree-is? zfield 'zbibliography))


;;;
;;; Input is a field tree, already found.
;;;
(define (zfield-IsNote? zfield)
  ;; (tm-zotero-format-debug "zfield-IsNote?:called...")
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
  (tm-zotero-set-message
   (string-append "Processing command: Field_setText " zfieldID "..."))
  (tm-zotero-format-debug "tm-zotero-Field_setText:called...")
  (let* ((zfield   (get-document-zfield-by-zfieldID documentID zfieldID))
         (is-note? (and zfield (zfield-IsNote? zfield)))
         (is-bib?  (and zfield (zfield-IsBib? zfield)))
         (tmtext
          (tm-zotero-UTF-8-str_text->texmacs str_text is-note? is-bib?)))
    ;;;;(tm-zotero-format-debug "tm-zotero-Field_setText: about to set zfield-Text-t")
    ;;; It was crashing here at the destructor for tree-pointer. I think it's
    ;;; the tree-pointers on the ztHrefFromCiteToBib tags.
    ;;;;(set! (zfield-Text-t zfield) tmtext)
    ;;
  (with-fluids
      ((fluid/is-during-tm-zotero-clipboard-cut? #t))
    ;; (begin
    ;;   (set! inside-tm-zotero-clipboard-cut #t) ;; TODO fluid-ref
    ;;(tm-zotero-format-debug "tm-zotero-Field_setText: about to unintern-ztHrefFromCiteToBib-for-cut")
    (unintern-ztHrefFromCiteToBib-for-cut documentID zfield)
    ;;(tm-zotero-format-debug "tm-zotero-Field_setText: about to set zfield-Text-t")
    (set! (zfield-Text-t zfield) tmtext))
  ;; (set! inside-tm-zotero-clipboard-cut #f))
  ;;(tm-zotero-format-debug "tm-zotero-Field_setText: about to set zfield-Code-origText")
  (set! (zfield-Code-origText zfield) tmtext)
  (set! (zfield-Code-is-modified?-flag zfield) "false")
  (tm-zotero-write tid (safe-scm->json-string '()))))

;;}}}
;;{{{ Field_getText
;;;
;;; Gets the (visible) text of a field.
;;;
;;; ["Field_getText", [documentID, fieldID]] -> str_text
;;;
(define (tm-zotero-Field_getText tid documentID zfieldID)
  (tm-zotero-set-message
   (string-append "Processing command: Field_getText " zfieldID "..."))
  ;; (tm-zotero-format-debug "tm-zotero-Field_getText:called...")
  (tm-zotero-write tid (safe-scm->json-string
                        (string-convert (zfield-Text
                                         (get-document-zfield-by-zfieldID documentID zfieldID))
                                        "Cork"
                                        "UTF-8"))))

;;}}}
;;{{{ Field_setCode
;;;
;;; Sets the (hidden, persistent) code of a field.
;;;
;;; ["Field_setCode", [documentID, fieldID, str_code]] -> null
;;;
(define (tm-zotero-Field_setCode tid documentID zfieldID str_code)
  (tm-zotero-set-message
   (string-append "Processing command: Field_setCode " zfieldID "..."))
  ;; (tm-zotero-format-debug "tm-zotero-Field_setCode:called...")
  (set! (zfield-Code-code (get-document-zfield-by-zfieldID documentID zfieldID))
        str_code)
  (let* ((zfd (get-document-<zfield-data>-by-zfieldID documentID zfieldID))
         (scm (zfield-Code-code->scm str_code)))
    (when (and zfd scm)
      (set! (%zfd-Code-code-ht zfd) scm))
    (tm-zotero-format-debug "_GREEN_tm-zotero-Field_setCode_RESET_:scm:\n~s\n"
                            (catch #t
                              (lambda ()
                                (safe-scm->json-string
                                 (zfd-Code-code-ht
                                  (get-document-<zfield-data>-by-zfieldID documentID zfieldID))
                                 #:pretty #t))
                              (lambda args
                                "Error?"))))
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_getCode
;;;
;;; Gets the code of a field.
;;;
;;; ["Field_getCode", [documentID, fieldID]] -> str_code
;;;
(define (tm-zotero-Field_getCode tid documentID zfieldID)
  (tm-zotero-set-message
   (string-append "Processing command: Field_getCode " zfieldID "..."))
  ;; (tm-zotero-format-debug "tm-zotero-Field_getCode:called...")
  (tm-zotero-format-debug "_GREEN_tm-zotero-Field_getCode_RESET_:scm:\n~s\n"
                          (catch #t
                            (lambda ()
                              (safe-scm->json-string
                               (zfd-Code-code-ht
                                (get-document-<zfield-data>-by-zfieldID documentID zfieldID))
                               #:pretty #t))
                            (lambda args
                              "Error?")))
  (tm-zotero-write tid
                   (safe-scm->json-string
                    (zfield-Code-code
                     (get-document-zfield-by-zfieldID documentID zfieldID)))))

;;}}}
;;{{{ Field_convert
;;;
;;; Converts a field from one type to another.
;;;
;;; ["Field_convert", [documentID, fieldID, str_fieldType, int_noteType]] ->
;;; null
;;;
(define (tm-zotero-Field_convert tid documentID
                                 zfieldID str_fieldType int_noteType)
  (tm-zotero-set-message
   (string-append "Processing command: Field_convert " zfieldID "..."))
  (tm-zotero-format-debug "STUB:zotero-Field_convert: ~s ~s ~s ~s"
                   documentID zfieldID
                   str_fieldType int_noteType)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;}}}

;;;;;;
;;; Local Variables:
;;; fill-column: 79
;;; truncate-lines: t
;;; folded-file: t
;;; End:
;;;;;
