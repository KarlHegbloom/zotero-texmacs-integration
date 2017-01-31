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
        (compat guile-2)
        (term ansi-color)
        ))

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


(define timestamp-format-string-colored
  (string-concatenate
   (list
    (colorize-string "~9,,,'0@s" 'GREEN) ;; seconds +
    ":"
    (colorize-string "~9,,,'0@s" 'GREEN) ;; nanoseconds
    ":("
    (colorize-string "~9,,,'0@s" 'CYAN)  ;; seconds since last timestamp +
    ":"
    (colorize-string "~9,,,'0@s" 'CYAN)  ;; nanoseconds since last timestamp
    "):")))

(define timestamp-format-string-plain
  "~9,,,'0@s:~9,,,'0@s:(~9,,,'0@s:~9,,,'0@s):")

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


(define (tm-zotero-format-error . args)
  (tm-errput
   (apply format (cons #f
                       (cons
                        (string-concatenate
                         (list
                          (timestamp (current-time))
                          (car args)))
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
                          (string-concatenate
                           (list
                            (timestamp (current-time))
                            (car args)))
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
    ;; (tm-zotero-format-debug "zfield-NoteIndex-t: zfield => ~s, zfieldID => ~s\n"
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
     (when (or (== what "all")
               (== what "bibliography"))
       (tm-zotero-refresh))
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
  ;;(tm-zotero-format-debug "buffer-get-part-mode called, mode => ~s\n" mode)
  (clear-<document-data>! (get-documentID))
  (former mode))



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
  ;; (tm-zotero-format-debug "notify-activated called.\n")
  (tm-zotero-set-message "zcite reactivated! Checking for modification...")
  (let* ((origText (zfield-Code-origText zfield))
         (newText  (zfield-Text zfield))
         (is-modified? (if (string=? newText origText) "false" "true")))
    ;; (tm-zotero-format-debug "notify-activated: origText => ~s\n" origText)
    ;; (tm-zotero-format-debug "notify-activated: newText => ~s\n" newText)
    (set! (zfield-Code-is-modified?-flag zfield) is-modified?)
    (tm-zotero-set-message
     (string-append "zcite reactivated! Checking for modification... is-modified? => "
                    is-modified? ". Done.")))
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
  (let ((zhd-ht (get-document-ztbibItemRefs-ht documentID))
        (zfieldID (zfield-zfieldID zfield))
        (ztHref*-ls (tm-search
                     zfield
                     (cut tm-func? <> 'ztHrefFromCiteToBib))))
    (map (lambda (ztHref*)
           (tm-zotero-format-debug "unintern-ztHrefFromCiteToBib-for-cut: ztHref* => ~s\n" ztHref*)
           (let* ((sysID (ztHref*-sysID ztHref*))
                  (ref-label (ztHrefFromCiteToBib-reflabel zfieldID sysID))
                  (zhd (hash-ref zhd-ht ref-label)))
             (tm-zotero-format-debug "unintern-ztHrefFromCiteToBib-for-cut: removing zhd with ref-label: ~s\n" ref-label)
             (tm-zotero-format-debug "unintern-ztHrefFromCiteToBib-for-cut: ls before: ~s\n"
                                     (map (lambda (z)
                                            (ztHrefFromCiteToBib-reflabel (the-zfieldID-of z) (the-sysID-of z)))
                                          (hash-ref zhd-ht sysID)))
             (hash-set! zhd-ht sysID
                        (list-filter
                         (hash-ref zhd-ht sysID)
                         (lambda (z)
                           (not (eq? zhd z)))))
             (tm-zotero-format-debug "unintern-ztHrefFromCiteToBib-for-cut: ls after: ~s\n"
                                     (map (lambda (z)
                                            (ztHrefFromCiteToBib-reflabel (the-zfieldID-of z) (the-sysID-of z)))
                                          (hash-ref zhd-ht sysID)))
             (and zhd (clear-tree-pointer zhd))
             (hash-remove! zhd-ht ref-label)))
         ztHref*-ls)))

(tm-define (clipboard-cut which)
  (:require (and (in-tm-zotero-style?)
                 (in-text?)
                 (has-zfields? (selection-tree))))
  (set! inside-tm-zotero-clipboard-cut #t)
  (let* ((documentID (get-documentID))
         (selection-t (selection-tree))
         (zfields (tm-search selection-t is-zfield?))
         (zfd-ht (get-document-zfield-zfd-ht documentID))
         (zfd-ls (get-document-zfield-zfd-ls documentID))
         (zb-zfd-ls (get-document-zbibliography-zfd-ls documentID))
         (new-zfield-zfd (get-document-new-zfield-zfd documentID)))
    (map (lambda (zfield)
           (let* ((zfieldID (zfield-zfieldID zfield))
                  (zfd (hash-ref zfd-ht zfieldID #f)))
             (if (not (eq? zfd new-zfield-zfd))
                 (begin
                   (hash-remove! zfd-ht zfieldID)
                   (set-document-zfield-zfd-ls! documentID
                    (list-filter zfd-ls
                                 (lambda (elt)
                                   (not (eq? elt zfd)))))
                   (when (is-zbibliography? zfield)
                     (set-document-zbiblioraphy-zfd-ls!
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
                 (let ((tp (tree-pointer new-zfield-zfd))) ; is the new one???
                   ;;
                   ;; How? This can happen only when the protocol between
                   ;; Zotero and TeXmacs has failed for some reason, usually
                   ;; due to a bug in this program causing it to be
                   ;; interrupted, leaving the new-zfield in the document.
                   ;;
                   (tm-zotero-format-error
                    "clipboard-cut: Cutting new zfield! Fixme: Probably protocol breakdown; Restart Firefox and TeXmacs.")
                   (tree-set! zfield
                              (stree->tree
                               '(strong "{?? New Citation ??}")))
                   (when tp
                     (when (observer? tp)
                       (tree-pointer-detach tp))
                     (set-document-new-zfield-zfd! #f))))))
         zfields)
      (clipboard-set which selection-t)
      (tree-set! selection-t ""))
  (set! inside-tm-zotero-clipboard-cut #f))


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
  (let ((clipboard-t (clipboard-get which)))
    (insert (tree-ref clipboard-t 1) 1)
    (let ((zfields (tm-search clipboard-t is-zfield?)))
      (map (lambda (zfield)
             (set! (zfield-zfieldID zfield) (get-new-zfieldID)))
           zfields))))

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
        (tm-zotero-format-error "ERR: insert-new-zfield ~s : focus-tree is a ~s\n"
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
       (tree-set! zfieldID-t t)))))


(define zfield-zfieldID
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (let* ((zfieldID-t (zfield-zfieldID-t zfield))
            (zfieldID (tree->stree zfieldID-t)))
       ;; (tm-zotero-format-debug "zfield-zfieldID: zfield => ~s\n" (tree->stree zfield))
       ;; (tm-zotero-format-debug "zfield-zfieldID: zfieldID-t => ~s\n" (tree->stree zfieldID-t))
       ;; (tm-zotero-format-debug "zfield-zfieldID: zfieldID => ~s\n" zfieldID)
       zfieldID))
   ;; set!
   (lambda (zfield str)
     (let ((zfieldID-t (zfield-zfieldID-t zfield)))
       (tree-set! zfieldID-t
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
(define zfield-Code-code-t
  (letrec ((ref-impl
            (lambda (zfield)
              (let ((code (tree-ref zfield 1)))
                (cond
                  ((tm-func? code 'tuple) ; >= v.3
                   ;; <tuple|3|<raw-data|THIS>|"false"|<raw-data|"origText">>
                   (tree-ref code 1 0))
                  ((tm-func? code 'raw-data) ; v.2
                   (tree-set! code
                              (stree->tree
                               `(tuple "3" ; update to v.3
                                       ,(tree->stree code)
                                       "false"
                                       (raw-data ""))))
                   (ref-impl zfield) ; tail-call
                   )
                  ((tm-atomic? code)    ; v.1
                   (tree-set! code
                              (stree->tree
                               `(tuple "3" ; to v.3
                                       (raw-data ,(tree->stree code))
                                       "false"
                                       (raw-data ""))))
                   (ref-impl zfield) ; tail-call
                   )
                  (else ; ? I don't think this can really happen.
                    (tree-set! code
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
                (tree-set! code-t t))))
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
       (tree-set! code-t
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
       (tree-set! is-modified?-flag-t t)))))


(define zfield-Code-is-modified?-flag
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree->stree (zfield-Code-is-modified?-flag-t zfield)))
   ;; set!
   (lambda (zfield str-bool)
     (let ((is-modified?-flag-t (zfield-Code-is-modified?-flag-t zfield)))
       (tree-set! is-modified?-flag-t
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
       (tree-set! origText-t t)))))

(define zfield-Code-origText
  (make-procedure-with-setter
   ;; ref
   (lambda (zfield)
     (tree->stree (zfield-Code-origText-t zfield)))
   ;; set!
   (lambda (zfield t)
     (let ((origText-t (zfield-Code-origText-t zfield)))
       (tree-set! origText-t
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
       (tree-set! text-t t)))))

;;;;;;
;;;
;;; This is used to convert the zfield-Text texmacs tree into a string so that
;;; Zotero's mechanism for determining if the user has editted the zfield-Text
;;; by hand can have something it can work with. It is used to store the
;;; original text in the <zfield-data> for the zfield, and to create the
;;; comparison string from the current value of the zfield-Text.
;;;
(define (zfield-Text zfield)
  (format #f "~s" (tree->stree (zfield-Text-t zfield))))
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
       (tree-set! hashLabel-t t)))))

(define ztHref*-hashLabel
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree->stree (ztHref*-hashLabel-t ztHref*)))
   ;; set!
   (lambda (ztHref* str)
     (let ((hashLabel-t (ztHref*-hashLabel-t ztHref*)))
       (tree-set! hashLabel-t
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
       (tree-set! hashLabel-t t)))))

(define ztHref*-url
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree->stree (ztHref*-url-t ztHref*)))
   ;; set!
   (lambda (ztHref* str)
     (let ((url-t (ztHref*-url-t ztHref*)))
       (tree-set! url-t
                  (stree->tree str))))))


(define ztHref*-display-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref*)
     (tree-ref ztHref* 2))
   ;; set!
   (lambda (ztHref* t)
     (let ((display-t (tree-ref ztHref* 2)))
       (tree-set! display-t t)))))



(define ztHref-url-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref)
     (tree-ref ztHref 0))
   ;; set!
   (lambda (ztHref t)
     (let ((url-t (tree-ref ztHref 0)))
       (tree-set! url-t t)))))

(define ztHref-url
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref)
     (tree->stree (ztHref-url-t ztHref)))
   ;; set!
   (lambda (ztHref str)
     (let ((url-t (ztHref-url-t ztHref)))
       (tree-set! url-t
                  (stree->tree str))))))


(define ztHref-display-t
  (make-procedure-with-setter
   ;; get
   (lambda (ztHref)
     (tree-ref ztHref 1))
   ;; set!
   (lambda (ztHref t)
     (let ((display-t (tree-ref ztHref 1)))
       (tree-set! display-t t)))))


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
  (tree-pointer #:init-value #f)
  (zfd-tree #:allocation #:virtual
            #:slot-ref (lambda (zfd)
                         (let ((tp (slot-ref zfd 'tree-pointer)))
                           (if (and tp
                                    (observer? tp))
                               (tree-pointer->tree tp)
                               #f)))
            #:slot-set! (lambda (zfd t)
                          (and-with tp (slot-ref zfd 'tree-pointer)
                            (when (and tp
                                       (observer? tp))
                              (tree-pointer-detach tp)))
                          (slot-set! zfd 'tree-pointer (tree->tree-pointer t))
                          (slot-set! zfd 'the-zfieldID-of (zfield-zfieldID t))
                          ;; (slot-set! zfd 'zfd-zfield-Code (zfield-zfield-Code-code t))
                          ))
  ;; String, original unmodified text for comparison
  (zfd-orig-text #:init-value "")
  ;;;
  ;;
  ;; TODO Do these need time stamps for change-dependency trigger/track?
  ;;
  ;;;
  )

(define-method (clear-tree-pointer (zfd <zfield-data>))
  (let ((tp (tree-pointer zfd)))
    (when tp
      (when (observer? tp)
        (tree-pointer-detach tp))
      (set! (tree-pointer zfd) #f))))

;;}}}
;;{{{ define-class for <ztHrefFromCiteToBib-data>

;;; We need the tree-pointer for maintaining lists of these in in-document
;;; order using merge.
;;;
(define-class-with-accessors-keywords <ztHrefFromCiteToBib-data> ()
  (the-zfieldID-of #:init-value "")
  (the-sysID-of #:init-value "")
  ;; TeXmacs tree-pointer
  (tree-pointer #:init-value #f)
  (zhd-tree #:allocation #:virtual
            #:slot-ref (lambda (zhd)
                         (let ((tp (slot-ref zhd 'tree-pointer)))
                           (if tp
                               (tree-pointer->tree tp)
                               #f)))
            #:slot-set! (lambda (zhd t)
                          (slot-set! zhd 'tree-pointer (tree->tree-pointer t)))))


(define-method (the-ref-label-of (zhd <ztHrefFromCiteToBib-data>))
  (ztHrefFromCiteToBib-reflabel (the-zfieldID-of zhd) (the-sysID-of zhd)))


(define-method (clear-tree-pointer (zhd <ztHrefFromCiteToBib-data>))
  (let ((tp (tree-pointer zhd)))
    (when tp
      (when (observer? tp)
        (tree-pointer-detach tp))
      (set! (tree-pointer zhd) #f))))

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
                                  (hash-for-each (lambda (key elt)
                                                   key)
                                                 zhd-ht))))))
    (when new-zfield-zfd
      (clear-tree-pointer new-zfield-zfd)
      (set! (document-new-zfield-zfd dd) #f))
    (when zfd-ls
      (map clear-tree-pointer zfd-ls)) ; TODO Guardians and after-gc-hook
    (when zhd-ls
      (map clear-tree-pointer zhd-ls)) ; TODO Guardians and after-gc-hook
    (set-<document-data>! documentID
                          (make-instance <document-data>))))


;;}}}

;;{{{ document-active-mark-nr

(define-method (get-document-active-mark-nr (documentID <string>))
  (document-active-mark-nr (get-<document-data> documentID)))

(define (set-document-active-mark-nr! documentID val)
  (set! (document-active-mark-nr (get-<document-data> documentID)) val))


;;}}}

;;{{{ get-new-zfieldID, get-document-new-zfield-zfd, etc.

(define get-new-zfieldID create-unique-id)

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
    ;; (tm-zotero-format-debug "zfield-is-document-new-zfield?: new-zfieldID => ~s, zfieldID => ~s\n" new-zfieldID zfieldID)
    (if zfd
        (string=? zfieldID new-zfieldID)
        #f)))

;;;
;;; Called from the   document-mark-cancel-error-cleanup-hook
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
  (tree-pointer
   (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))

(define (set-document-zfield-tree-pointer-by-zfieldID! documentID zfieldID tp)
  (set! (tree-pointer (get-document-<zfield-data>-by-zfieldID documentID zfieldID))
        tp))


(define (get-document-zfield-by-zfieldID documentID zfieldID)
  (tree-pointer->tree
   (get-document-zfield-tree-pointer-by-zfieldID documentID zfieldID)))

(define (go-to-document-zfield-by-zfieldID documentID zfieldID)
  (tree-go-to (get-document-zfield-by-zfieldID documentID zfieldID) 1))


(define (get-document-zfield-orig-text-by-zfieldID documentID zfieldID)
  (zfd-orig-text (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))

(define (set-document-zfield-orig-text-by-zfieldID! documentID zfieldID str)
  (set! (zfd-orig-text (get-document-<zfield-data>-by-zfieldID documentID zfieldID))
        str))

;;}}}

;;{{{ document-zfield-text-user-modified?

(define (document-zfield-text-user-modified? documentID zfieldID)
  (let* ((zfield        (get-document-zfield-by-zfieldID documentID zfieldID))
         (zfield-Text-t (and zfield (zfield-Text-t zfield)))
         ;; from the document tree itself
         (text          (or (and zfield-Text-t
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
  (unless inside-tm-zotero-clipboard-cut
    (tm-zotero-format-debug "tm-zotero-ext:ensure-zfield-interned! called, zfieldID-t => ~s\n" zfieldID-t)
    (let* ((documentID (get-documentID))
           (zfieldID (tree->stree zfieldID-t))
           ;;         fail if this is the new-zfield not yet finalized by
           ;;         Document_insertField.
           (is-new? (zfield-is-document-new-zfield? documentID zfieldID)))
      ;; (tm-zotero-format-debug "tm-zotero-ext:ensure-zfield-interned!: zfieldID => ~s\n" zfieldID)
      ;; (tm-zotero-format-debug "tm-zotero-ext:ensure-zfield-interned!: is-new? => ~s\n" is-new?)
      (if is-new?
          (begin
            ;; Then we're done here, that quick, since the new zfield is already
            ;; partly interned, and isn't finalized until
            ;; tm-zotero-Document_insertField.
            ;;(tm-zotero-format-debug "tm-zotero-ext:ensure-zfield-interned! returning, is-new? => #t\n")
            "")
          (let ((zfd (get-document-<zfield-data>-by-zfieldID documentID zfieldID)))
            (if zfd
                (begin
                  ;; then we're done, it's already interned.
                  ;;(tm-zotero-format-debug "tm-zotero-ext:ensure-zfield-interned! returning, zfd was already interned, zfd => ~s\n" zfd)
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
                (and-with zfield (tree-search-upwards zfieldID-t zfield-tags)
                  (if (inside-shown-part? zfield)
                      (begin
                        (set! zfd (make-instance <zfield-data> #:zfd-tree zfield))
                        (hash-set! (get-document-zfield-zfd-ht documentID) zfieldID zfd)
                        (document-merge!-<zfield-data> zfd)
                        (when (is-zbibliography? zfield)
                          (document-merge!-zbibliography-zfd zfd))
                        ;; (tm-zotero-format-debug
                        ;; "tm-zotero-ext:ensure-zfield-interned! returning. Interned new zfield, zfd => ~s\n" zfd)
                        )
                      (begin
                        ;; (tm-zotero-format-debug "tm-zotero-ext:ensure-zfield-interned! returning. Not inside show-part. Nothing interned.\n")
                        )
                      )
                  ""))))))
  "")


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
  ;; When the tag is not activated, zfieldID-t will be uninitialized.
  (when (and (not inside-tm-zotero-clipboard-cut)
             (eq? (class-of zfieldID-t) <tree>))
    (let* ((zfieldID (tree->stree zfieldID-t))
           ;;(dummy (tm-zotero-format-debug "tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!: zfieldID => ~s\n" zfieldID))
           (ztHref* (tree-up hashLabel-t))
           ;;(dummy (tm-zotero-format-debug "tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!: ztHref* => ~s\n" ztHref*))
           (sysID (string-tail (tree->stree hashLabel-t) ztbibItemRef-hashlabel-string-prefix-length))
           ;;(dummy (tm-zotero-format-debug "tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!: sysID => ~s\n" sysID))
           (documentID (get-documentID))
           (zhd (make-instance <ztHrefFromCiteToBib-data>
                               #:the-zfieldID-of zfieldID
                               #:the-sysID-of sysID
                               #:zhd-tree ztHref*))
           (zhd-ht (get-document-ztbibItemRefs-ht documentID))
           (ref-label (ztHrefFromCiteToBib-reflabel zfieldID sysID)))
      ;; Inside of legal case parallel citations the secondary citations have the
      ;; same sysID as the first one does. Only intern the first one.
      (unless (hash-ref zhd-ht ref-label #f)
        ;; handles for later clearing of the tree pointers in these
        (hash-set! zhd-ht ref-label zhd)
        (document-merge!-ztbibItemRefs-ls zhd))))
  "")

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
    (tm-zotero-format-debug "tm-zotero-ext:get-ztbibItemRefsList: ref-labels-ls => ~s\n" ref-labels-ls)
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
(tm-define (tm-zotero-ext:ztShowID node cslid body)
  (:secure)
  ;; (tm-zotero-format-debug "zt-ext-ztShowID: ~s ~s ~s\n" node clsid body)
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
(tm-define (tm-zotero-ext:zbibCitationItemID sysID)
  (:secure)
  ;; (tm-zotero-format-debug "STUB:zt-ext-zbibCitationItemID: ~s\n\n" sysID)
  "")

(tm-define (tm-zotero-ext:bibitem key)
  (:secure)
  ;; (tm-zotero-format-debug "STUB:zt-ext-bibitem: ~s\n" key)
  "")

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
        (tm-zotero-format-error "ERR: Exception caught in get-tm-zotero-socket-port: ~s\n" args)
        (close-port tm-zotero-socket-port)
        (set! tm-zotero-socket-port #f)
      (set-document-active-mark-nr! documentID #f)
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
  (tm-zotero-format-debug "tm-zotero-write:tid:~s:cmd:~s\n" tid cmd)
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
          (tm-zotero-format-error "ERR: System error in tm-zotero-write: ~s ~s\n" tid cmd)
          (tm-zotero-format-error "ERR: Exception caught: ~s\n" args)
          (tm-zotero-format-error "ERR: Closing Zotero port!\n")
          (close-tm-zotero-socket-port!)
          (set-document-active-mark-nr! documentID #f)
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
        (tm-zotero-format-error "ERR: Exception caught in tm-zotero-read: ~s\n" args)
        (list (or tid 0) (or len 666) (format #f "ERR: System error in tm-zotero-read: ~s" args)))))) ;; return to tm-zotero-listen


(define (safe-json-string->scm str)
  (catch 'json-invalid
    (lambda ()
      (json-string->scm str))
    (lambda args
      (tm-zotero-format-error "ERR: Exception caught from json-string->scm: ~s\n" args)
      ;; return to tm-zotero-listen
      (list (format #f "ERR: Invalid JSON: ~s\n" str) '()))))


(define (safe-scm->json-string scm)
  (catch #t
    (lambda ()
      (scm->json-string scm))
    (lambda args
      (tm-zotero-format-error "ERR: Exception caught from scm->json-string: ~s\n" args)
      (tm-zotero-format-error "ERR: scm: ~s\n" scm)
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
  ;; (tm-zotero-format-debug "document-mark-cancel-and-error-cleanup called, documentID => ~s\n" documentID)
  (let ((mark-nr (get-document-active-mark-nr documentID)))
    (when mark-nr
      (mark-cancel mark-nr) ; causes undo to happen
      (set-document-active-mark-nr! documentID #f)
      (run-hook document-mark-cancel-error-cleanup-hook documentID))))


(define document-mark-end-cleanup-hook (make-hook 1))

(define (document-mark-end-and-cleanup documentID)
  ;; (tm-zotero-format-debug "document-mark-end-and-cleanup called, documentID => ~s\n" documentID)
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
  (tm-zotero-format-debug "tm-zotero-listen called by: cmd => ~s\n" cmd)
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
            (tm-zotero-format-debug "tm-zotero-listen delayed read: tid => ~s, len => ~s, cmdstr => ~s\n" tid len cmdstr)
            (if (> len 0)
                ;; then
                (with (editCommand args) (safe-json-string->scm cmdstr)
                  ;; (tm-zotero-format-debug "~s\n" (list editCommand (cons tid args)))
                  (cond
                    ((and (>= (string-length editCommand) 4)
                          (string=? (string-take editCommand 4) "ERR:"))
                     ;; editCommand is really an error string this time.
                     ;; (tm-zotero-format-debug "tm-zotero-listen:~s\n" editCommand)
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
                     ;; (tm-zotero-format-debug "tm-zotero-Document_complete
                     ;; called.\n")
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
  ;;(tm-zotero-set-message "Processing command: Application_getActiveDocument...")
  ;;(tm-zotero-format-debug "zotero-Application_getActiveDocument called.\n")
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
;;;
;;; Shows an alert.
;;;
;;; ["Document_displayAlert", [documentID, str_dialogText, int_icon, int_buttons]] -> int_button_pressed
;;;
(define (tm-zotero-Document_displayAlert tid documentID str_dialogText int_icon
                                         int_buttons)
  ;;(tm-zotero-set-message "Processing command: Document_displayAlert...")
  ;;(tm-zotero-format-debug "tm-zotero-Document_displayAlert called.\n")
  (dialogue-window (zotero-display-alert documentID str_dialogText int_icon int_buttons)
                   (lambda (val)
                     (tm-zotero-write tid (safe-scm->json-string val)))
                   "Zotero Alert!"))

;;}}}
;;{{{ Document_activate
;;;
;;; Brings the document to the foreground.
;;;  (For OpenOffice, this is a no-op on non-Mac systems.)
;;;
;;; ["Document_activate", [documentID]] -> null
;;;
(define (tm-zotero-Document_activate tid documentID)
  ;;(tm-zotero-set-message "Processing command: Document_activate...")
  ;;(tm-zotero-format-debug "tm-zotero-Document_activate called.\n")
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Document_canInsertField
;;;
;;; Indicates whether a field can be inserted at the current cursor position.
;;;
;;; ["Document_canInsertField", [documentID, str_fieldType]] -> boolean
;;;
(define (tm-zotero-Document_canInsertField tid documentID str_fieldType)
  ;;(tm-zotero-set-message "Processing command: Document_canInsertField...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_canInsertField called.\n")
  (let ((ret (not
              (not
               (and (in-text?)
                    (not (in-math?))
                    (if (focus-is-zfield?)
                        (let ((zfield (focus-tree)))
                          ;; (tm-zotero-format-debug
                          ;;  "tm-zotero-Document_canInsertField:focus-is-zfield? => #t, document-new-zfieldID => ~s, (focus-tree) => ~s\n"
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
  ;;(tm-zotero-set-message "Processing command: Document_getDocumentData...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_getDocumentData called.\n")
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
  ;;(tm-zotero-set-message "Processing command: Document_setDocumentData...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_setDocumentData called.\n")
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
  ;;(tm-zotero-set-message "Processing command: Document_cursorInField...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_cursorInField called.\n")
  (let ((ret (if (focus-is-zfield?)
                 (begin
                   ;; (tm-zotero-format-debug "tm-zotero-Document_cursorInField: focus-is-zfield? => #t\n")
                   (let* ((zfield (focus-tree))
                          (zfieldID (zfield-zfieldID zfield)))
                     (if (not (zfield-is-document-new-zfield? documentID zfieldID))
                         (begin
                           (let ((zfieldCode (zfield-Code-code zfield))
                                 (noteIndex (zfield-NoteIndex zfield)))
                             ;; (tm-zotero-format-debug
                             ;;  "tm-zotero-Document_cursorInField:id:~s:code:~s:ni:~s\n"
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
  ;;(tm-zotero-set-message "Processing command: Document_insertField...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_insertField called.\n")
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
  ;;(tm-zotero-set-message "Processing command: Document_getFields...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_getFields called.\n")
  (let ((ret
         (let loop ((zfield-zfd-ls (get-document-zfield-zfd-ls documentID)) ; list of <zfield-data>.
                    (ids '()) (codes '()) (indx '()))
           ;; (tm-zotero-format-debug "tm-zotero-Document_getFields:zfield-zfd-ls => ~s\n" zfield-zfd-ls)
           ;; (tm-zotero-format-debug "tm-zotero-Document_getFields: ~s\n" (map (lambda (zfd)
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
                      (zfield (zfd-tree zfd))
                      (code (zfield-Code-code zfield))
                      (zfieldID (zfield-zfieldID zfield))
                      (noteIndex (zfield-NoteIndex zfieldID)))
                 (loop (cdr zfield-zfd-ls)
                       (cons zfieldID ids)
                       (cons code codes)
                       (cons noteIndex indx))))))))
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
  ;;(tm-zotero-set-message "Processing command: Document_convert...")
  ;; (tm-zotero-format-debug "tm-zotero-Document_convert called.\n")
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
  ;;(tm-zotero-set-message "Processing command: Document_setBibliographyStyle...")
  ;;(tm-zotero-format-debug "tm-zotero-Document_setBibliographyStyle called.\n")
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
  ;;(tm-zotero-set-message "Processing command: Document_cleanup...")
  (tm-zotero-format-debug "STUB:tm-zotero-Document_cleanup: ~s\n" documentID)
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
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_delete " zfieldID "..."))
  ;;(tm-zotero-format-debug "tm-zotero-Field_delete called.\n")
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
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_select " zfieldID "..."))
  ;;(tm-zotero-format-debug "tm-zotero-Field_select called.\n")
  (go-to-document-zfield-by-zfieldID documentID zfieldID)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_removeCode
;;;
;;; ["Field_removeCode", [documentID, fieldID]] -> null
;;;
(define (tm-zotero-Field_removeCode tid documentID zfieldID)
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_removeCode " zfieldID "..."))
  ;;(tm-zotero-format-debug "tm-zotero-Field_removeCode called.\n")
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
  ;;(tm-zotero-format-debug "move-link-to-own-line called, lnk => ~s\n" lnk)
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
    ;; (tm-zotero-format-debug "lnk before: ~s\n" lnk)
    ;; (tm-zotero-format-debug "pre-lnk-str: ~s\n" pre-lnk-str)
    ;; (tm-zotero-format-debug "post-lnk-str: ~s\n" post-lnk-str)
    (unless is-doi?
      ;; (tm-zotero-format-debug "is-doi? => #f\n")
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
                ;;(tm-zotero-format-debug "Keep link next to the prefix text.\n")
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
    ;; (tm-zotero-format-debug "move-link-to-own-line returning, lnk => ~s\n" lnk)
    )
  lnk)


;; (define (delete-one-space-to-left-of lnk)
;;   (tm-zotero-format-debug "delete-one-space-to-left-of called.\n")
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
;;   (tm-zotero-format-debug "fixup-embedded-slink-as-url called.\n")
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
         (("(<doi:)")
          pre "\\ztlt{}doi:" post)
         (("(}>)")
          pre "}\\ztgt{}" post)
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
  ;;(tm-zotero-format-debug "tm-zotero-regex-transform called, str_text => ~s\n" str_text)
  (let ((text str_text))
    (do ((rc tm-zotero-regex-replace-clauses (cdr rc)))
        ((null? rc)
         ;;(tm-zotero-format-debug "tm-zotero-regex-transform returning, text => ~s\n" text)
         text)
      ;; each is applied in turn, so later ones can modify results of earlier
      ;; ones if you like.
      ;;(tm-zotero-format-debug "tm-zotero-regex-transform:during:text: ~S\n" text)
      (set! text (apply regexp-substitute/global `(#f ,(caar rc) ,text ,@(cdar rc)))))))


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
  (tm-zotero-set-message-and-system-wait
   "Munging, transcoding, and parsing input..." please-wait)
  (noop)
  (tm-zotero-set-message-and-system-wait ; try to force refresh so it is readable
   "Munging, transcoding, and parsing input..." please-wait)
  (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs called, str_text => ~s, is-note? => ~s, is-bib? => ~s\n" str_text is-note? is-bib?)
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
    ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs after let*. !!!\n")
    (buffer-set-body b t) ;; This is magical.
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    ;;
    ;; Used from inside tm-zotero.ts
    ;;
    (let ((lt (select t '(:* (:or ztHref hlink href)))))
      ;; It turns out that tm-select will return these not in tree or document 
      ;; order.  For this function, that's alright.
      ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:t ztHref hlink href before: ~s\n" t)
      ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:select lt: ~s\n" lt)
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
      ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:t ztHrefFromBibToURL ztHrefFromCiteToBib before: ~s\n" t)
      ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:select lt: ~s\n" lt)
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
              ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:fixup-slink-as-url lnk:~s\n" lnk)
              (fixup-embedded-slink-as-url lnk))))))
    ;;
    ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:before tree-simplify\n")
    (tree-simplify t)
    ;; (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs:after tree-simplify\n")
    (buffer-pretend-autosaved b)
    (buffer-pretend-saved b)
    (buffer-close b)
    (recall-message)
    (tm-zotero-format-debug "tm-zotero-UTF-8-str_text->texmacs returning.\n")
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
  ;; (tm-zotero-format-debug "zfield-IsBib? called... zfield label:~s\n"
  ;;                  (tree-label zfield))
  (tree-is? zfield 'zbibliography))


;;;
;;; Input is a field tree, already found.
;;;
(define (zfield-IsNote? zfield)
  ;; (tm-zotero-format-debug "zfield-IsNote? called.\n")
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
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_setText " zfieldID "..."))
  (tm-zotero-format-debug "tm-zotero-Field_setText called.\n")
  (let* ((zfield   (get-document-zfield-by-zfieldID documentID zfieldID))
         (is-note? (and zfield (zfield-IsNote? zfield)))
         (is-bib?  (and zfield (zfield-IsBib? zfield)))
         (tmtext
          (tm-zotero-UTF-8-str_text->texmacs str_text is-note? is-bib?)))
    ;;(tm-zotero-format-debug "tm-zotero-Field_setText: about to set zfield-Text-t\n")
    ;; It was crashing here at the destructor for tree-pointer. I think it's
    ;; the tree-pointers on the ztHrefFromCiteToBib tags.
    ;;(set! (zfield-Text-t zfield) tmtext)
    (set! inside-tm-zotero-clipboard-cut #t)
    (tm-zotero-format-debug "tm-zotero-Field_setText: about to unintern-ztHrefFromCiteToBib-for-cut\n")
    (unintern-ztHrefFromCiteToBib-for-cut documentID zfield)
    (tm-zotero-format-debug "tm-zotero-Field_setText: about to set zfield-Text-t\n")
    (set! (zfield-Text-t zfield) tmtext)
    (set! inside-tm-zotero-clipboard-cut #f)
    (tm-zotero-format-debug "tm-zotero-Field_setText: about to set zfield-Code-origText\n")
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
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_getText " zfieldID "..."))
  ;; (tm-zotero-format-debug "tm-zotero-Field_getText called.\n")
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
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_setCode " zfieldID "..."))
  ;; (tm-zotero-format-debug "tm-zotero-Field_setCode called.\n")
  (set! (zfield-Code-code (get-document-zfield-by-zfieldID documentID zfieldID))
        str_code)
  (tm-zotero-write tid (safe-scm->json-string '())))

;;}}}
;;{{{ Field_getCode
;;;
;;; Gets the code of a field.
;;;
;;; ["Field_getCode", [documentID, fieldID]] -> str_code
;;;
(define (tm-zotero-Field_getCode tid documentID zfieldID)
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_getCode " zfieldID "..."))
  ;; (tm-zotero-format-debug "tm-zotero-Field_getCode called.\n")
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
  ;; (tm-zotero-set-message
  ;;  (string-append "Processing command: Field_convert " zfieldID "..."))
  (tm-zotero-format-debug "STUB:zotero-Field_convert: ~s ~s ~s ~s\n"
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
;;;;;;
