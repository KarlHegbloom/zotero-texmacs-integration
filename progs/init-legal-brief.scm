;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;;; MODULE      : init-legal-brief.scm
;;; DESCRIPTION : Initialize Legal Brief Style and Support Code
;;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;;
;;; This software falls under the GNU general public license version 3 or
;;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;;;;;

(plugin-configure legal-brief
  (:require #t))


(when (supports-legal-brief?)
  (import-from (legal-brief))
  (extend-table style-menu-name
    ("legal-brief" "Legal Brief Style"))
  (extend-table style-synopsis
    ("legal-brief" "Legal Brief Style")))

;;; You can put your own customized copy of the legal-brief style, etc. ahead
;;; of the normal path this is installed in, to shadow this, or you can create
;;; a new style that loads this one and overrides things...
;;;;
;;; Todo: explain that better in documentation.


;;; This is magic that lets it know that when the main style is legal-brief,
;;; then the tm-zotero style is also active. It ought to be able to do that on
;;; it's own but does not for some reason; I have to declare it this way.
;;;
(tm-define (style-includes? p q)
  (:require (and (== p "legal-brief")
                 (== q "tm-zotero")))
  #t)
