;;; coding: utf-8
;;; ✠
;;;
;; MODULE      : init-legal-brief.scm
;; DESCRIPTION : Initialize Legal Brief Style and Support Code
;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
(plugin-configure legal-brief
  (:require #t))

(when (supports-legal-brief?)
  ;; The legal-brief.ts will cause the insert-legal-templates.scm to be loaded,
  ;; so no lazy keyboard or other auto-load entries are required here.
  (extend-table style-menu-name
    ("legal-brief" "Utah Legal Brief Style"))
  (extend-table style-synopsis
    ("legal-brief" "Utah Legal Brief Style customized for Karl")))
