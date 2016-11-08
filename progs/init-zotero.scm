;;; coding: utf-8
;;;
;;
;; MODULE      : init-zotero.scm
;; DESCRIPTION : Initialize Zotero Connector Plugin
;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;

(plugin-configure zotero
  (:require #t))

;; The tm-zotero.ts will load the zotero.scm module, so simply adding it to
;; your document as a style package will cause the Zotero menu to appear, etc.
;;
(when (supports-zotero?)
  ;; (import-from (zotero))
  (texmacs-modes
    (in-tm-zotero-style% (style-has? "tm-zotero-dtd"))
    (in-zcite% (tree-is? (focus-tree) 'zcite) in-text% in-tm-zotero-style%)
    (in-zbibliography% (tree-is? (focus-tree) 'zbibliography) in-text% in-tm-zotero-style%)
    (in-zfield% (or (in-zcite?) (in-zbibliography?)))
    (in-ztHref% (tree-is? (focus-tree) 'ztHref) in-text% in-tm-zotero-style%))
  (lazy-keyboard (zotero-kbd) in-tm-zotero-style?)
  (lazy-menu (zotero-menu) in-tm-zotero-style?)
  (extend-table style-menu-name
    ("tm-zotero" "Zotero Integration"))
  (extend-table style-synopsis
    ("tm-zotero" "TeXmacs Integration with Juris-M/Zotero Reference Manager")))
