;;; coding: utf-8
;;;
;;
;; MODULE      : zotero-kbd.scm
;; DESCRIPTION : Zotero Connector Plugin, Key bindings
;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;

(texmacs-module (zotero-kbd)
  (:use (zotero)))

(kbd-commands
  ("zc" "Insert Zotero Citation"
   (when (in-tm-zotero-style?) (zotero-addCitation)))
  ("zcite" "Insert Zotero Citation"
   (when (in-tm-zotero-style?) (zotero-addCitation)))
  ("zb" "Insert Zotero Bibliography"
   (when (in-tm-zotero-style?) (zotero-addBibliography)))
  ("zbibliography" "Insert Zotero Bibliography"
   (when (in-tm-zotero-style?) (zotero-addBibliography))))


;; (tm-define (kbd-tab)
;;   (:require (in-tm-zotero-style?))
;;   (zt-format-debug "Debug:kbd:kbd-tab: (inside-which '(zcite)) => ~s\n"
;;                    (inside-which '(zcite)))
;;   (zt-format-debug "Debug:kbd:kbd-tab: (get-focus-path) => ~s\n"
;;                    (get-focus-path))
;;   (zt-format-debug "Debug:kbd:kbd-tab: (focus-tree) => ~s\n" (focus-tree))
;;   (zt-format-debug "Debug:kbd:kbd-tab: (tree-label (focus-tree)) => ~s\n"
;;                    (tree-label (focus-tree)))
;;   (zt-format-debug "Debug:kbd:kbd-tab: (tree-func? (focus-tree) 'zcite) => ~s\n"
;;                    (tree-func? (focus-tree) 'zcite)))
;;
;; Cursor is at right end of zcite, blue highlight appears around it.
;; Debug => Status => Path menu prints [ 1, 1, 2, 1 ].
;; Debug:kbd:kbd-tab: (inside-which '(zcite)) => #f
;; Debug:kbd:kbd-tab: (get-focus-path) => (1 1 2)
;; Debug:kbd:kbd-tab: (focus-tree) => <tree <zcite|+JGeR0gQNwL2AKT|ITEM CSL_CITATION {"citationID"...
;; Debug:kbd:kbd-tab: (tree-label (focus-tree)) => zcite
;; Debug:kbd:kbd-tab: (tree-func? (focus-tree) 'zcite) => #t

(tm-define (kbd-tab)
  (:require (and (in-zcite?)
                 (not (in-source?))))
  (zotero-editCitation))

(tm-define (kbd-tab)
  (:require (and (in-zbibliography?)
                 (not (in-source?))))
  (zotero-editBibliography))
