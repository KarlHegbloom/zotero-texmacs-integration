;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;
;; MODULE      : tm-zotero-kbd.scm
;; DESCRIPTION : Zotero Connector Plugin, Key bindings
;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;

(texmacs-module (tm-zotero-kbd)
  (:use (tm-zotero)))

(kbd-commands
  ("zc" "Insert Zotero Citation"
   (when (and (in-tm-zotero-style?)
              (not (focus-is-zfield?)))
     (tm-zotero-addCitation)))
  ("zcite" "Insert Zotero Citation"
   (when (and (in-tm-zotero-style?)
              (not (focus-is-zfield?)))
     (tm-zotero-addCitation)))
  ("zb" "Insert Zotero Bibliography"
   (when (and (in-tm-zotero-style?)
              (not (focus-is-zfield?)))
     (tm-zotero-addBibliography)))
  ("zbibliography" "Insert Zotero Bibliography"
   (when (and (in-tm-zotero-style?)
              (not (focus-is-zfield?)))
     (tm-zotero-addBibliography))))


(kbd-map
 (:mode in-tm-zotero-style?)
 ("M-C-r" (tm-zotero-refresh)))


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
  (:require (and (focus-is-zcite?)
                 (not (in-source?))))
  (tm-zotero-editCitation))

(tm-define (kbd-tab)
  (:require (and (focus-is-zbibliography?)
                 (not (in-source?))))
  (tm-zotero-editBibliography))
