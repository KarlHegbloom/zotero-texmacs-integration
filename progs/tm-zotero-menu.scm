;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;;; MODULE      : tm-zotero-menu.scm
;;; DESCRIPTION : Zotero Connector Plugin Menu definitions
;;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;;
;;; This software falls under the GNU general public license version 3 or
;;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;;
;;;;


(texmacs-module (tm-zotero-menu)
  (:use (tm-zotero)))


(menu-bind tm-zotero-menu
  (=> "Zotero"
      ("Add Citation" (tm-zotero-addCitation))
      ("Edit Citation" (tm-zotero-editCitation))
      ;;("Add or Edit Citation" (tm-zotero-addEditCitation))
      ("Affirm Citation" (tm-zotero-affirmCitation))
      ---
      ("Add Bibliography" (tm-zotero-addBibliography))
      ("Edit Bibliography" (tm-zotero-editBibliography))
      ---
      ("Refresh" (tm-zotero-refresh))
      ;; ("Remove Codes" (tm-zotero-removeCodes))
      ---
      ("Set Document Prefs" (tm-zotero-setDocPrefs))
      ---
      ("Debug: Protocol trace to stdout?" (toggle-preference
                                           "zt-debug-trace?"))))



;;; How do I add to or replace a menu? Such as: Insert -> Link -> Citation


;; The tm-zotero.ts will load the zotero.scm module.
(tm-menu (texmacs-extra-menu)
  (:mode in-tm-zotero-style?)
  (former)
  (link tm-zotero-menu))

;; (menu-bind texmacs-extra-menu
;;   (:mode in-tm-zotero-style?)
;;   (link tm-zotero-menu))
;;  (former))
