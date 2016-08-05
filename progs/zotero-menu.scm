;;; coding: utf-8


(texmacs-module (zotero-menu)
  (:use (zotero)))

(menu-bind zotero-menu
  (=> "Zotero"
      ("Add Citation" (zotero-addCitation))
      ("Edit Citation" (zotero-editCitation))
      ---
      ("Add Bibliography" (zotero-addBibliography))
      ("Edit Bibliography" (zotero-editBibliography))
      ---
      ("Refresh" (zotero-refresh))
      
      ;; ("Remove Codes" (zotero-removeCodes))
      ---
      ("Set Document Prefs" (zotero-setDocPrefs))
      ---
      ("Debug: Protocol trace to stdout?" (toggle-preference
                                           "zt-debug-trace?"))))


;;; How do I add to or replace a menu? Such as: Insert -> Link -> Citation



;; The tm-zotero.ts will load the zotero.scm module.
(menu-bind texmacs-extra-menu
  (:mode in-tm-zotero-style?)
  (link zotero-menu))
;;  (former))
