(texmacs-module (zotero-menu)
  (:use (zotero)))

(menu-bind zotero-menu
  ("Add Citation" (zotero-addCitation))
  ("Edit Citation" (zotero-editCitation))
  ---
  ("Add Bibliography" (zotero-addBibliography))
  ("Edit Bibliography" (zotero-editBibliography))
  ---
  ("Refresh" (zotero-refresh))

  ;; ("Remove Codes" (zotero-removeCodes))
  ---
  ("Set Document Prefs" (zotero-setDocPrefs)))


;; The tm-zotero.ts will load the zotero.scm module.
(menu-bind texmacs-extra-menu
  (:require (in-tm-zotero-style?))
  (=> "Zotero" (link zotero-menu)))
