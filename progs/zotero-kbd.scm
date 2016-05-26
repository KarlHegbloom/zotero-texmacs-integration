;; Todo

(texmacs-module (zotero-kbd)
  (:use (zotero)))

(kbd-commands
  ("zc" "Insert Zotero Citation" (when (in-tm-zotero-style?) (zotero-addCitation)))
  ("zcite" "Insert Zotero Citation" (when (in-tm-zotero-style?) (zotero-addCitation)))
  ("zb" "Insert Zotero Bibliography" (when (in-tm-zotero-style?) (zotero-addBibliography)))
  ("zbibliography" "Insert Zotero Bibliography" (when (in-tm-zotero-style?) (zotero-addBibliography))))
