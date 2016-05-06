<TeXmacs|1.99.4>

<style|article>

<\body>
  <\session|scheme|default>
    <\unfolded-io|Scheme] >
      (%search-load-path "json.scm")
    <|unfolded-io>
      "/home/karlheg/.TeXmacs/plugins/zotero/progs/json.scm"
    </unfolded-io>

    <\input|Scheme] >
      (use-modules (ice-9 format))
    </input>

    <\input|Scheme] >
      (use-modules (json))
    </input>

    <\input|Scheme] >
      (load-from-path "zotero.scm")
    </input>

    <\input|Scheme] >
      (zotero-listener)
    </input>

    <\unfolded-io|Scheme] >
      (zotero-addCitation)
    <|unfolded-io>
      <errput|Unbound variable: zotero-addCitation>
    </unfolded-io>

    <\input|Scheme] >
      (begin (write-zotero 0 "\\"addCitation\\""))
    </input>

    <\unfolded-io|Scheme] >
      (read-zotero)
    <|unfolded-io>
      (1 37 "[\\"Application_getActiveDocument\\",[3]]")
    </unfolded-io>

    <\input|Scheme] >
      (zotero-Application-getActiveDocument 1 3)
    </input>

    <\unfolded-io|Scheme] >
      (url-\<gtr\>unix (current-buffer-url))
    <|unfolded-io>
      "/home/karlheg/src/Juris-M/zotero-texmacs-integration/scheme-session.tm"
    </unfolded-io>

    <\input|Scheme] >
      (define-public (zotero-getDocId) (url-\<gtr\>unix
      (current-buffer-url)))
    </input>

    <\unfolded-io|Scheme] >
      (read-zotero)
    <|unfolded-io>
      (2 103 "[\\"Document_getDocumentData\\",[\\"/home/karlheg/src/Juris-M/zotero-texmacs-integration/scheme-session.tm\\"]]")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (buffer-get)
    <|unfolded-io>
      <errput|Wrong number of arguments to #\<primitive-procedure
      buffer-get\>>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (go-to-buffer)
    <|unfolded-io>
      <errput|Wrong number of arguments to #\<procedure go-to-buffer
      (name)\>>
    </unfolded-io>

    <\input|Scheme] >
      (go-to-buffer "scheme-session.tm")
    </input>

    <\unfolded-io|Scheme] >
      (url-\<gtr\>string (current-buffer))
    <|unfolded-io>
      "/home/karlheg/src/Juris-M/zotero-texmacs-integration/scheme-session.tm"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (buffer-path)
    <|unfolded-io>
      (1)
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (scm-\<gtr\>json-string '())
    <|unfolded-io>
      "null"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm "[\\"Document_getDocumentData\\",[\\"/home/karlheg/src/Juris-M/zotero-texmacs-integration/scheme-session.tm"\\\\")
    <|unfolded-io>
      <errput|#\<unknown port\>:1:129: end of file in string constant>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm ""[\\"Document_getDocumentData\\",[\\"/home/karlheg/src/Juris-M/zotero-texmacs-integration/scheme-session.tm"\\]]")
    <|unfolded-io>
      <errput|#\<unknown port\>:1:130: end of file in string constant>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm "[\\"Document_getDocumentData\\",[\\"/home/karlheg/src/Juris-M/zotero-texmacs-integration/scheme-session.tm"\\]]")
    <|unfolded-io>
      <errput|#\<unknown port\>:1:129: end of file in string constant>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm "[\\"Document_getDocumentData\\",[1]]")
    <|unfolded-io>
      ("Document_getDocumentData" (1))
    </unfolded-io>

    <\input|Scheme] >
      (zotero-addCitation)
    </input>

    <\input|Scheme] >
      \;
    </input>

    <\input|Scheme] >
      \;
    </input>

    <\input|Scheme] >
      (display* "testing" " one" "\\n")
    </input>

    <\unfolded-io|Scheme] >
      (begin (zotero-write 0 "\\"addCitation\\"") (zotero-select-then-read))
    <|unfolded-io>
      (1 37 "[\\"Application_getActiveDocument\\",[3]]")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (zotero-Application_getActiveDocument 3)
    <|unfolded-io>
      (3 1)
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (begin (zotero-write 1 (scm-\<gtr\>json-string (list 3 1)))
      (zotero-select-then-read))
    <|unfolded-io>
      (2 32 "[\\"Document_getDocumentData\\",[1]]")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (zotero-Document_getDocumentData 1)
    <|unfolded-io>
      ""
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (begin (zotero-write 2 (scm-\<gtr\>json-string ""))
      (zotero-select-then-read))
    <|unfolded-io>
      (0 0 "")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (zotero-select-then-read)
    <|unfolded-io>
      (3 1259 "[\\"Document_setDocumentData\\",[1,\\"\<less\>data
      data-version=\\\\\\"3\\\\\\" zotero-version=\\\\\\"4.0.29.8m73\\\\\\"\<gtr\>\<less\>session
      id=\\\\\\"iOE03FdV\\\\\\"/\<gtr\>\<less\>style
      id=\\\\\\"http://juris-m.github.io/styles/jm-indigobook-in-text\\\\\\"
      locale=\\\\\\"en-US\\\\\\" hasBibliography=\\\\\\"1\\\\\\"
      bibliographyStyleHasBeenSet=\\\\\\"0\\\\\\"/\<gtr\>\<less\>prefs\<gtr\>\<less\>pref
      name=\\\\\\"citationTransliteration\\\\\\"
      value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationTranslation\\\\\\"
      value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationSort\\\\\\" value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsPersons\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsInstitutions\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsTitles\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsJournals\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsPublishers\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsPlaces\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationAffixes\\\\\\" value=\\\\\\"\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"projectName\\\\\\" value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"extractingLibraryID\\\\\\"
      value=\\\\\\"0\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"extractingLibraryName\\\\\\" value=\\\\\\"No group
      selected\\\\\\"/\<gtr\>\<less\>pref name=\\\\\\"fieldType\\\\\\"
      value=\\\\\\"ReferenceMark\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"storeReferences\\\\\\" value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"automaticJournalAbbreviations\\\\\\"
      value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"noteType\\\\\\" value=\\\\\\"0\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"suppressTrailingPunctuation\\\\\\"
      value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>/prefs\<gtr\>\<less\>/data\<gtr\>\\"]]")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm "[\\"Document_setDocumentData\\",[1,\\"\<less\>data
      data-version=\\\\\\"3\\\\\\" zotero-version=\\\\\\"4.0.29.8m73\\\\\\"\<gtr\>\<less\>session
      id=\\\\\\"iOE03FdV\\\\\\"/\<gtr\>\<less\>style
      id=\\\\\\"http://juris-m.github.io/styles/jm-indigobook-in-text\\\\\\"
      locale=\\\\\\"en-US\\\\\\" hasBibliography=\\\\\\"1\\\\\\"
      bibliographyStyleHasBeenSet=\\\\\\"0\\\\\\"/\<gtr\>\<less\>prefs\<gtr\>\<less\>pref
      name=\\\\\\"citationTransliteration\\\\\\"
      value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationTranslation\\\\\\"
      value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationSort\\\\\\" value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsPersons\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsInstitutions\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsTitles\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsJournals\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsPublishers\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationLangPrefsPlaces\\\\\\"
      value=\\\\\\"orig\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"citationAffixes\\\\\\" value=\\\\\\"\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"projectName\\\\\\" value=\\\\\\"\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"extractingLibraryID\\\\\\"
      value=\\\\\\"0\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"extractingLibraryName\\\\\\" value=\\\\\\"No group
      selected\\\\\\"/\<gtr\>\<less\>pref name=\\\\\\"fieldType\\\\\\"
      value=\\\\\\"ReferenceMark\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"storeReferences\\\\\\" value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"automaticJournalAbbreviations\\\\\\"
      value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"noteType\\\\\\" value=\\\\\\"0\\\\\\"/\<gtr\>\<less\>pref
      name=\\\\\\"suppressTrailingPunctuation\\\\\\"
      value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>/prefs\<gtr\>\<less\>/data\<gtr\>\\"]]")
    <|unfolded-io>
      ("Document_setDocumentData" (1 "\<less\>data data-version=\\"3\\"
      zotero-version=\\"4.0.29.8m73\\"\<gtr\>\<less\>session
      id=\\"iOE03FdV\\"/\<gtr\>\<less\>style
      id=\\"http://juris-m.github.io/styles/jm-indigobook-in-text\\"
      locale=\\"en-US\\" hasBibliography=\\"1\\"
      bibliographyStyleHasBeenSet=\\"0\\"/\<gtr\>\<less\>prefs\<gtr\>\<less\>pref
      name=\\"citationTransliteration\\" value=\\"\\"/\<gtr\>\<less\>pref
      name=\\"citationTranslation\\" value=\\"\\"/\<gtr\>\<less\>pref
      name=\\"citationSort\\" value=\\"\\"/\<gtr\>\<less\>pref
      name=\\"citationLangPrefsPersons\\"
      value=\\"orig\\"/\<gtr\>\<less\>pref
      name=\\"citationLangPrefsInstitutions\\"
      value=\\"orig\\"/\<gtr\>\<less\>pref name=\\"citationLangPrefsTitles\\"
      value=\\"orig\\"/\<gtr\>\<less\>pref
      name=\\"citationLangPrefsJournals\\"
      value=\\"orig\\"/\<gtr\>\<less\>pref
      name=\\"citationLangPrefsPublishers\\"
      value=\\"orig\\"/\<gtr\>\<less\>pref name=\\"citationLangPrefsPlaces\\"
      value=\\"orig\\"/\<gtr\>\<less\>pref name=\\"citationAffixes\\"
      value=\\"\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\\"/\<gtr\>\<less\>pref
      name=\\"projectName\\" value=\\"\\"/\<gtr\>\<less\>pref
      name=\\"extractingLibraryID\\" value=\\"0\\"/\<gtr\>\<less\>pref
      name=\\"extractingLibraryName\\" value=\\"No group
      selected\\"/\<gtr\>\<less\>pref name=\\"fieldType\\"
      value=\\"ReferenceMark\\"/\<gtr\>\<less\>pref
      name=\\"storeReferences\\" value=\\"true\\"/\<gtr\>\<less\>pref
      name=\\"automaticJournalAbbreviations\\"
      value=\\"true\\"/\<gtr\>\<less\>pref name=\\"noteType\\"
      value=\\"0\\"/\<gtr\>\<less\>pref name=\\"suppressTrailingPunctuation\\"
      value=\\"true\\"/\<gtr\>\<less\>/prefs\<gtr\>\<less\>/data\<gtr\>"))
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>
  </session>
</body>

<\initial>
  <\collection>
    <associate|font|concrete>
    <associate|font-base-size|12>
    <associate|page-even-footer|>
    <associate|page-even-header|>
    <associate|page-medium|paper>
    <associate|page-odd-footer|>
    <associate|page-odd-header|>
    <associate|page-screen-margin|false>
    <associate|page-type|letter>
  </collection>
</initial>