<TeXmacs|1.99.4>

<style|<tuple|article|tm-zotero>>

<\body>
  <\session|scheme|default>
    <\input|Scheme] >
      (load-from-path "zotero.scm")
    </input>

    <\input|Scheme] >
      (define res1 (begin (zotero-write 0 "\\"addCitation\\"")
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res1
    <|unfolded-io>
      (1 37 "[\\"Application_getActiveDocument\\",[3]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res2 (begin (zotero-write 1 (scm-\<gtr\>json-string (list 3
      1))) (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res2
    <|unfolded-io>
      (2 32 "[\\"Document_getDocumentData\\",[1]]")
    </unfolded-io>

    <\input|Scheme] >
      (zotero-write 2 (scm-\<gtr\>json-string ""))
    </input>

    <\input|Scheme] >
      (define res3 (zotero-select-then-read))
    </input>

    <\unfolded-io|Scheme] >
      res3
    <|unfolded-io>
      (3 1259 "[\\"Document_setDocumentData\\",[1,\\"\<less\>data
      data-version=\\\\\\"3\\\\\\" zotero-version=\\\\\\"4.0.29.9m75\\\\\\"\<gtr\>\<less\>session
      id=\\\\\\"tVgMxdNe\\\\\\"/\<gtr\>\<less\>style
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

    <\input|Scheme] >
      (define res4 (begin (zotero-write 3 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res4
    <|unfolded-io>
      (4 47 "[\\"Document_canInsertField\\",[1,\\"ReferenceMark\\"]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res5 (begin (zotero-write 4 (scm-\<gtr\>json-string #t))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res5
    <|unfolded-io>
      (5 46 "[\\"Document_cursorInField\\",[1,\\"ReferenceMark\\"]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res6 (begin (zotero-write 5 (scm-\<gtr\>json-string (list 1 ""
      0))) (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res6
    <|unfolded-io>
      (6 62 "[\\"Document_displayAlert\\",[1,\\"Replace this Zotero
      field?\\",0,1]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res7 (begin (zotero-write 6 (scm-\<gtr\>json-string 1))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res7
    <|unfolded-io>
      (7 30 "[\\"Field_setCode\\",[1,1,\\"TEMP\\"]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res8 (begin (zotero-write 7 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res8
    <|unfolded-io>
      (8 42 "[\\"Document_getFields\\",[1,\\"ReferenceMark\\"]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res9 (begin (zotero-write 8 (scm-\<gtr\>json-string (list (list
      1) (list "TEMP") (list 0)))) (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res9
    <|unfolded-io>
      (9 420 "[\\"Field_setText\\",[1,1,\\"\\\\\\\\textsc{Frank G. Bennett},
      \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero for Legal and
      Multilingual Research} (2013); dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\",true]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res10 (begin (zotero-write 9 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res10
    <|unfolded-io>
      (10 23 "[\\"Field_getText\\",[1,1]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res11 (begin (zotero-write 10 (scm-\<gtr\>json-string (caddr
      (cadr (json-string-\<gtr\>scm (caddr res9))))))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res11
    <|unfolded-io>
      (11 2248 "[\\"Field_setCode\\",[1,1,\\"ITEM CSL_CITATION
      {\\\\\\"citationID\\\\\\":\\\\\\"tBtN44vk\\\\\\",\\\\\\"properties\\\\\\":{\\\\\\"formattedCitation\\\\\\":\\\\\\"\\\\\\\\\\\\\\\\textsc{Frank
      G. Bennett}, \\\\\\\\\\\\\\\\textsc{Citations, Out of the Box: Adapting
      Zotero for Legal and Multilingual Research} (2013);
      dev:client\\\\\\\\\\\\\\\\_coding:libreoffice\\\\\\\\\\\\\\\\_plugin\\\\\\\\\\\\\\\\_wire\\\\\\\\\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\\\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\\\\\\\\\_coding/libreoffice\\\\\\\\\\\\\\\\_plugin\\\\\\\\\\\\\\\\_wire\\\\\\\\\\\\\\\\_protocol}.\\\\\\",\\\\\\"plainCitation\\\\\\":\\\\\\"\\\\\\\\\\\\\\\\textsc{Frank
      G. Bennett}, \\\\\\\\\\\\\\\\textsc{Citations, Out of the Box: Adapting
      Zotero for Legal and Multilingual Research} (2013);
      dev:client\\\\\\\\\\\\\\\\_coding:libreoffice\\\\\\\\\\\\\\\\_plugin\\\\\\\\\\\\\\\\_wire\\\\\\\\\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\\\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\\\\\\\\\_coding/libreoffice\\\\\\\\\\\\\\\\_plugin\\\\\\\\\\\\\\\\_wire\\\\\\\\\\\\\\\\_protocol}.\\\\\\"},\\\\\\"citationItems\\\\\\":[{\\\\\\"id\\\\\\":224,\\\\\\"uris\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/FXJZ3KQD\\\\\\"],\\\\\\"uri\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/FXJZ3KQD\\\\\\"],\\\\\\"itemData\\\\\\":{\\\\\\"type\\\\\\":\\\\\\"book\\\\\\",\\\\\\"title\\\\\\":\\\\\\"Citations,
      Out of the Box: Adapting Zotero for legal and multilingual
      research\\\\\\",\\\\\\"publisher\\\\\\":\\\\\\"CreateSpace Independent
      Publishing Platform\\\\\\",\\\\\\"number-of-pages\\\\\\":\\\\\\"148\\\\\\",\\\\\\"source\\\\\\":\\\\\\"Amazon.com\\\\\\",\\\\\\"abstract\\\\\\":\\\\\\"An
      introduction to MLZ, a multilingual prototype of the open-source
      reference manager Zotero with support for legal
      writing.\\\\\\",\\\\\\"ISBN\\\\\\":\\\\\\"978-1-4793-4771-1\\\\\\",\\\\\\"note\\\\\\":\\\\\\"00000
      \\\\\\\\nbibtex: bennett_citations_2013\\\\\\",\\\\\\"shortTitle\\\\\\":\\\\\\"Citations,
      Out of the Box\\\\\\",\\\\\\"language\\\\\\":\\\\\\"English\\\\\\",\\\\\\"author\\\\\\":[{\\\\\\"family\\\\\\":\\\\\\"Bennett\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Frank
      G.\\\\\\"}],\\\\\\"issued\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"May 25,
      2013\\\\\\"}}},{\\\\\\"id\\\\\\":3381,\\\\\\"uris\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/C3R97V46\\\\\\"],\\\\\\"uri\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/C3R97V46\\\\\\"],\\\\\\"itemData\\\\\\":{\\\\\\"type\\\\\\":\\\\\\"webpage\\\\\\",\\\\\\"title\\\\\\":\\\\\\"dev:client_coding:libreoffice_plugin_wire_protocol
      [Zotero Documentation]\\\\\\",\\\\\\"URL\\\\\\":\\\\\\"https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol\\\\\\",\\\\\\"accessed\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"2016-04-30T02:52:02Z\\\\\\"}}}],\\\\\\"schema\\\\\\":\\\\\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\\\\\"}\\"]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res12 (begin (zotero-write 11 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res12
    <|unfolded-io>
      (12 420 "[\\"Field_setText\\",[1,1,\\"\\\\\\\\textsc{Frank G. Bennett},
      \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero for Legal and
      Multilingual Research} (2013); dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\",true]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res13 (begin (zotero-write 12 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res13
    <|unfolded-io>
      (13 25 "[\\"Document_activate\\",[1]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res14 (begin (zotero-write 13 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res14
    <|unfolded-io>
      (14 25 "[\\"Document_complete\\",[1]]")
    </unfolded-io>

    <\input|Scheme] >
      (zotero-write 14 (scm-\<gtr\>json-string '()))
    </input>

    <\input|Scheme] >
      \;
    </input>

    <\input|Scheme] >
      (define res11-fieldCode (caddar (cdr (json-string-\<gtr\>scm (caddr
      res11)))))
    </input>

    <\unfolded-io|Scheme] >
      res11-fieldCode
    <|unfolded-io>
      "ITEM CSL_CITATION {\\"citationID\\":\\"N2mgDrxi\\",\\"properties\\":{\\"formattedCitation\\":\\"\\\\\\\\textsc{Frank
      G. Bennett}, \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero
      for Legal and Multilingual Research} (2013);
      dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\",\\"plainCitation\\":\\"\\\\\\\\textsc{Frank
      G. Bennett}, \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero
      for Legal and Multilingual Research} (2013);
      dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\"},\\"citationItems\\":[{\\"id\\":224,\\"uris\\":[\\"http://zotero.org/users/226074/items/FXJZ3KQD\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/FXJZ3KQD\\"],\\"itemData\\":{\\"type\\":\\"book\\",\\"title\\":\\"Citations,
      Out of the Box: Adapting Zotero for legal and multilingual
      research\\",\\"publisher\\":\\"CreateSpace Independent Publishing
      Platform\\",\\"number-of-pages\\":\\"148\\",\\"source\\":\\"Amazon.com\\",\\"abstract\\":\\"An
      introduction to MLZ, a multilingual prototype of the open-source
      reference manager Zotero with support for legal
      writing.\\",\\"ISBN\\":\\"978-1-4793-4771-1\\",\\"note\\":\\"00000
      \\\\nbibtex: bennett_citations_2013\\",\\"shortTitle\\":\\"Citations,
      Out of the Box\\",\\"language\\":\\"English\\",\\"author\\":[{\\"family\\":\\"Bennett\\",\\"given\\":\\"Frank
      G.\\"}],\\"issued\\":{\\"raw\\":\\"May 25,
      2013\\"}}},{\\"id\\":3381,\\"uris\\":[\\"http://zotero.org/users/226074/items/C3R97V46\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/C3R97V46\\"],\\"itemData\\":{\\"type\\":\\"webpage\\",\\"title\\":\\"dev:client_coding:libreoffice_plugin_wire_protocol
      [Zotero Documentation]\\",\\"URL\\":\\"https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol\\",\\"accessed\\":{\\"raw\\":\\"2016-04-30T02:52:02Z\\"}}}],\\"schema\\":\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\"}"
    </unfolded-io>

    <\input|Scheme] >
      (define res11-fieldCode-csl-json-string (substring res11-fieldCode 18
      (string-length res11-fieldCode)))
    </input>

    <\unfolded-io|Scheme] >
      res11-fieldCode-csl-json-string
    <|unfolded-io>
      "{\\"citationID\\":\\"N2mgDrxi\\",\\"properties\\":{\\"formattedCitation\\":\\"\\\\\\\\textsc{Frank
      G. Bennett}, \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero
      for Legal and Multilingual Research} (2013);
      dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\",\\"plainCitation\\":\\"\\\\\\\\textsc{Frank
      G. Bennett}, \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero
      for Legal and Multilingual Research} (2013);
      dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\"},\\"citationItems\\":[{\\"id\\":224,\\"uris\\":[\\"http://zotero.org/users/226074/items/FXJZ3KQD\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/FXJZ3KQD\\"],\\"itemData\\":{\\"type\\":\\"book\\",\\"title\\":\\"Citations,
      Out of the Box: Adapting Zotero for legal and multilingual
      research\\",\\"publisher\\":\\"CreateSpace Independent Publishing
      Platform\\",\\"number-of-pages\\":\\"148\\",\\"source\\":\\"Amazon.com\\",\\"abstract\\":\\"An
      introduction to MLZ, a multilingual prototype of the open-source
      reference manager Zotero with support for legal
      writing.\\",\\"ISBN\\":\\"978-1-4793-4771-1\\",\\"note\\":\\"00000
      \\\\nbibtex: bennett_citations_2013\\",\\"shortTitle\\":\\"Citations,
      Out of the Box\\",\\"language\\":\\"English\\",\\"author\\":[{\\"family\\":\\"Bennett\\",\\"given\\":\\"Frank
      G.\\"}],\\"issued\\":{\\"raw\\":\\"May 25,
      2013\\"}}},{\\"id\\":3381,\\"uris\\":[\\"http://zotero.org/users/226074/items/C3R97V46\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/C3R97V46\\"],\\"itemData\\":{\\"type\\":\\"webpage\\",\\"title\\":\\"dev:client_coding:libreoffice_plugin_wire_protocol
      [Zotero Documentation]\\",\\"URL\\":\\"https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol\\",\\"accessed\\":{\\"raw\\":\\"2016-04-30T02:52:02Z\\"}}}],\\"schema\\":\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\"}"
    </unfolded-io>

    <\input|Scheme] >
      (define res11-ht (json-string-\<gtr\>scm
      res11-fieldCode-csl-json-string))
    </input>

    <\unfolded-io|Scheme] >
      (hash-map-\<gtr\>list (lambda (key val) (cons key val)) res11-ht)
    <|unfolded-io>
      (("properties" . #\<less\>hash-table 2/31\<gtr\>) ("citationID" .
      "N2mgDrxi") ("schema" . "https://github.com/citation-style-language/schema/raw/master/csl-citation.json")
      ("citationItems" #\<less\>hash-table 4/31\<gtr\> #\<less\>hash-table
      4/31\<gtr\>))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (map (lambda (ht) (hash-map-\<gtr\>list (lambda (key val) (cons key
      val)) ht)) (hash-ref res11-ht "citationItems"))
    <|unfolded-io>
      ((("itemData" . #\<less\>hash-table 12/31\<gtr\>) ("uris"
      "http://zotero.org/users/226074/items/FXJZ3KQD") ("id" . 224) ("uri"
      "http://zotero.org/users/226074/items/FXJZ3KQD")) (("itemData" .
      #\<less\>hash-table 4/31\<gtr\>) ("uris"
      "http://zotero.org/users/226074/items/C3R97V46") ("id" . 3381) ("uri"
      "http://zotero.org/users/226074/items/C3R97V46")))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (hash-map-\<gtr\>list (lambda (key val) (cons key val)) (hash-ref
      res11-ht "properties"))
    <|unfolded-io>
      (("plainCitation" . "\\\\textsc{Frank G. Bennett},
      \\\\textsc{Citations, Out of the Box: Adapting Zotero for Legal and
      Multilingual Research} (2013); dev:client\\\\_coding:libreoffice\\\\_plugin\\\\_wire\\\\_protocol
      [Zotero Documentation], \\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\_coding/libreoffice\\\\_plugin\\\\_wire\\\\_protocol}.")
      ("formattedCitation" . "\\\\textsc{Frank G. Bennett},
      \\\\textsc{Citations, Out of the Box: Adapting Zotero for Legal and
      Multilingual Research} (2013); dev:client\\\\_coding:libreoffice\\\\_plugin\\\\_wire\\\\_protocol
      [Zotero Documentation], \\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\_coding/libreoffice\\\\_plugin\\\\_wire\\\\_protocol}."))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      res12
    <|unfolded-io>
      (12 420 "[\\"Field_setText\\",[1,1,\\"\\\\\\\\textsc{Frank G. Bennett},
      \\\\\\\\textsc{Citations, Out of the Box: Adapting Zotero for Legal and
      Multilingual Research} (2013); dev:client\\\\\\\\_coding:libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol
      [Zotero Documentation], \\\\\\\\href{https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol}{https://www.zotero.org/support/dev/client\\\\\\\\_coding/libreoffice\\\\\\\\_plugin\\\\\\\\_wire\\\\\\\\_protocol}.\\",true]]")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (latex-\<gtr\>texmacs (parse-latex (third (cadr (json-string-\<gtr\>scm
      (caddr res12))))))
    <|unfolded-io>
      <text|<with|font-shape|small-caps|Frank G. Bennett>,
      <with|font-shape|small-caps|Citations, Out of the Box: Adapting Zotero
      for Legal and Multilingual Research> (2013);
      dev:client_coding:libreoffice_plugin_wire_protocol [Zotero
      Documentation], <hlink|https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol|https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol>.>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      res1
    <|unfolded-io>
      (1 37 "[\\"Application_getActiveDocument\\",[3]]")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm (caddr res1))
    <|unfolded-io>
      ("Application_getActiveDocument" (3))
    </unfolded-io>

    <\unfolded-io|Scheme] >
      res3
    <|unfolded-io>
      (3 1259 "[\\"Document_setDocumentData\\",[1,\\"\<less\>data
      data-version=\\\\\\"3\\\\\\" zotero-version=\\\\\\"4.0.29.9m75\\\\\\"\<gtr\>\<less\>session
      id=\\\\\\"tVgMxdNe\\\\\\"/\<gtr\>\<less\>style
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
      (zotero-Document_setDocumentData 1 (cadadr (json-string-\<gtr\>scm
      (caddr res3))))
    <|unfolded-io>
      <errput|Wrong type (expecting pair): "u«Zu«Z½êì\\x8a\\x89">
    </unfolded-io>

    <\input|Scheme] >
      (init-env "zoteroDocumentData" (cadadr (json-string-\<gtr\>scm (caddr
      res3))))
    </input>

    <\unfolded-io|Scheme] >
      (zotero-set-DocumentData (zotero-get-DocumentData))
    <|unfolded-io>
      #t
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (eq? 'data (sxml-name (car (cdr (parse-xml
      (zotero-get-DocumentData))))))
    <|unfolded-io>
      #t
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (get-env "zoteroDocumentData")
    <|unfolded-io>
      "\<less\>data data-version=\\"3\\" zotero-version=\\"4.0.29.9m75\\"\<gtr\>\<less\>session
      id=\\"tVgMxdNe\\"/\<gtr\>\<less\>style
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
      value=\\"true\\"/\<gtr\>\<less\>/prefs\<gtr\>\<less\>/data\<gtr\>"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (zotero-init-env-zotero-prefs)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      (init-env "zotero-pref-noteType" "1")
    </input>

    <\unfolded-io|Scheme] >
      (get-env "zotero-pref-noteType")
    <|unfolded-io>
      "1"
    </unfolded-io>

    \;

    <\unfolded-io|Scheme] >
      (get-style-list)
    <|unfolded-io>
      ("article" "tm-zotero")
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-style-package? "tm-zotero")
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      (add-style-package "tm-zotero")
    </input>

    <\unfolded-io|Scheme] >
      (acm-style?)
    <|unfolded-io>
      #f
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (zotero-style?)
    <|unfolded-io>
      <errput|Unbound variable: zotero-style?>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (has-style-package?)
    <|unfolded-io>
      <errput|Wrong number of arguments to #\<procedure has-style-package?
      (pack)\>>
    </unfolded-io>

    <\input|Scheme] >
      (init-env "testingVar" "testingVal")
    </input>

    <\unfolded-io|Scheme] >
      (get-env "testingVar")
    <|unfolded-io>
      "testingVal"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (let ((cite-fields (tm-search-tag (buffer-tree) 'zcite)))

      \ \ 
    <|unfolded-io>
      (\<less\>tree \<less\>zcite\|id01\|code\|0\|Text\<gtr\>\<gtr\>
      \<less\>tree \<less\>zcite\|id02\|code\|0\|Text2\<gtr\>\<gtr\>
      \<less\>tree \<less\>zcite\|id03\|code\|0\|Text3\<gtr\>\<gtr\>)
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>

    \;
  </session>

  <zcite|id01|code|0|Text>

  <zcite|id02|code|0|Text2>

  <zcite|id03|code|0|Text3>
</body>

<\initial>
  <\collection>
    <associate|font|roman>
    <associate|font-base-size|10>
    <associate|math-font|roman>
    <associate|page-even-footer|>
    <associate|page-even-header|>
    <associate|page-medium|paper>
    <associate|page-odd-footer|>
    <associate|page-odd-header|>
    <associate|page-screen-margin|false>
    <associate|page-type|letter>
    <associate|preamble|false>
    <associate|testingVar|testingVal>
    <associate|zotero-data-data-version|3>
    <associate|zotero-data-zotero-version|4.0.29.9m75>
    <associate|zotero-pref-automaticJournalAbbreviations|true>
    <associate|zotero-pref-citationAffixes|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|>
    <associate|zotero-pref-citationLangPrefsInstitutions|orig>
    <associate|zotero-pref-citationLangPrefsJournals|orig>
    <associate|zotero-pref-citationLangPrefsPersons|orig>
    <associate|zotero-pref-citationLangPrefsPlaces|orig>
    <associate|zotero-pref-citationLangPrefsPublishers|orig>
    <associate|zotero-pref-citationLangPrefsTitles|orig>
    <associate|zotero-pref-citationSort|>
    <associate|zotero-pref-citationTranslation|>
    <associate|zotero-pref-citationTransliteration|>
    <associate|zotero-pref-extractingLibraryID|0>
    <associate|zotero-pref-extractingLibraryName|No group selected>
    <associate|zotero-pref-fieldType|ReferenceMark>
    <associate|zotero-pref-noteType|1>
    <associate|zotero-pref-projectName|>
    <associate|zotero-pref-storeReferences|true>
    <associate|zotero-pref-suppressTrailingPunctuation|true>
    <associate|zotero-session-id|tVgMxdNe>
    <associate|zotero-style-bibliographyStyleHasBeenSet|0>
    <associate|zotero-style-hasBibliography|1>
    <associate|zotero-style-id|http://juris-m.github.io/styles/jm-indigobook-in-text>
    <associate|zotero-style-locale|en-US>
    <associate|zoteroDocumentData|\<data data-version="3"
    zotero-version="4.0.29.9m75"\>\<session id="tVgMxdNe"/\>\<style
    id="http://juris-m.github.io/styles/jm-indigobook-in-text" locale="en-US"
    hasBibliography="1" bibliographyStyleHasBeenSet="0"/\>\<prefs\>\<pref
    name="citationTransliteration" value=""/\>\<pref
    name="citationTranslation" value=""/\>\<pref name="citationSort"
    value=""/\>\<pref name="citationLangPrefsPersons" value="orig"/\>\<pref
    name="citationLangPrefsInstitutions" value="orig"/\>\<pref
    name="citationLangPrefsTitles" value="orig"/\>\<pref
    name="citationLangPrefsJournals" value="orig"/\>\<pref
    name="citationLangPrefsPublishers" value="orig"/\>\<pref
    name="citationLangPrefsPlaces" value="orig"/\>\<pref
    name="citationAffixes" value="\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|"/\>\<pref
    name="projectName" value=""/\>\<pref name="extractingLibraryID"
    value="0"/\>\<pref name="extractingLibraryName" value="No group
    selected"/\>\<pref name="fieldType" value="ReferenceMark"/\>\<pref
    name="storeReferences" value="true"/\>\<pref
    name="automaticJournalAbbreviations" value="true"/\>\<pref
    name="noteType" value="0"/\>\<pref name="suppressTrailingPunctuation"
    value="true"/\>\</prefs\>\</data\>>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|footnote-1|<tuple|1|6>>
    <associate|footnote-2|<tuple|2|6>>
    <associate|footnote-3|<tuple|3|6>>
    <associate|footnr-1|<tuple|1|6>>
    <associate|footnr-2|<tuple|2|6>>
    <associate|footnr-3|<tuple|3|6>>
  </collection>
</references>