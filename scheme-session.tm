<TeXmacs|1.99.4>

<style|article>

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
      (define res1 '(1 37 "[\\"Application_getActiveDocument\\",[3]]"))
    </input>

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
      (define res2 '(2 32 "[\\"Document_getDocumentData\\",[1]]"))
    </input>

    <\input|Scheme] >
      (define res3 (begin (zotero-write 2 (scm-\<gtr\>json-string ""))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res3
    <|unfolded-io>
      (3 1259 "[\\"Document_setDocumentData\\",[1,\\"\<less\>data
      data-version=\\\\\\"3\\\\\\" zotero-version=\\\\\\"4.0.29.8m73\\\\\\"\<gtr\>\<less\>session
      id=\\\\\\"1ejMCIXf\\\\\\"/\<gtr\>\<less\>style
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
      (define res3 '(3 1259 "[\\"Document_setDocumentData\\",[1,\\"\<less\>data
      data-version=\\\\\\"3\\\\\\" zotero-version=\\\\\\"4.0.29.8m73\\\\\\"\<gtr\>\<less\>session
      id=\\\\\\"1ejMCIXf\\\\\\"/\<gtr\>\<less\>style
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
      value=\\\\\\"true\\\\\\"/\<gtr\>\<less\>/prefs\<gtr\>\<less\>/data\<gtr\>\\"]]"))
    </input>

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
      (define res4 '(4 47 "[\\"Document_canInsertField\\",[1,\\"ReferenceMark\\"]]"))
    </input>

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
      (define res5 '(5 46 "[\\"Document_cursorInField\\",[1,\\"ReferenceMark\\"]]"))
    </input>

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
      (define res6 '(6 62 "[\\"Document_displayAlert\\",[1,\\"Replace this
      Zotero field?\\",0,1]]"))
    </input>

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
      (define res7 '(7 30 "[\\"Field_setCode\\",[1,1,\\"TEMP\\"]]"))
    </input>

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
      (define res8 '(8 42 "[\\"Document_getFields\\",[1,\\"ReferenceMark\\"]]"))
    </input>

    <\input|Scheme] >
      (define res9 (begin (zotero-write 8 (scm-\<gtr\>json-string (list (list
      1) (list "TEMP") (list 0)))) (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res9
    <|unfolded-io>
      (9 313 "[\\"Field_setText\\",[1,1,\\"{\\\\\\\\rtf 1 {\\\\\\\\scaps Paul
      W. Abrahams et al.}, {\\\\\\\\scaps TEX for the Impatient} (1990);
      Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE
      WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED
      3T fMRI STUDY}, 102 {\\\\\\\\scaps Schizophrenia Research} 91
      (2008).}\\",true]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res9 '(9 313 "[\\"Field_setText\\",[1,1,\\"{\\\\\\\\rtf 1
      {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps
      Schizophrenia Research} 91 (2008).}\\",true]]"))
    </input>

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
      (define res10 '(10 23 "[\\"Field_getText\\",[1,1]]"))
    </input>

    <\input|Scheme] >
      (define res11 (begin (zotero-write 10 (scm-\<gtr\>json-string (caddr
      (cadr (json-string-\<gtr\>scm (caddr res9))))))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res11
    <|unfolded-io>
      (11 3795 "[\\"Field_setCode\\",[1,1,\\"ITEM CSL_CITATION
      {\\\\\\"citationID\\\\\\":\\\\\\"4TGwf4LB\\\\\\",\\\\\\"properties\\\\\\":{\\\\\\"formattedCitation\\\\\\":\\\\\\"{\\\\\\\\\\\\\\\\rtf
      1 {\\\\\\\\\\\\\\\\scaps Paul W. Abrahams et al.},
      {\\\\\\\\\\\\\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann
      et al., {\\\\\\\\\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN
      CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI
      STUDY}, 102 {\\\\\\\\\\\\\\\\scaps Schizophrenia Research} 91
      (2008).}\\\\\\",\\\\\\"plainCitation\\\\\\":\\\\\\"{\\\\\\\\\\\\\\\\rtf
      1 {\\\\\\\\\\\\\\\\scaps Paul W. Abrahams et al.},
      {\\\\\\\\\\\\\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann
      et al., {\\\\\\\\\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN
      CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI
      STUDY}, 102 {\\\\\\\\\\\\\\\\scaps Schizophrenia Research} 91
      (2008).}\\\\\\"},\\\\\\"citationItems\\\\\\":[{\\\\\\"id\\\\\\":2795,\\\\\\"uris\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/DJEFXEG9\\\\\\"],\\\\\\"uri\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/DJEFXEG9\\\\\\"],\\\\\\"itemData\\\\\\":{\\\\\\"type\\\\\\":\\\\\\"book\\\\\\",\\\\\\"title\\\\\\":\\\\\\"TEX
      for the Impatient\\\\\\",\\\\\\"publisher\\\\\\":\\\\\\"Addison-Wesley\\\\\\",\\\\\\"volume\\\\\\":\\\\\\"1\\\\\\",\\\\\\"ISBN\\\\\\":\\\\\\"978-0-201-51375-2\\\\\\",\\\\\\"note\\\\\\":\\\\\\"TEX,
      a software system created by Donald E. Knuth, sets the standard for
      typesetting in mathematics, science, and engineering. Features:
      complete desciption of TEX commands, arranged for lookup either by
      function or alphabetically; clear definitions of essential TEX
      concepts, collected in separate chapter so that the command
      descriptions remain brief and accessible; explanations of common error
      messages and advice on solving problems that frequently arise; a
      collection of useful macros (also available in electronic form).
      \\\\\\\\n \\\\\\\\nbibtex[author_sort=Abrahams, Paul W. & Berry, Karl &
      Hargreaves, Kathryn A.;identifiers=amazon:0201513757,google:9wwZAQAAIAAJ,isbn:9780201513752;languages=eng;library_name=Calibre
      Library;rating=8;size=1421239 octets;tags=Computerized Typesetting,
      Mathematics Printing - Computer Programs, Mathematics Printing,
      Computer Programs, TeX (Computer File), TeX (Computer System),
      Computers, General, Digital Media, Desktop Publishing, Programming
      Languages, Design, Graphic Arts, Typography;title_sort=TEX for the
      Impatient] \\\\\\\\nbibtex: abrahams_tex_1990\\\\\\",\\\\\\"author\\\\\\":[{\\\\\\"family\\\\\\":\\\\\\"Abrahams\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Paul
      W.\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Berry\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Karl\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Hargreaves\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Kathryn
      A.\\\\\\"}],\\\\\\"issued\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"February
      1990\\\\\\"}}},{\\\\\\"id\\\\\\":455,\\\\\\"uris\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/RNMTIJ22\\\\\\"],\\\\\\"uri\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/RNMTIJ22\\\\\\"],\\\\\\"itemData\\\\\\":{\\\\\\"type\\\\\\":\\\\\\"article-journal\\\\\\",\\\\\\"title\\\\\\":\\\\\\"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY\\\\\\",\\\\\\"container-title\\\\\\":\\\\\\"Schizophrenia
      Research\\\\\\",\\\\\\"page\\\\\\":\\\\\\"91\\\\\\",\\\\\\"volume\\\\\\":\\\\\\"102\\\\\\",\\\\\\"issue\\\\\\":\\\\\\"1-3,
      Supplement 2\\\\\\",\\\\\\"source\\\\\\":\\\\\\"ScienceDirect\\\\\\",\\\\\\"URL\\\\\\":\\\\\\"http://www.sciencedirect.com/science/article/B6TC2-4SX99F1-9T/2/8e8f14104a436c0190375b6f604914e1\\\\\\",\\\\\\"DOI\\\\\\":\\\\\\"10.1016/S0920-9964(08)70275-5\\\\\\",\\\\\\"ISSN\\\\\\":\\\\\\"0920-9964\\\\\\",\\\\\\"note\\\\\\":\\\\\\"bibtex:
      ohrmann_functional_2008\\\\\\",\\\\\\"shortTitle\\\\\\":\\\\\\"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS\\\\\\",\\\\\\"author\\\\\\":[{\\\\\\"family\\\\\\":\\\\\\"Ohrmann\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Patricia\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Wilmsmeier\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Andreas\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Bauer\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Jochen\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Siegmund\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Ansgar\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Suslow\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Thomas\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Wiedl\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Karl\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Koelkebeck\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Katja\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Kugel\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Harald\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Rothermundt\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Matthias\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Arolt\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Volker\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Pedersen\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Anya\\\\\\"}],\\\\\\"issued\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"June
      2008\\\\\\"},\\\\\\"accessed\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"2010-04-23T18:51:58Z\\\\\\"}}}],\\\\\\"schema\\\\\\":\\\\\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\\\\\"}\\"]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res11 '(11 3795 "[\\"Field_setCode\\",[1,1,\\"ITEM CSL_CITATION
      {\\\\\\"citationID\\\\\\":\\\\\\"4TGwf4LB\\\\\\",\\\\\\"properties\\\\\\":{\\\\\\"formattedCitation\\\\\\":\\\\\\"{\\\\\\\\\\\\\\\\rtf
      1 {\\\\\\\\\\\\\\\\scaps Paul W. Abrahams et al.},
      {\\\\\\\\\\\\\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann
      et al., {\\\\\\\\\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN
      CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI
      STUDY}, 102 {\\\\\\\\\\\\\\\\scaps Schizophrenia Research} 91
      (2008).}\\\\\\",\\\\\\"plainCitation\\\\\\":\\\\\\"{\\\\\\\\\\\\\\\\rtf
      1 {\\\\\\\\\\\\\\\\scaps Paul W. Abrahams et al.},
      {\\\\\\\\\\\\\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann
      et al., {\\\\\\\\\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN
      CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI
      STUDY}, 102 {\\\\\\\\\\\\\\\\scaps Schizophrenia Research} 91
      (2008).}\\\\\\"},\\\\\\"citationItems\\\\\\":[{\\\\\\"id\\\\\\":2795,\\\\\\"uris\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/DJEFXEG9\\\\\\"],\\\\\\"uri\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/DJEFXEG9\\\\\\"],\\\\\\"itemData\\\\\\":{\\\\\\"type\\\\\\":\\\\\\"book\\\\\\",\\\\\\"title\\\\\\":\\\\\\"TEX
      for the Impatient\\\\\\",\\\\\\"publisher\\\\\\":\\\\\\"Addison-Wesley\\\\\\",\\\\\\"volume\\\\\\":\\\\\\"1\\\\\\",\\\\\\"ISBN\\\\\\":\\\\\\"978-0-201-51375-2\\\\\\",\\\\\\"note\\\\\\":\\\\\\"TEX,
      a software system created by Donald E. Knuth, sets the standard for
      typesetting in mathematics, science, and engineering. Features:
      complete desciption of TEX commands, arranged for lookup either by
      function or alphabetically; clear definitions of essential TEX
      concepts, collected in separate chapter so that the command
      descriptions remain brief and accessible; explanations of common error
      messages and advice on solving problems that frequently arise; a
      collection of useful macros (also available in electronic form).
      \\\\\\\\n \\\\\\\\nbibtex[author_sort=Abrahams, Paul W. & Berry, Karl &
      Hargreaves, Kathryn A.;identifiers=amazon:0201513757,google:9wwZAQAAIAAJ,isbn:9780201513752;languages=eng;library_name=Calibre
      Library;rating=8;size=1421239 octets;tags=Computerized Typesetting,
      Mathematics Printing - Computer Programs, Mathematics Printing,
      Computer Programs, TeX (Computer File), TeX (Computer System),
      Computers, General, Digital Media, Desktop Publishing, Programming
      Languages, Design, Graphic Arts, Typography;title_sort=TEX for the
      Impatient] \\\\\\\\nbibtex: abrahams_tex_1990\\\\\\",\\\\\\"author\\\\\\":[{\\\\\\"family\\\\\\":\\\\\\"Abrahams\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Paul
      W.\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Berry\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Karl\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Hargreaves\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Kathryn
      A.\\\\\\"}],\\\\\\"issued\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"February
      1990\\\\\\"}}},{\\\\\\"id\\\\\\":455,\\\\\\"uris\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/RNMTIJ22\\\\\\"],\\\\\\"uri\\\\\\":[\\\\\\"http://zotero.org/users/226074/items/RNMTIJ22\\\\\\"],\\\\\\"itemData\\\\\\":{\\\\\\"type\\\\\\":\\\\\\"article-journal\\\\\\",\\\\\\"title\\\\\\":\\\\\\"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY\\\\\\",\\\\\\"container-title\\\\\\":\\\\\\"Schizophrenia
      Research\\\\\\",\\\\\\"page\\\\\\":\\\\\\"91\\\\\\",\\\\\\"volume\\\\\\":\\\\\\"102\\\\\\",\\\\\\"issue\\\\\\":\\\\\\"1-3,
      Supplement 2\\\\\\",\\\\\\"source\\\\\\":\\\\\\"ScienceDirect\\\\\\",\\\\\\"URL\\\\\\":\\\\\\"http://www.sciencedirect.com/science/article/B6TC2-4SX99F1-9T/2/8e8f14104a436c0190375b6f604914e1\\\\\\",\\\\\\"DOI\\\\\\":\\\\\\"10.1016/S0920-9964(08)70275-5\\\\\\",\\\\\\"ISSN\\\\\\":\\\\\\"0920-9964\\\\\\",\\\\\\"note\\\\\\":\\\\\\"bibtex:
      ohrmann_functional_2008\\\\\\",\\\\\\"shortTitle\\\\\\":\\\\\\"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS\\\\\\",\\\\\\"author\\\\\\":[{\\\\\\"family\\\\\\":\\\\\\"Ohrmann\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Patricia\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Wilmsmeier\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Andreas\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Bauer\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Jochen\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Siegmund\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Ansgar\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Suslow\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Thomas\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Wiedl\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Karl\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Koelkebeck\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Katja\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Kugel\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Harald\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Rothermundt\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Matthias\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Arolt\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Volker\\\\\\"},{\\\\\\"family\\\\\\":\\\\\\"Pedersen\\\\\\",\\\\\\"given\\\\\\":\\\\\\"Anya\\\\\\"}],\\\\\\"issued\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"June
      2008\\\\\\"},\\\\\\"accessed\\\\\\":{\\\\\\"raw\\\\\\":\\\\\\"2010-04-23T18:51:58Z\\\\\\"}}}],\\\\\\"schema\\\\\\":\\\\\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\\\\\"}\\"]]"))
    </input>

    <\input|Scheme] >
      (define res12 (begin (zotero-write 11 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res12
    <|unfolded-io>
      (12 313 "[\\"Field_setText\\",[1,1,\\"{\\\\\\\\rtf 1 {\\\\\\\\scaps
      Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the Impatient} (1990);
      Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE
      WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED
      3T fMRI STUDY}, 102 {\\\\\\\\scaps Schizophrenia Research} 91
      (2008).}\\",true]]")
    </unfolded-io>

    <\input|Scheme] >
      (define res12 '(12 313 "[\\"Field_setText\\",[1,1,\\"{\\\\\\\\rtf 1
      {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps
      Schizophrenia Research} 91 (2008).}\\",true]]"))
    </input>

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
      (define res13 '(13 25 "[\\"Document_activate\\",[1]]"))
    </input>

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
      (define res14 '(14 25 "[\\"Document_complete\\",[1]]"))
    </input>

    <\input|Scheme] >
      (define res15 (begin (zotero-write 14 (scm-\<gtr\>json-string '()))
      (zotero-select-then-read)))
    </input>

    <\unfolded-io|Scheme] >
      res15
    <|unfolded-io>
      (0 0 "")
    </unfolded-io>

    <\input|Scheme] >
      (define res15 '(0 0 ""))
    </input>

    <\input|Scheme] >
      \;
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
      "ITEM CSL_CITATION {\\"citationID\\":\\"4TGwf4LB\\",\\"properties\\":{\\"formattedCitation\\":\\"{\\\\\\\\rtf
      1 {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps
      Schizophrenia Research} 91 (2008).}\\",\\"plainCitation\\":\\"{\\\\\\\\rtf
      1 {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps
      Schizophrenia Research} 91 (2008).}\\"},\\"citationItems\\":[{\\"id\\":2795,\\"uris\\":[\\"http://zotero.org/users/226074/items/DJEFXEG9\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/DJEFXEG9\\"],\\"itemData\\":{\\"type\\":\\"book\\",\\"title\\":\\"TEX
      for the Impatient\\",\\"publisher\\":\\"Addison-Wesley\\",\\"volume\\":\\"1\\",\\"ISBN\\":\\"978-0-201-51375-2\\",\\"note\\":\\"TEX,
      a software system created by Donald E. Knuth, sets the standard for
      typesetting in mathematics, science, and engineering. Features:
      complete desciption of TEX commands, arranged for lookup either by
      function or alphabetically; clear definitions of essential TEX
      concepts, collected in separate chapter so that the command
      descriptions remain brief and accessible; explanations of common error
      messages and advice on solving problems that frequently arise; a
      collection of useful macros (also available in electronic form). \\\\n
      \\\\nbibtex[author_sort=Abrahams, Paul W. & Berry, Karl & Hargreaves,
      Kathryn A.;identifiers=amazon:0201513757,google:9wwZAQAAIAAJ,isbn:9780201513752;languages=eng;library_name=Calibre
      Library;rating=8;size=1421239 octets;tags=Computerized Typesetting,
      Mathematics Printing - Computer Programs, Mathematics Printing,
      Computer Programs, TeX (Computer File), TeX (Computer System),
      Computers, General, Digital Media, Desktop Publishing, Programming
      Languages, Design, Graphic Arts, Typography;title_sort=TEX for the
      Impatient] \\\\nbibtex: abrahams_tex_1990\\",\\"author\\":[{\\"family\\":\\"Abrahams\\",\\"given\\":\\"Paul
      W.\\"},{\\"family\\":\\"Berry\\",\\"given\\":\\"Karl\\"},{\\"family\\":\\"Hargreaves\\",\\"given\\":\\"Kathryn
      A.\\"}],\\"issued\\":{\\"raw\\":\\"February
      1990\\"}}},{\\"id\\":455,\\"uris\\":[\\"http://zotero.org/users/226074/items/RNMTIJ22\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/RNMTIJ22\\"],\\"itemData\\":{\\"type\\":\\"article-journal\\",\\"title\\":\\"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY\\",\\"container-title\\":\\"Schizophrenia
      Research\\",\\"page\\":\\"91\\",\\"volume\\":\\"102\\",\\"issue\\":\\"1-3,
      Supplement 2\\",\\"source\\":\\"ScienceDirect\\",\\"URL\\":\\"http://www.sciencedirect.com/science/article/B6TC2-4SX99F1-9T/2/8e8f14104a436c0190375b6f604914e1\\",\\"DOI\\":\\"10.1016/S0920-9964(08)70275-5\\",\\"ISSN\\":\\"0920-9964\\",\\"note\\":\\"bibtex:
      ohrmann_functional_2008\\",\\"shortTitle\\":\\"FUNCTIONAL NEUROANATOMY
      OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS\\",\\"author\\":[{\\"family\\":\\"Ohrmann\\",\\"given\\":\\"Patricia\\"},{\\"family\\":\\"Wilmsmeier\\",\\"given\\":\\"Andreas\\"},{\\"family\\":\\"Bauer\\",\\"given\\":\\"Jochen\\"},{\\"family\\":\\"Siegmund\\",\\"given\\":\\"Ansgar\\"},{\\"family\\":\\"Suslow\\",\\"given\\":\\"Thomas\\"},{\\"family\\":\\"Wiedl\\",\\"given\\":\\"Karl\\"},{\\"family\\":\\"Koelkebeck\\",\\"given\\":\\"Katja\\"},{\\"family\\":\\"Kugel\\",\\"given\\":\\"Harald\\"},{\\"family\\":\\"Rothermundt\\",\\"given\\":\\"Matthias\\"},{\\"family\\":\\"Arolt\\",\\"given\\":\\"Volker\\"},{\\"family\\":\\"Pedersen\\",\\"given\\":\\"Anya\\"}],\\"issued\\":{\\"raw\\":\\"June
      2008\\"},\\"accessed\\":{\\"raw\\":\\"2010-04-23T18:51:58Z\\"}}}],\\"schema\\":\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\"}"
    </unfolded-io>

    <\input|Scheme] >
      (define res11-fieldCode-csl-json-string (substring res11-fieldCode 18
      (string-length res11-fieldCode)))
    </input>

    <\unfolded-io|Scheme] >
      res11-fieldCode-csl-json-string
    <|unfolded-io>
      "{\\"citationID\\":\\"4TGwf4LB\\",\\"properties\\":{\\"formattedCitation\\":\\"{\\\\\\\\rtf
      1 {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps
      Schizophrenia Research} 91 (2008).}\\",\\"plainCitation\\":\\"{\\\\\\\\rtf
      1 {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps
      Schizophrenia Research} 91 (2008).}\\"},\\"citationItems\\":[{\\"id\\":2795,\\"uris\\":[\\"http://zotero.org/users/226074/items/DJEFXEG9\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/DJEFXEG9\\"],\\"itemData\\":{\\"type\\":\\"book\\",\\"title\\":\\"TEX
      for the Impatient\\",\\"publisher\\":\\"Addison-Wesley\\",\\"volume\\":\\"1\\",\\"ISBN\\":\\"978-0-201-51375-2\\",\\"note\\":\\"TEX,
      a software system created by Donald E. Knuth, sets the standard for
      typesetting in mathematics, science, and engineering. Features:
      complete desciption of TEX commands, arranged for lookup either by
      function or alphabetically; clear definitions of essential TEX
      concepts, collected in separate chapter so that the command
      descriptions remain brief and accessible; explanations of common error
      messages and advice on solving problems that frequently arise; a
      collection of useful macros (also available in electronic form). \\\\n
      \\\\nbibtex[author_sort=Abrahams, Paul W. & Berry, Karl & Hargreaves,
      Kathryn A.;identifiers=amazon:0201513757,google:9wwZAQAAIAAJ,isbn:9780201513752;languages=eng;library_name=Calibre
      Library;rating=8;size=1421239 octets;tags=Computerized Typesetting,
      Mathematics Printing - Computer Programs, Mathematics Printing,
      Computer Programs, TeX (Computer File), TeX (Computer System),
      Computers, General, Digital Media, Desktop Publishing, Programming
      Languages, Design, Graphic Arts, Typography;title_sort=TEX for the
      Impatient] \\\\nbibtex: abrahams_tex_1990\\",\\"author\\":[{\\"family\\":\\"Abrahams\\",\\"given\\":\\"Paul
      W.\\"},{\\"family\\":\\"Berry\\",\\"given\\":\\"Karl\\"},{\\"family\\":\\"Hargreaves\\",\\"given\\":\\"Kathryn
      A.\\"}],\\"issued\\":{\\"raw\\":\\"February
      1990\\"}}},{\\"id\\":455,\\"uris\\":[\\"http://zotero.org/users/226074/items/RNMTIJ22\\"],\\"uri\\":[\\"http://zotero.org/users/226074/items/RNMTIJ22\\"],\\"itemData\\":{\\"type\\":\\"article-journal\\",\\"title\\":\\"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY\\",\\"container-title\\":\\"Schizophrenia
      Research\\",\\"page\\":\\"91\\",\\"volume\\":\\"102\\",\\"issue\\":\\"1-3,
      Supplement 2\\",\\"source\\":\\"ScienceDirect\\",\\"URL\\":\\"http://www.sciencedirect.com/science/article/B6TC2-4SX99F1-9T/2/8e8f14104a436c0190375b6f604914e1\\",\\"DOI\\":\\"10.1016/S0920-9964(08)70275-5\\",\\"ISSN\\":\\"0920-9964\\",\\"note\\":\\"bibtex:
      ohrmann_functional_2008\\",\\"shortTitle\\":\\"FUNCTIONAL NEUROANATOMY
      OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS\\",\\"author\\":[{\\"family\\":\\"Ohrmann\\",\\"given\\":\\"Patricia\\"},{\\"family\\":\\"Wilmsmeier\\",\\"given\\":\\"Andreas\\"},{\\"family\\":\\"Bauer\\",\\"given\\":\\"Jochen\\"},{\\"family\\":\\"Siegmund\\",\\"given\\":\\"Ansgar\\"},{\\"family\\":\\"Suslow\\",\\"given\\":\\"Thomas\\"},{\\"family\\":\\"Wiedl\\",\\"given\\":\\"Karl\\"},{\\"family\\":\\"Koelkebeck\\",\\"given\\":\\"Katja\\"},{\\"family\\":\\"Kugel\\",\\"given\\":\\"Harald\\"},{\\"family\\":\\"Rothermundt\\",\\"given\\":\\"Matthias\\"},{\\"family\\":\\"Arolt\\",\\"given\\":\\"Volker\\"},{\\"family\\":\\"Pedersen\\",\\"given\\":\\"Anya\\"}],\\"issued\\":{\\"raw\\":\\"June
      2008\\"},\\"accessed\\":{\\"raw\\":\\"2010-04-23T18:51:58Z\\"}}}],\\"schema\\":\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\"}"
    </unfolded-io>

    <\unfolded-io|Scheme] >
      (json-string-\<gtr\>scm res11-fieldCode-csl-json-string)
    <|unfolded-io>
      <errput|{"citationID":"4TGwf4LB","properties":{"formattedCitation":"{\\\\rtf
      1 {\\\\scaps Paul W. Abrahams et al.}, {\\\\scaps TEX for the
      Impatient} (1990); Patricia Ohrmann et al., {\\\\i{}FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\scaps Schizophrenia
      Research} 91 (2008).}","plainCitation":"{\\\\rtf 1 {\\\\scaps Paul W.
      Abrahams et al.}, {\\\\scaps TEX for the Impatient} (1990); Patricia
      Ohrmann et al., {\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD
      SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI
      STUDY}, 102 {\\\\scaps Schizophrenia Research} 91
      (2008).}"},"citationItems":[{"id":2795,"uris":["http://zotero.org/users/226074/items/DJEFXEG9"],"uri":["http://zotero.org/users/226074/items/DJEFXEG9"],"itemData":{"type":"book","title":"TEX
      for the Impatient","publisher":"Addison-Wesley","volume":"1","ISBN":"978-0-201-51375-2","note":"TEX,
      a software system created by Donald E. Knuth, sets the standard for
      typesetting in mathematics, science, and engineering. Features:
      complete desciption of TEX commands, arranged for lookup either by
      function or alphabetically; clear definitions of essential TEX
      concepts, collected in separate chapter so that the command
      descriptions remain brief and accessible; explanations of common error
      messages and advice on solving problems that frequently arise; a
      collection of useful macros (also available in electronic form). \\n
      \\nbibtex[author_sort=Abrahams, Paul W. & Berry, Karl & Hargreaves,
      Kathryn A.;identifiers=amazon:0201513757,google:9wwZAQAAIAAJ,isbn:9780201513752;languages=eng;library_name=Calibre
      Library;rating=8;size=1421239 octets;tags=Computerized Typesetting,
      Mathematics Printing - Computer Programs, Mathematics Printing,
      Computer Programs, TeX (Computer File), TeX (Computer System),
      Computers, General, Digital Media, Desktop Publishing, Programming
      Languages, Design, Graphic Arts, Typography;title_sort=TEX for the
      Impatient] \\nbibtex: abrahams_tex_1990","author":[{"family":"Abrahams","given":"Paul
      W."},{"family":"Berry","given":"Karl"},{"family":"Hargreaves","given":"Kathryn
      A."}],"issued":{"raw":"February 1990"}}},{"id":455,"uris":["http://zotero.org/users/226074/items/RNMTIJ22"],"uri":["http://zotero.org/users/226074/items/RNMTIJ22"],"itemData":{"type":"article-journal","title":"FUNCTIONAL
      NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS: AN EVENT-RELATED 3T fMRI STUDY","container-title":"Schizophrenia
      Research","page":"91","volume":"102","issue":"1-3, Supplement
      2","source":"ScienceDirect","URL":"http://www.sciencedirect.com/science/article/B6TC2-4SX99F1-9T/2/8e8f14104a436c0190375b6f604914e1","DOI":"10.1016/S0920-9964(08)70275-5","ISSN":"0920-9964","note":"bibtex:
      ohrmann_functional_2008","shortTitle":"FUNCTIONAL NEUROANATOMY OF THE
      WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC
      PATIENTS","author":[{"family":"Ohrmann","given":"Patricia"},{"family":"Wilmsmeier","given":"Andreas"},{"family":"Bauer","given":"Jochen"},{"family":"Siegmund","given":"Ansgar"},{"family":"Suslow","given":"Thomas"},{"family":"Wiedl","given":"Karl"},{"family":"Koelkebeck","given":"Katja"},{"family":"Kugel","given":"Harald"},{"family":"Rothermundt","given":"Matthias"},{"family":"Arolt","given":"Volker"},{"family":"Pedersen","given":"Anya"}],"issued":{"raw":"June
      2008"},"accessed":{"raw":"2010-04-23T18:51:58Z"}}}],"schema":"https://github.com/citation-style-language/schema/raw/master/csl-citation.json"}>
    </unfolded-io>

    <\unfolded-io|Scheme] >
      #t
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      \;
    </input>

    \;
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