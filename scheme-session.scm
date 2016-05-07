(load-from-path "zotero.scm")

(define res1 (begin (zotero-write 0 "\"addCitation\"") (zotero-select-then-read)))

(define res1 '(1 37 "[\"Application_getActiveDocument\",[3]]"))

(define res2 (begin (zotero-write 1 (scm->json-string (list 3 1))) (zotero-select-then-read)))

(define res2 '(2 32 "[\"Document_getDocumentData\",[1]]"))

(define res3 (begin (zotero-write 2 (scm->json-string "")) (zotero-select-then-read)))

(define res3 '(3 1259 "[\"Document_setDocumentData\",[1,\"<data data-version=\\\"3\\\" zotero-version=\\\"4.0.29.8m73\\\"><session id=\\\"1ejMCIXf\\\"/><style id=\\\"http://juris-m.github.io/styles/jm-indigobook-in-text\\\" locale=\\\"en-US\\\" hasBibliography=\\\"1\\\" bibliographyStyleHasBeenSet=\\\"0\\\"/><prefs><pref name=\\\"citationTransliteration\\\" value=\\\"\\\"/><pref name=\\\"citationTranslation\\\" value=\\\"\\\"/><pref name=\\\"citationSort\\\" value=\\\"\\\"/><pref name=\\\"citationLangPrefsPersons\\\" value=\\\"orig\\\"/><pref name=\\\"citationLangPrefsInstitutions\\\" value=\\\"orig\\\"/><pref name=\\\"citationLangPrefsTitles\\\" value=\\\"orig\\\"/><pref name=\\\"citationLangPrefsJournals\\\" value=\\\"orig\\\"/><pref name=\\\"citationLangPrefsPublishers\\\" value=\\\"orig\\\"/><pref name=\\\"citationLangPrefsPlaces\\\" value=\\\"orig\\\"/><pref name=\\\"citationAffixes\\\" value=\\\"|||||||||||||||||||||||||||||||||||||||||||||||\\\"/><pref name=\\\"projectName\\\" value=\\\"\\\"/><pref name=\\\"extractingLibraryID\\\" value=\\\"0\\\"/><pref name=\\\"extractingLibraryName\\\" value=\\\"No group selected\\\"/><pref name=\\\"fieldType\\\" value=\\\"ReferenceMark\\\"/><pref name=\\\"storeReferences\\\" value=\\\"true\\\"/><pref name=\\\"automaticJournalAbbreviations\\\" value=\\\"true\\\"/><pref name=\\\"noteType\\\" value=\\\"0\\\"/><pref name=\\\"suppressTrailingPunctuation\\\" value=\\\"true\\\"/></prefs></data>\"]]"))

(define res4 (begin (zotero-write 3 (scm->json-string '())) (zotero-select-then-read)))

(define res4 '(4 47 "[\"Document_canInsertField\",[1,\"ReferenceMark\"]]"))

(define res5 (begin (zotero-write 4 (scm->json-string #t)) (zotero-select-then-read)))

(define res5 '(5 46 "[\"Document_cursorInField\",[1,\"ReferenceMark\"]]"))

(define res6 (begin (zotero-write 5 (scm->json-string (list 1 "" 0))) (zotero-select-then-read)))

(define res6 '(6 62 "[\"Document_displayAlert\",[1,\"Replace this Zotero field?\",0,1]]"))

(define res7 (begin (zotero-write 6 (scm->json-string 1)) (zotero-select-then-read)))

(define res7 '(7 30 "[\"Field_setCode\",[1,1,\"TEMP\"]]"))

(define res8 (begin (zotero-write 7 (scm->json-string '())) (zotero-select-then-read)))

(define res8 '(8 42 "[\"Document_getFields\",[1,\"ReferenceMark\"]]"))

(define res9 (begin (zotero-write 8 (scm->json-string (list (list 1) (list "TEMP") (list 0)))) (zotero-select-then-read)))

(define res9 '(9 313 "[\"Field_setText\",[1,1,\"{\\\\rtf 1 {\\\\scaps Paul W. Abrahams et al.}, {\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann et al., {\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\scaps Schizophrenia Research} 91 (2008).}\",true]]"))

(define res10 (begin (zotero-write 9 (scm->json-string '())) (zotero-select-then-read)))

(define res10 '(10 23 "[\"Field_getText\",[1,1]]"))

(define res11 (begin (zotero-write 10 (scm->json-string (caddr (cadr (json-string->scm (caddr res9)))))) (zotero-select-then-read)))

(define res11 '(11 3795 "[\"Field_setCode\",[1,1,\"ITEM CSL_CITATION {\\\"citationID\\\":\\\"4TGwf4LB\\\",\\\"properties\\\":{\\\"formattedCitation\\\":\\\"{\\\\\\\\rtf 1 {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps Schizophrenia Research} 91 (2008).}\\\",\\\"plainCitation\\\":\\\"{\\\\\\\\rtf 1 {\\\\\\\\scaps Paul W. Abrahams et al.}, {\\\\\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann et al., {\\\\\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\\\\\scaps Schizophrenia Research} 91 (2008).}\\\"},\\\"citationItems\\\":[{\\\"id\\\":2795,\\\"uris\\\":[\\\"http://zotero.org/users/226074/items/DJEFXEG9\\\"],\\\"uri\\\":[\\\"http://zotero.org/users/226074/items/DJEFXEG9\\\"],\\\"itemData\\\":{\\\"type\\\":\\\"book\\\",\\\"title\\\":\\\"TEX for the Impatient\\\",\\\"publisher\\\":\\\"Addison-Wesley\\\",\\\"volume\\\":\\\"1\\\",\\\"ISBN\\\":\\\"978-0-201-51375-2\\\",\\\"note\\\":\\\"TEX, a software system created by Donald E. Knuth, sets the standard for typesetting in mathematics, science, and engineering. Features: complete desciption of TEX commands, arranged for lookup either by function or alphabetically; clear definitions of essential TEX concepts, collected in separate chapter so that the command descriptions remain brief and accessible; explanations of common error messages and advice on solving problems that frequently arise; a collection of useful macros (also available in electronic form). \\\\n \\\\nbibtex[author_sort=Abrahams, Paul W. & Berry, Karl & Hargreaves, Kathryn A.;identifiers=amazon:0201513757,google:9wwZAQAAIAAJ,isbn:9780201513752;languages=eng;library_name=Calibre Library;rating=8;size=1421239 octets;tags=Computerized Typesetting, Mathematics Printing - Computer Programs, Mathematics Printing, Computer Programs, TeX (Computer File), TeX (Computer System), Computers, General, Digital Media, Desktop Publishing, Programming Languages, Design, Graphic Arts, Typography;title_sort=TEX for the Impatient] \\\\nbibtex: abrahams_tex_1990\\\",\\\"author\\\":[{\\\"family\\\":\\\"Abrahams\\\",\\\"given\\\":\\\"Paul W.\\\"},{\\\"family\\\":\\\"Berry\\\",\\\"given\\\":\\\"Karl\\\"},{\\\"family\\\":\\\"Hargreaves\\\",\\\"given\\\":\\\"Kathryn A.\\\"}],\\\"issued\\\":{\\\"raw\\\":\\\"February 1990\\\"}}},{\\\"id\\\":455,\\\"uris\\\":[\\\"http://zotero.org/users/226074/items/RNMTIJ22\\\"],\\\"uri\\\":[\\\"http://zotero.org/users/226074/items/RNMTIJ22\\\"],\\\"itemData\\\":{\\\"type\\\":\\\"article-journal\\\",\\\"title\\\":\\\"FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI STUDY\\\",\\\"container-title\\\":\\\"Schizophrenia Research\\\",\\\"page\\\":\\\"91\\\",\\\"volume\\\":\\\"102\\\",\\\"issue\\\":\\\"1-3, Supplement 2\\\",\\\"source\\\":\\\"ScienceDirect\\\",\\\"URL\\\":\\\"http://www.sciencedirect.com/science/article/B6TC2-4SX99F1-9T/2/8e8f14104a436c0190375b6f604914e1\\\",\\\"DOI\\\":\\\"10.1016/S0920-9964(08)70275-5\\\",\\\"ISSN\\\":\\\"0920-9964\\\",\\\"note\\\":\\\"bibtex: ohrmann_functional_2008\\\",\\\"shortTitle\\\":\\\"FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS\\\",\\\"author\\\":[{\\\"family\\\":\\\"Ohrmann\\\",\\\"given\\\":\\\"Patricia\\\"},{\\\"family\\\":\\\"Wilmsmeier\\\",\\\"given\\\":\\\"Andreas\\\"},{\\\"family\\\":\\\"Bauer\\\",\\\"given\\\":\\\"Jochen\\\"},{\\\"family\\\":\\\"Siegmund\\\",\\\"given\\\":\\\"Ansgar\\\"},{\\\"family\\\":\\\"Suslow\\\",\\\"given\\\":\\\"Thomas\\\"},{\\\"family\\\":\\\"Wiedl\\\",\\\"given\\\":\\\"Karl\\\"},{\\\"family\\\":\\\"Koelkebeck\\\",\\\"given\\\":\\\"Katja\\\"},{\\\"family\\\":\\\"Kugel\\\",\\\"given\\\":\\\"Harald\\\"},{\\\"family\\\":\\\"Rothermundt\\\",\\\"given\\\":\\\"Matthias\\\"},{\\\"family\\\":\\\"Arolt\\\",\\\"given\\\":\\\"Volker\\\"},{\\\"family\\\":\\\"Pedersen\\\",\\\"given\\\":\\\"Anya\\\"}],\\\"issued\\\":{\\\"raw\\\":\\\"June 2008\\\"},\\\"accessed\\\":{\\\"raw\\\":\\\"2010-04-23T18:51:58Z\\\"}}}],\\\"schema\\\":\\\"https://github.com/citation-style-language/schema/raw/master/csl-citation.json\\\"}\"]]"))

(define res12 (begin (zotero-write 11 (scm->json-string '())) (zotero-select-then-read)))

(define res12 '(12 313 "[\"Field_setText\",[1,1,\"{\\\\rtf 1 {\\\\scaps Paul W. Abrahams et al.}, {\\\\scaps TEX for the Impatient} (1990); Patricia Ohrmann et al., {\\\\i{}FUNCTIONAL NEUROANATOMY OF THE WISCONSIN CARD SORTING TEST IN SCHIZOPHRENIC PATIENTS: AN EVENT-RELATED 3T fMRI STUDY}, 102 {\\\\scaps Schizophrenia Research} 91 (2008).}\",true]]"))

(define res13 (begin (zotero-write 12 (scm->json-string '())) (zotero-select-then-read)))

(define res13 '(13 25 "[\"Document_activate\",[1]]"))

(define res14 (begin (zotero-write 13 (scm->json-string '())) (zotero-select-then-read)))

(define res14 '(14 25 "[\"Document_complete\",[1]]"))

(define res15 (begin (zotero-write 14 (scm->json-string '())) (zotero-select-then-read)))

(define res15 '(0 0 ""))





(define res11-fieldCode (caddar (cdr (json-string->scm (caddr res11)))))

(define res11-fieldCode-csl-json-string (substring res11-fieldCode 18 (string-length res11-fieldCode)))



res1

res2

res3

res4

res5

res6

res7

res8

res9

res10

res11

res12

res13

res14

res15

res11-fieldCode

res11-fieldCode-csl-json-string