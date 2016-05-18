<TeXmacs|1.99.4>

<style|<tuple|article|tm-zotero>>

<\body>
  <\session|scheme|default>
    <\input|Scheme] >
      (load-from-path "zotero.scm")
    </input>

    <\unfolded-io|Scheme] >
      (zotero-init-env-zotero-prefs)
    <|unfolded-io>
      #t
    </unfolded-io>

    <\input|Scheme] >
      (tree-go-to (zotero-find-zcite "id02") 1)
    </input>

    <\input|Scheme] >
      \;
    </input>
  </session>

  <zcite|id01|CODE|rawext|Formatted Text> and then some text and another
  citation here<zcite|id02|CODE|rawText|Very nicely Formatted Text id02> And
  another. <hlink|Wikipedia|https://en.wikipedia.org/>

  <\footnote>
    This is a normal looking footnote with a citation inside of
    it,<zcite|id03|CODE|rawText|Formatted text id03.> and a link:
    <href|https://en.wikipedia.org>
  </footnote> <href|https://en.wikipedia.org>
</body>

<\initial>
  <\collection>
    <associate|page-type|letter>
    <associate|tm-zotero:hlinks-as-footnotes|on>
    <associate|tm-zotero:hlinks-as-smaller|on>
    <associate|tm-zotero:hlinks-as-tt|on>
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
    <associate|zotero-session-id|H9cHGBln>
    <associate|zotero-style-bibliographyStyleHasBeenSet|0>
    <associate|zotero-style-hasBibliography|1>
    <associate|zotero-style-id|http://juris-m.github.io/styles/jm-indigobook-law-review>
    <associate|zotero-style-locale|en-US>
    <associate|zoteroDocumentData|\<data data-version="3"
    zotero-version="4.0.29.9m75"\>\<session id="H9cHGBln"/\>\<style
    id="http://juris-m.github.io/styles/jm-indigobook-law-review"
    locale="en-US" hasBibliography="1" bibliographyStyleHasBeenSet="0"/\>\<prefs\>\<pref
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
    name="noteType" value="1"/\>\<pref name="suppressTrailingPunctuation"
    value="true"/\>\</prefs\>\</data\>>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|footnote-1|<tuple|1|?>>
    <associate|footnote-2|<tuple|2|?>>
    <associate|footnote-3|<tuple|3|?>>
    <associate|footnote-4|<tuple|4|?>>
    <associate|footnote-5|<tuple|5|?>>
    <associate|footnr-1|<tuple|1|?>>
    <associate|footnr-2|<tuple|2|?>>
    <associate|footnr-3|<tuple|3|?>>
    <associate|footnr-4|<tuple|4|?>>
    <associate|footnr-5|<tuple|5|?>>
    <associate|zotero-id01-noteIndex|<tuple|1|?>>
    <associate|zotero-id02-noteIndex|<tuple|2|?>>
    <associate|zotero-id03-noteIndex|<tuple|0|?>>
  </collection>
</references>