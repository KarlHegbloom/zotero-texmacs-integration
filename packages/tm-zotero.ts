<TeXmacs|1.99.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tm-zotero|0.007-UT-1-W>

    <\src-purpose>
      This package contains extended macros for citations and provides a
      <TeXmacs> integration with the Juris-M or Zotero reference manager for
      Firefox. It utilizes the same wire-protocolinterface that is used by
      the Zotero \<rightarrow\> OpenOffice.org integration;<compound|math|>
      presently it only works with a patch applied to Juris-M/Zotero to allow
      switching the output format, and with Better BibTeX for Zotero that is
      extended to create a ".bbl" output formatter. (vs HTML or RTF).
    </src-purpose>

    <src-copyright|2016|Karl Martin Hegbloom>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(zotero)>

  <use-package|std-counter|std-utils|env-float|std-list>

  <\active*>
    <\src-comment>
      Default values to avoid transcient "bad case" errors prior to setting
      documentData.
    </src-comment>
  </active*>

  <assign|zotero-pref-noteType0|true>

  <assign|zotero-pref-noteType1|false>

  <assign|zotero-pref-noteType2|false>

  <\active*>
    <\src-comment>
      <with|color|red|Todo:> What should happen when the citation style is
      set to noteType2 (endnote) and the writer wants to make a note? Should
      there still be footnotes, as well as endnotes? Or should there be a
      general purpose macro that switches depending on which style has been
      selected, so that changing styles automatically moves the manually
      created notes between being footnotes or endnotes? Will anyone really
      use it?
    </src-comment>
  </active*>

  <\active*>
    <\src-comment>
      Flag to prevent the attempt to create notes inside of notes problem. By
      default, we are not inside of a footnote or endnote.
    </src-comment>
  </active*>

  <assign|zt-not-inside-note|true>

  <assign|zt-in-footnote|false>

  <assign|zt-in-endnote|false>

  <\active*>
    <\src-comment>
      Per zcite option, to force an in-text citation when using a CSL "note"
      style.
    </src-comment>
  </active*>

  <assign|zt-option-this-zcite-in-text|false>

  <\active*>
    <\src-comment>
      Setup for special handling of in-text citations inside of footnotes and
      endnotes; And for hlinks with extra href's displayed as footnotes, and
      hrefs that display as footnotes rather than in-text.
    </src-comment>
  </active*>

  <assign|zt-orig-footnote|<value|footnote>>

  <assign|zt-footnote|<macro|body|<style-with|src-compact|none|<next-footnote><with|zt-not-inside-note|false|zt-in-footnote|true|<render-footnote|<the-footnote>|<arg|body>>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<with|font-shape|right|<reference|<merge|footnote-|<the-footnote>>>>>>>>

  <assign|footnote|<value|zt-footnote>>

  <\active*>
    <\src-comment>
      End-notes <with|color|red|ARE NOT WORKING.> I do not know how to do
      this without it storing typesetter-expanded things into the endnote
      attachment aux... quote / eval ?
    </src-comment>
  </active*>

  <new-counter|endnote>

  <assign|endnote-sep|<footnote-sep>>

  <assign|endnote-item-render|<value|aligned-space-item>>

  <assign|endnote-item-transform|<value|identity>>

  <new-list|endnote-list|<value|endnote-item-render>|<value|endnote-item-transform>>

  <assign|the-endnotes|<macro|<endnote-list*|<get-attachment|endnotes>>>>

  <assign|render-endnote*|<\macro|sym|nr|body>
    <write|endnotes|<style-with|src-compact|all|<with|par-mode|justify|par-left|0cm|par-right|0cm|font-shape|right|<style-with|src-compact|none|<surround|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<merge|#endnr-|<arg|nr>>>>|<item*|<arg|sym>>><endnote-sep>|<set-binding|<merge|endnote-|<arg|nr>>|<value|the-label>|body><right-flush>|<style-with|src-compact|none|<arg|body>>>>>>>
  </macro>>

  <assign|render-endnote|<macro|nr|body|<render-endnote*|<arg|nr>|<arg|nr>|<arg|body>>>>

  <assign|zt-endnote|<macro|body|<style-with|src-compact|none|<next-endnote><with|zt-not-inside-note|false|zt-in-endnote|true|<render-endnote|<the-endnote>|<arg|body>>><space|0spc><label|<merge|endnr-|<the-endnote>>><rsup|<with|font-shape|right|<reference|<merge|endnote-|<the-endnote>>>>>>>>

  <assign|zt-endnote|<with|color|red|ENDNOTES NOT IMPLEMENTED.> See:
  tm-zotero.ts>

  <\active*>
    <\src-comment>
      Citation display depending on CSL noteType: 0
      \<rightarrow\><compound|text|<compound|math|>><compound|math|><compound|math|>
      in-text, 1 \<rightarrow\> footnote, 2 \<rightarrow\> end-note, plus
      override per zcite.
    </src-comment>
  </active*>

  <assign|zt-flag-modified|<macro|fieldID|<extern|(lambda (id)
  (zt-ext-flag-if-modified id))|<arg|fieldID>>>>

  <assign|zt-zcite-in-text|<macro|fieldID|citebody|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<case|<value|zt-not-inside-note>|0|<value|zt-in-footnote>|<value|footnote-nr>|<value|zt-in-endnote>|<value|endnote-nr>>><zt-flag-modified|<arg|fieldID>><arg|citebody>>>

  <assign|zt-zcite-as-footnote|<macro|fieldID|citebody|<zt-footnote|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<value|footnote-nr>><zt-flag-modified|<arg|fieldID>><arg|citebody>>>>

  <assign|zt-zcite-as-endnote|<macro|fieldID|citebody|<zt-endnote|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<value|endnote-nr>><zt-flag-modified|<arg|fieldID>><arg|citebody>>>>

  <assign|render-zcite|<macro|fieldID|citebody|<case|<or|<value|zotero-pref-noteType0>|<value|zt-option-this-zcite-in-text>>|<zt-zcite-in-text|<arg|fieldID>|<arg|citebody>>|<and|<value|zotero-pref-noteType1>|<value|zt-not-inside-note>>|<zt-zcite-as-footnote|<arg|fieldID>|<arg|citebody>>|<and|<value|zotero-pref-noteType2>|<value|zt-not-inside-note>>|<zt-zcite-as-endnote|<arg|fieldID>|<arg|citebody>>|<zt-zcite-in-text|<arg|fieldID>|<arg|citebody>>>>>

  \;

  <assign|ztcslidNode|<macro|nodename|<extern|(lambda (nodename)
  (zt-ext-ztcslidNode nodename))|<arg|nodename>>>>

  <assign|ztcslid|<macro|cslid|<extern|(lambda (cslid) (zt-ext-ztcslid
  cslid))|<arg|cslid>>>>

  <assign|zbibCitationItemID|<macro|itemID|<extern|(lambda (itemID)
  (zt-ext-zbibCitationItemID itemID))|<arg|itemID>>>>

  <assign|ztShowID|<macro|id|<extern|(lambda (id) (zt-ext-ztShowID
  id))|<arg|id>>>>

  \;

  <assign|ztbibItemIndentTabN|1>

  <assign|ztbibIndent|<macro|body|<extern|(lambda (body) (zt-ext-ztbibIndent
  body))|<arg|body>>>>

  <assign|ztLeftMargin|<macro|body|<arg|body><hspace|<minimum|<minus|<value|item-hsep>|1.0spc>|1.0spc>>>>

  <assign|XztLeftMargin|<macro|body|<set-binding|<value|the-label>|<arg|body>>>>

  <assign|XXztLeftMargin|<value|identity>>

  <assign|ztRightInline|<value|identity>>

  <assign|ztbibItem-vsep|<macro|<value|par-sep>>>

  <assign|ztbibItemText|<\macro|body>
    <\with|par-sep|<times|<value|par-sep>|<value|zotero-BibliographyStyle_lineSpacing>>|ztbibItem-vsep|<times|<value|ztbibItem-vsep>|<value|zotero-BibliographyStyle_itemSpacing>>>
      <\surround|<vspace*|<value|ztbibItem-vsep>>|<right-flush>>
        <\with|par-no-first|false|par-first|<value|zotero-BibliographyStyle_firstLineIndent>|par-left|<value|zotero-BibliographyStyle_bodyIndent>>
          <arg|body>
        </with>
      </surround>
    </with>
  </macro>>

  <\active*>
    <\src-comment>
      Fix-ups for default macros for displaying the bib-list.
      <with|color|red|This is a work in progress and not final.>

      This is so that the same bbl outputFormat from Better BibTeX for Zotero
      can be used for LaTeX and for TeXmacs.
    </src-comment>
  </active*>

  \;

  <assign|thebibliography|<macro|keywidth|body|<arg|body>>>

  <assign|bibitem|<macro|key|<assign|ztbibItemIndentTabN|1>>>

  <\active*>
    <\src-comment>
      Juris-M / Zotero Citations and Bibliography. Both the zcite and
      zbibliography macros must have the same arity, semantics, and order of
      arguments because Zotero treats them generically as "fields".

      <with|color|red|This is a work in progress. Note: take care when
      setting drd-props so that the cursor can be inside of the light-blue
      box so that the "editCitation" thing will work properly.>

      The use of `surround' in the zbibliography forces it to be typeset in
      block context. Without that, the lines don't wrap properly and run off
      the right edge of the page. The zcite on the other hand must be in line
      context, because if it's block context, you can't put a citation
      in-text without it forcing itself to be on it's own line. When I was
      trying to use a converter from rtf to TeXmacs, they kept coming out as
      blocks rather than in-line.
    </src-comment>
  </active*>

  <assign|zcite|<macro|fieldID|fieldCode|fieldText|<render-zcite|<arg|fieldID>|<arg|fieldText>>>>

  <drd-props|zcite|disable-writability|0|unaccessible|0|disable-writability|1|unaccessible|1|enable-writability|2|accessible|2>

  <assign|zbibliography|<\macro|fieldID|fieldCode|fieldText>
    <\surround|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|0>|<right-flush>>
      <principal-section*|<bibliography-text>>

      <with|font-size|0.84|par-left|0tab|par-first|0tab|par-no-first|true|<arg|fieldText>>
    </surround>
  </macro>>

  <drd-props|zbibliography|disable-writability|0|unaccessible|0|disable-writability|1|unaccessible|1|enable-writability|2|accessible|2>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>