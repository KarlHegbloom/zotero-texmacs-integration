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

  \;

  <assign|ztDebug|<macro|body|<extern|(lambda (body) (zt-format-debug
  "Debug:ztDebug: ~s\\n" body))|<arg|body>>>>

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
      Ditto, but for things that depend on not being inside of a
      zbibliography.
    </src-comment>
  </active*>

  <assign|zt-not-inside-zbibliography|true>

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
      Links are wrapped in this macro so that they can be rendered
      differently depending on whether they are in-text, in footnote or
      endnote, or in a bibliography. I could not simply redefine hlink the
      way I did with footnote, since it is a primitive defined in C++.
    </src-comment>
  </active*>

  \;

  <assign|ztHref|<macro|url|display|<if|<not|<and|<value|zt-not-inside-note>|<value|zt-not-inside-zbibliography>>>|<hlink|<arg|display>|<arg|url>>|<hlink|URL|<arg|url>><space|0.2spc><rsup|(><if|<value|zotero-pref-noteType2>|<zt-endnote|<hlink|<arg|display>|<arg|url>>>|<zt-footnote|<hlink|<arg|display>|<arg|url>>>><rsup|)>>>>

  <drd-props|ztHref|accessible|all|enable-writability|all|border|yes>

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

  <\active*>
    <\src-comment>
      When <with|font-family|tt|development_extensions.csl_reverse_lookup_support
      = true;> in citeproc.js, it can print out a bunch of information that I
      couldn't help but experimentally enable once just to see what it does.
      I'm not convinced that I want to use it for anything, but just in case,
      it's still possible as long as these macros are defined and are output
      by the bbl outputFormat in that case.

      \\ztShowID{\\ztcslidNode{#{state.opt.nodenames[cslid]}}\\ztcslid{#{cslid}}#{str}}}
    </src-comment>
  </active*>

  <assign|ztShowID|<macro|node|cslid|body|<extern|(lambda (node cslid body)
  (zt-ext-ztShowID id))|<arg|node>|<arg|cslid>|<arg|body>>>>

  <\active*>
    <\src-comment>
      These are used below to simplify the expressions inside the macros, to
      make them easier to read.
    </src-comment>
  </active*>

  <assign|ztRigidHspace|<macro|len|<hspace|<arg|len>|<arg|len>|<arg|len>>>>

  <assign|ztRawWidth|<macro|body|<look-up|<box-info|<arg|body>|w>|0>>>

  <assign|ztAsTmlen|<macro|rawWidth|<times|1tmpt|<arg|rawWidth>>>>

  <\active*>
    <\src-comment>
      The itemID here is the same as the itemID in the zfield-Code JSON when
      "Store References in Document" is enabled via the Document Preferences
      dialogue. This will be useful for hyperlinking, I think. That will
      require scheme code that has access to the information parsed from the
      JSON.
    </src-comment>
  </active*>

  <assign|zbibCitationItemID|<macro|itemID|<extern|(lambda (itemID)
  (zt-ext-zbibCitationItemID itemID))|<arg|itemID>>>>

  <\active*>
    <\src-comment>
      The indent will be the same as that set by the firstLineIndent and
      bodyIndent.
    </src-comment>
  </active*>

  <assign|AAAzt-left-margin-extra-indent|0tmpt>

  <assign|AAAztNewBlock|<macro|body|<arg|body><next-line><assign|zt-left-margin-extra-indent|1tab>>>

  <assign|ztNewBlock|<macro|body|<arg|body><next-line>>>

  <assign|ztbibIndent|<macro|body|<arg|body>>>

  <assign|AAAztLeftMargin|<macro|body|<ztRigidHspace|<value|zt-left-margin-extra-indent>><arg|body><with|tab-stop|<if|<greatereq|<get-arity|<value|zotero-BibliographyStyle_arrayList>>|1>|<look-up|<value|zotero-BibliographyStyle_arrayList>|0>|<value|zotero-BibliographyStyle_bodyIndent>>|<ztRigidHspace|<if|<greater|<ztRawWidth|<ztRigidHspace|<value|tab-stop>>>|0>|<ztAsTmlen|<minimum|<minus|<ztRawWidth|<ztRigidHspace|<value|tab-stop>>>|<ztRawWidth|<arg|body>>>|<ztRawWidth|<ztRigidHspace|<value|item-hsep>>>>>|<value|item-hsep>>>>>>

  <assign|ztLeftMargin|<macro|body|<arg|body><with|tab-stop|<if|<greatereq|<get-arity|<value|zotero-BibliographyStyle_arrayList>>|1>|<look-up|<value|zotero-BibliographyStyle_arrayList>|0>|<value|zotero-BibliographyStyle_bodyIndent>>|<ztRigidHspace|<if|<greater|<ztRawWidth|<ztRigidHspace|<value|tab-stop>>>|0>|<ztAsTmlen|<minimum|<minus|<ztRawWidth|<ztRigidHspace|<value|tab-stop>>>|<ztRawWidth|<arg|body>>>|<ztRawWidth|<ztRigidHspace|<value|item-hsep>>>>>|<value|item-hsep>>>>>>

  <assign|ztRightInline|<value|identity>>

  <assign|ztbibItemText|<\macro|body>
    <\with|par-sep|<times|<value|par-sep>|<value|zotero-BibliographyStyle_lineSpacing>>|ztbibItem-vsep|<times|<value|ztbibItem-vsep>|<value|zotero-BibliographyStyle_entrySpacing>>>
      <\surround|<vspace*|<value|item-vsep>>|<right-flush>>
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

  <assign|bibitem|<macro|key|<extern|(lambda (key) (zt-ext-bibitem
  key))|<arg|key>>>>

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

  <assign|zt-option-zbib-font-size|0.84>

  <assign|zbibliography|<\macro|fieldID|fieldCode|fieldText>
    <\surround|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|0>|<right-flush>>
      <principal-section*|<bibliography-text>>

      <with|font-size|<value|zt-option-zbib-font-size>|par-left|0tab|par-first|0tab|par-no-first|true|zt-not-inside-zbibliography|false|<arg|fieldText>>
    </surround>
  </macro>>

  <drd-props|zbibliography|disable-writability|0|unaccessible|0|disable-writability|1|unaccessible|1|enable-writability|2|accessible|2>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>