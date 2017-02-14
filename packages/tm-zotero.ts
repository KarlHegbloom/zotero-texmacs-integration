<TeXmacs|1.99.9>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tm-zotero|0.007-UT-1-W-SOURCE>

    <\src-purpose>
      This package contains extended macros for citations and provides a
      <TeXmacs> integration with the Juris-M or Zotero reference manager for
      Firefox.

      \;

      It utilizes the same wire-protocol interface that is used by the Zotero
      \<rightarrow\> OpenOffice.org integration;<compound|math|>

      \;

      That works by applying a monkey-patch to Juris-M / Zotero that adds a
      new outputFormat, which is roughly based on BibTeX's 'bbl' format, and
      then switches the integration to use the new outputFormat.

      \;

      Thus while it's in use, the ordinary LibreOffice integration won't
      work. In order to use Juris-M or Zotero with LibreOffice again, you
      must uninstall or disable the Firefox propachi-texmacs addon.
    </src-purpose>

    <src-copyright|2016, 2017,|Karl Martin Hegbloom>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <\active*>
    <\src-comment>
      <with|color|red|I realize that this is very disorganized! So: To do:
      Organize this style sheet source better.><with|font-series|bold|>

      To do: Clean out XXX'd out ones.

      To do: Simplify

      To do: uniform naming convention\ 
    </src-comment>
  </active*>

  <use-module|(tm-zotero)>

  <use-package|std-counter|std-utils|env-float|std-list|std-markup>

  <\active*>
    <\src-comment>
      These are enclosed by macros so that there is a UTF-8 encoded font
      being used for the paragraphsign and sectionsign so that when the PDF
      is generated, the outline will have the right symbol showing. When a
      cork encoded font is the document font, they don't show up right
      otherwise.

      I recommend using one of the new True Type TeX Gyre fonts, such as
      Bonum, Pagella, Schola, or Termes.
    </src-comment>
  </active*>

  <assign|ParagraphSignGlyph|<macro|\<paragraph\>>>

  <assign|SectionSignGlyph|<macro|Ÿ>>

  <assign|ldquo|\P>

  <assign|rdquo|\Q>

  <assign|ztlt|\<less\>>

  <assign|ztgt|\<gtr\>>

  <assign|less-than-sign|\<less\>>

  <assign|greater-than-sign|\<gtr\>>

  \;

  <\active*>
    <\src-comment>
      Because the underlying zt-format-debug function prints timing
      information, this can be used to benchmark how long it takes to typeset
      a document by putting one of these at the top, and another at the
      bottom.

      TODO extend and improve this debug/scaffold format to stderr mechanism,
      integrating it with the normal TeXmacs debugging console thing...
    </src-comment>
  </active*>

  <assign|ztDebug|<macro|body|<extern|(lambda (body-t) (zt-format-debug
  "Debug:ztDebug: ~s\\n" (tree-\<gtr\>stree body-t)))|<arg|body>>>>

  \;

  <assign|XXXusepackage*|<macro|ign1|ign2|<concealed|<arg|ign1><arg|ign2>>>>

  <\active*>
    <\src-comment>
      In order to prevent the latex to texmacs conversion from mangling
      these, I had to prefix them with zt to get it past the substitution
      phase of the converter. Also, zttexttt is used by the exception / error
      dialog formatting.
    </src-comment>
  </active*>

  <assign|zttextit|<macro|body|<with|font-shape|italic|<arg|body>>>>

  <assign|zttextsl|<macro|body|<with|font-shape|slanted|<arg|body>>>>

  <assign|zttextup|<macro|body|<with|font-shape|right|<arg|body>>>>

  <assign|zttextsc|<macro|body|<with|font-shape|small-caps|<arg|body>>>>

  <assign|zttexttt|<macro|body|<with|font-family|tt|<arg|body>>>>

  <assign|zttextnormal|<macro|body|<with|font-family|rm|font-shape|right|font-series|medium|<arg|body>>>>

  <assign|zttextbf|<macro|body|<with|font-series|bold|<arg|body>>>>

  <assign|zttextmd|<macro|body|<with|font-series|medium|<arg|body>>>>

  <\active*>
    <\src-comment>
      Default values to avoid transcient "bad case" errors being thrown
      (regarding a \\case macro) prior to setting documentData.
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

  \;

  <assign|zt-footnote|<macro|body|<style-with|src-compact|none|<next-footnote><with|zt-not-inside-note|false|zt-in-footnote|true|<render-footnote|<the-footnote>|<arg|body>>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<with|font-shape|right|<reference|<merge|footnote-|<the-footnote>>>>>>>>

  \;

  <assign|footnote|<value|zt-footnote>>

  <\active*>
    <\src-comment>
      End-notes <with|color|red|ARE NOT WORKING.> I do not know how to do
      this without it storing typesetter-expanded things into the endnote
      attachment aux... quote / eval ?

      I think that end-notes are sort of a kind of bibliography... I wonder
      if TeXmacs ought to just have an end-notes thing, that can put end
      notes at the end of sections, chapters, or the entire document or
      whatever. I think they should be gathered by something, and then all
      displayed when called for by the insertion of an environment where they
      get displayed, just like a bibliography or table of contents.

      Remember that storing them in reference bindings does not work right.
      Instead, see the way that the bibtex information gets cached inside
      documents in the default TeXmacs bibliography generation system.
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

  <assign|ztHref|<macro|url|display|<if|<and|<value|zt-not-inside-note>|<value|zt-not-inside-zbibliography>>|<hlink|URL|<arg|url>><space|0.2spc><rsup|(><if|<value|zotero-pref-noteType2>|<zt-endnote|<small|<hlink|<arg|display>|<arg|url>>>>|<zt-footnote|<small|<hlink|<arg|display>|<arg|url>>>>><rsup|)>|<small|<hlink|<arg|display>|<arg|url>>>>>>

  <drd-props|ztHref|accessible|all|enable-writability|all|border|yes>

  <\active*>
    <\src-comment>
      hashLabel is not used by ztHrefFromBibToURL but available to it or to
      code acting on it. I think I put it in there mainly so that
      ztHrefFromCiteToBib and ztHrefFromBibToURL will have the same arity and
      layout. It may go away before this hits beta.

      zt-zfieldID is defined here, and the string +DISACTIVATED is a flag
      value that must not be changed here because it is relied upon by
      tm-zotero.scm. It's value will be locally with-bound inside of the
      zcite macro so that from within that dynamic scope, zt-zfieldID will
      hold the expected string value.
    </src-comment>
  </active*>

  <assign|zt-zfieldID|+DISACTIVATED>

  \;

  <assign|zt-link-BibToURL|true>

  <assign|ztDefaultCiteURL|>

  <assign|zt-link-FromCiteToBib|true>

  \;

  <assign|create-unique-id|<macro|<extern|tm-zotero-ext:create-unique-id>>>

  \;

  <assign|ztHrefFromBibToURL|<macro|hashLabel|URL|display|<with|link-BibToURL|<value|zt-link-BibToURL>|unique-id|<create-unique-id>|<if|<value|link-BibToURL>|<locus|<id|<value|unique-id>>|<link|hyperlink|<id|<value|unique-id>>|<url|<arg|URL>>>|<arg|display>>|<arg|display>>>>>

  \;

  <assign|XXztHrefFromBibToURL|<macro|hashLabel|URI|display|<with|link-BibToURL|<value|zt-link-BibToURL>|<if|<value|link-BibToURL>|<hlink|<arg|display>|<arg|URI>>|<arg|display>>>>>

  <assign|ztHrefFromBibToURL*|<value|ztHrefFromBibToURL>>

  \;

  <assign|tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!|<macro|zfieldID-t|hashLabel-t|<extern|tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!|<arg|zfieldID-t>|<arg|hashLabel-t>>>>

  <assign|ztHrefFromCiteToBib|<macro|hashLabel|URL|display|<label|<merge|zciteID|<value|zt-zfieldID>|<arg|hashLabel>>><tm-zotero-ext:ensure-ztHrefFromCiteToBib-interned!|<value|zt-zfieldID>|<arg|hashLabel>><with|link-FromCiteToBib|<value|zt-link-FromCiteToBib>|link-BibToURL|<value|zt-link-BibToURL>|unique-id|<create-unique-id>|<case|<and|<value|link-FromCiteToBib>|<has-zbibliography?>>|<locus|<id|<value|unique-id>>|<link|hyperlink|<id|<value|unique-id>>|<url|<arg|hashLabel>>>|<arg|display>>|<and|<value|link-FromCiteToBib>|<value|link-BibToURL>>|<locus|<id|<value|unique-id>>|<link|<id|<value|unique-id>>|<url|<arg|URL>>>|<arg|display>>|<arg|display>>>>>

  <assign|ztHrefFromCiteToBib*|<value|ztHrefFromCiteToBib>>

  <\active*>
    <\src-comment>
      Citation display depending on CSL noteType: 0
      \<rightarrow\><compound|text|<compound|math|>><compound|math|><compound|math|>
      in-text, 1 \<rightarrow\> footnote, 2 \<rightarrow\> end-note, plus
      override per zcite.
    </src-comment>
  </active*>

  <assign|zt-zcite-in-text|<macro|fieldID|citebody|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<case|<value|zt-not-inside-note>|0|<value|zt-in-footnote>|<value|footnote-nr>|<value|zt-in-endnote>|<value|endnote-nr>>><arg|citebody>>>

  <assign|zt-zcite-as-footnote|<macro|fieldID|citebody|<zt-footnote|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<value|footnote-nr>><arg|citebody>>>>

  <assign|zt-zcite-as-endnote|<macro|fieldID|citebody|<zt-endnote|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<value|endnote-nr>><arg|citebody>>>>

  \;

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
  (tm-zotero-ext:ztShowID node cslid body))|<arg|node>|<arg|cslid>|<arg|body>>>>

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
  (tm-zotero-ext:zbibCitationItemID itemID))|<arg|itemID>>>>

  <\active*>
    <\src-comment>
      The indent will be the same as that set by the firstLineIndent and
      bodyIndent.

      Look at std-utils.ts for the indentation macros and see if they can be
      used to improve this sometime when I'm not about to run out of
      batteries.

      TODO The amount of space after the label in ztLeftMargin ought to be
      such that the text following it lines up exactly with the rest of the
      bibliography entry, that is, at zotero-BibliographyStyle_bodyIndent...
      but whenever the width of a label is such that it's first line is
      pushed over to the right, then perhaps the bodyIndent ought to increase
      by that amount?

      Anyway, this works pretty good now.
    </src-comment>
  </active*>

  <assign|ztNewBlock|<macro|body|<surround|<next-line>|<next-line>|<arg|body>>>>

  <\active*>
    <\src-comment>
      TODO verify? The indent will be the same as that set by the
      firstLineIndent and bodyIndent.
    </src-comment>
  </active*>

  <assign|ztbibIndent|<macro|body|<arg|body>>>

  <assign|zt-item-hsep|1spc>

  \;

  <assign|ztLeftMargin|<macro|label|<arg|label><with|tab-stop|<if|<greatereq|<get-arity|<value|zotero-BibliographyStyle_arrayList>>|1>|<look-up|<value|zotero-BibliographyStyle_arrayList>|0>|<value|zotero-BibliographyStyle_bodyIndent>>|<ztRigidHspace|<if|<greater|<ztRawWidth|<ztRigidHspace|<value|tab-stop>>>|0>|<ztAsTmlen|<minimum|<minus|<ztRawWidth|<ztRigidHspace|<value|tab-stop>>>|<plus|<ztRawWidth|<arg|label>>|<ztRawWidth|<ztRigidHspace|<value|zt-item-hsep>>>>>|<ztRawWidth|<ztRigidHspace|<value|zt-item-hsep>>>>>|<value|zt-item-hsep>>>>>>

  \;

  <assign|ztRightInline|<value|identity>>

  <\active*>
    <\src-comment>
      The embeddedBibliographyEntry is going to be blank unless the
      state.sys.prototype.embedBibliographyEntry(state, this.itemID) is
      defined inside of citeproc. I honestly don't know what it's for right
      now; there are not any examples of it that I can find with a Google
      search. I want to remove it now, but it's already there and I don't
      want to break my older documents... and maybe it will prove useful to
      define it someday... or remove it later. It is something for injecting
      behavior into citeproc.js by defining it. Search the citeproc.js
      sources to see how it's actually referenced in there.
    </src-comment>
  </active*>

  <\active*>
    <\src-comment>
      One ztbibItemText is emitted by the Juris-M / Zotero citeproc-js bbl
      output format for each bibliography item. The arguments are:

      \ sysID \ is a string: \ \ sys_id =
      state.registry.registry[this.system_id].ref.id; \ It looks like an
      integer.

      refsList is not used yet, and for now it is blank. \ If I decide to
      move this functionality into the propachi-texmacs citeproc-js bbl
      output format, I think it will be an in-document-order list of zfieldID
      strings, one for each zfield or citation cluster (inside of citeproc
      they are known as citation clusters, not zfields) wherein this
      bibliography item was referenced. When the same sysID is referenced
      more than once inside of the same zfield, that zfieldID will appear
      once for each time. After the first one, subequent ones will have a
      number appended to the label, incrementing, to create separate labels
      for each citation. It will not simply link to the start of the citation
      cluster zfield because those can split across pages when they contain
      many citations and are long, as for IndigoBook inline legal citations.
      This implies that it will be necessary to ensure that the zfieldID's
      never contain spaces or commas or whatever character is used to
      separate them in this list. How about semi-colons, with no extra
      spaces? Q: Can that string tree be split by a TeXmacs macro, or will it
      need to be processed by a scheme function? Also, for use with LaTeX, it
      may be necessary to have citeproc-js emit the refsList in order to make
      backlinks work? Not sure; no time for that right now.

      citekey \ is a string, and is formed by pasting the string "sysID" to
      the sysID for this bibliography entry, unless
      state.sys.getBibTeXCiteKey(sysID, state) is defined, which I think is
      supposed to return a BibTeX style citation key. I've never tested this
      case because better BibTeX for Zotero was incompatible in some way with
      propachi-texmacs and I think it's where that function gets defined.
      Because I never use it, this argument could be removed for the purposes
      of this extension to TeXmacs, but if the same bbl output format was to
      be used for LaTeX, perhaps auto-completion or whatever would work
      better for some people if it uses a standard BibTeX citekey format? So
      for now it stays.

      body \ is the body of the bibliography entry. It contains text wrapped
      in the macros just above this comment.
    </src-comment>
  </active*>

  <assign|ztbibItemText|<\macro|sysID|refsList|citekey|body>
    <\with|par-sep|<times|<value|par-sep>|<value|zotero-BibliographyStyle_lineSpacing>>|ztbibItem-vsep|<times|<value|ztbibItem-vsep>|<value|zotero-BibliographyStyle_entrySpacing>>>
      <\surround|<vspace*|<value|item-vsep>>|<right-flush>>
        <\with|par-no-first|false|par-first|<value|zotero-BibliographyStyle_firstLineIndent>|par-left|<value|zotero-BibliographyStyle_bodyIndent>>
          <label|<merge|zbibSysID|<arg|sysID>>><arg|body><ztbibItemRefsList|<arg|sysID>>
        </with>
      </surround>
    </with>
  </macro>>

  \;

  \;

  <assign|zt-render-bibItemRefsLists|true>

  \;

  <inactive|<assign|XXXzbibItemRefsList-left|
  \ [<with|font-shape|italic|refs:> >>

  <assign|ztbibItemRefsList-left| \ [>

  <assign|ztbibItemRefsList-sep|, >

  <assign|ztbibItemRefsList-right|]>

  <assign|ztbibItemRef|<macro|label|<pageref|<arg|label>>>>

  \;

  <assign|zt-ref-sep-extra|<macro|x|<value|ztbibItemRefsList-sep><ztbibItemRef|<arg|x>>>>

  <assign|zt-ref-sep|<xmacro|args|<ztbibItemRef|<arg|args|0>><map-args|zt-ref-sep-extra|concat|args|1>>>

  \;

  <assign|get-ztbibItemRefsList|<macro|sysID|<extern|tm-zotero-ext:get-ztbibItemRefsList|<arg|sysID>>>>

  <assign|ztbibItemRefsList|<macro|sysID|<with|render-bibItemRefsList|<value|zt-render-bibItemRefsLists>|<if|<value|render-bibItemRefsList>|<ztbibItemRefsList-left><get-ztbibItemRefsList|<arg|sysID>><ztbibItemRefsList-right>>>>>

  \;

  \;

  <assign|ztbibSubHeadingTextSize|1>

  <assign|ztbibSubHeadingVspace*|1fn>

  \;

  <assign|ztbibSubHeading|<macro|name|<with|subheading-vspace|<value|ztbibSubHeadingVspace*>|font-size|<value|ztbibSubHeadingTextSize>|<sectional-normal-bold|<vspace*|<value|subheading-vspace>><arg|name>>>>>

  <\active*>
    <\src-comment>
      Juris-M / Zotero Citations and Bibliography. Both the zcite and
      zbibliography macros must have the same arity, semantics, and order of
      arguments because Zotero treats them generically as "fields".

      The use of `surround' in the zbibliography forces it to be typeset in
      block context. Without that, the lines don't wrap properly and run off
      the right edge of the page. The zcite on the other hand must be in line
      context, because if it's block context, you can't put a citation
      in-text without it forcing itself to be on it's own line. When I was
      trying to use a converter from rtf to TeXmacs, they kept coming out as
      blocks rather than in-line.

      tm-zotero-ensure-zfield-interned! triggers adding of the zfield to the
      tm-zotero data structures used to keep track of zfields in the buffer
      and the information needed for the integration with Juris-M or Zotero.
      This macro is not meant to be used outside of the expansion of the
      zcite or zbibliography macros.
    </src-comment>
  </active*>

  <assign|tm-zotero-ensure-zfield-interned!|<macro|fieldID-t|<extern|tm-zotero-ext:ensure-zfield-interned!|<arg|fieldID-t>>>>

  <assign|zcite-flag-if-modified|<macro|fieldCode|<case|<look-up|<arg|fieldCode>|2>|<flag|Modified|red>|<flag|Not
  Modified|green>>>>

  \;

  <assign|zcite|<macro|fieldID|fieldCode|fieldText|<with|zt-zfieldID|<arg|fieldID>|<tm-zotero-ensure-zfield-interned!|<arg|fieldID>><zcite-flag-if-modified|<arg|fieldCode>><with|dummy|<value|zt-link-FromCiteToBib>|<render-zcite|<arg|fieldID>|<arg|fieldText>>>>>>

  <drd-props|XXXrender-zcite|accessible|1>

  <drd-props|zcite|disable-writability|0|unaccessible|0|disable-writability|1|unaccessible|1|disable-writability|2|unaccessible|2>

  \;

  <assign|zt-option-zbib-font-size|0.84>

  <assign|zbibColumns|1>

  <assign|zt-option-zbib-zt-wrap-with-page-break-before|false>

  <assign|zt-option-zbib-zt-wrap-with-new-double-page-before|false>

  <assign|zt-extra-surround-before|>

  <assign|zbibliography-heading|<macro|<principal-section*|<bibliography-text>>>>

  <assign|zbibliography|<\macro|fieldID|fieldCode|fieldText>
    <\surround|<case|<equal|2|<value|zbibPageBefore>>|<new-dpage*>|<equal|1|<value|zbibPageBefore>>|<page-break*>|><zt-extra-surround-before><set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|0>|<right-flush>>
      <tm-zotero-ensure-zfield-interned!|<arg|fieldID>><zbibliography-heading>

      <with|font-size|<value|zt-option-zbib-font-size>|par-left|0tab|par-first|0tab|par-no-first|true|zt-not-inside-zbibliography|false|par-columns|<value|zbibColumns>|dummy|<value|ztbibSubHeadingVspace*>|dummy|<value|zt-link-BibToURL>|dummy|<value|zt-render-bibItemRefsLists>|dummy|<value|zbibPageBefore>|<arg|fieldText>>
    </surround>
  </macro>>

  <drd-props|zbibliography|disable-writability|0|unaccessible|0|disable-writability|1|unaccessible|1|enable-writability|2|accessible|2>

  \;

  <assign|has-zbibliography?|<macro|<extern|(lambda ()
  (tm-zotero-ext:document-has-zbibliography?))>>>

  \;

  <assign|inside-footnote?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:inside-footnote? t))|<arg|t>>>>

  <assign|inside-endnote?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:inside-endnote? t))|<arg|t>>>>

  <assign|inside-note?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:inside-note? t))|<arg|t>>>>

  <assign|inside-zcite?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:inside-zcite? t))|<arg|t>>>>

  <assign|inside-zbibliography?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:inside-zbibliography? t))|<arg|t>>>>

  <assign|not-inside-zbibliography?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:not-inside-zbibliography? t))|<arg|t>>>>

  <assign|inside-zfield?|<macro|t|<extern|(lambda (t)
  (tm-zotero-ext:inside-zfield? t))|<arg|t>>>>

  \;

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|font|bonum>
    <associate|math-font|math-bonum>
    <associate|preamble|true>
  </collection>
</initial>