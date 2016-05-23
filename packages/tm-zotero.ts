<TeXmacs|1.99.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tm-zotero|0.007-UT-1-W>

    <\src-purpose>
      This package contains extended macros for citations and provides a
      <TeXmacs> bridge to the Juris-M or Zotero reference manager for
      Firefox. It utilizes the same interface that is used by the Zotero
      \<rightarrow\> OpenOffice.org connector, presently with a patch applied
      to Juris-M/Zotero to allow switching the output format, and with Better
      BibTeX for Zotero that is extended to create a ".bbl" output formatter.
      (vs HTML or RTF).
    </src-purpose>

    <src-copyright|2016|Karl Martin Hegbloom, Esq.>

    <\src-license>
      This software falls under the <hlink|GNU general public license,
      version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
      WARRANTY WHATSOEVER. You should have received a copy of the license
      which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
    </src-license>
  </src-title>>

  <use-module|(zotero)>

  <\active*>
    <\src-comment>
      Helper macros (Not used presently.) <with|color|blue|Todo:>
      <with|color|red|Support multiple bibliographies per document, etc.>
    </src-comment>
  </active*>

  <assign|zbib-prefix|zbib>

  <assign|with-zbib|<macro|zbib|body|<with|zbib-prefix|<arg|zbib>|<arg|body>>>>

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
      hlink and href \ display rendering options.
    </src-comment>
  </active*>

  <assign|zt-pref-hrefs-as-footnotes|true>

  <assign|zt-pref-hlinks-with-href-footnotes|false>

  <assign|zt-not-inside-footnote|true>

  <assign|zt-option-this-zcite-in-text|false>

  <\active*>
    <\src-comment>
      Setup for special handling of in-text citations inside of footnotes and
      endnotes; And for hlinks with extra href's displayed as footnotes, and
      hrefs that display as footnotes rather than in-text.
    </src-comment>
  </active*>

  <assign|zt-orig-footnote|<value|footnote>>

  <assign|zt-footnote|<macro|body|<style-with|src-compact|none|<next-footnote><with|zt-not-inside-footnote|false|<render-footnote|<the-footnote>|<arg|body>>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<with|font-shape|right|<reference|<merge|footnote-|<the-footnote>>>>>>>>

  <assign|footnote|<value|zt-footnote>>

  \;

  <assign|zt-orig-href|<value|href>>

  <assign|zt-href|<macro|dest|<if|<and|<value|zt-pref-hrefs-as-footnotes>|<value|zt-not-inside-footnote>>|<zt-footnote|<zt-orig-href|<arg|dest>>>|<zt-orig-href|<arg|dest>>>>>

  <assign|href|<value|zt-href>>

  \;

  <assign|zt-orig-hlink|<value|hlink>>

  <assign|zt-hlink|<macro|linktext|dest|<if|<and|<value|zt-pref-hlinks-with-href-footnotes>|<value|zt-not-inside-footnote>>|<zt-orig-hlink|<arg|linktext>|<arg|dest>><zt-footnote|<zt-orig-href|<arg|dest>>>|<zt-orig-hlink|<arg|linktext>|<arg|dest>>>>>

  <assign|hlink|<value|zt-hlink>>

  <\active*>
    <\src-comment>
      End-notes
    </src-comment>
  </active*>

  <assign|zt-endnote|<macro|body|<todo|TODO: Endnotes>>>

  <\active*>
    <\src-comment>
      Citation display depending on CSL noteType: 0
      \<rightarrow\><compound|text|<compound|math|>><compound|math|><compound|math|>
      in-text, 1 \<rightarrow\> footnote, 2 \<rightarrow\> end-note, plus
      override per zcite.
    </src-comment>
  </active*>

  <assign|zt-zcite-in-text|<macro|fieldID|citebody|<arg|citebody><set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|0>>>

  <assign|zt-zcite-as-footnote|<macro|fieldID|citebody|<zt-footnote|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<value|footnote-nr>><arg|citebody>>>>

  <assign|zt-zcite-as-endnote|<macro|fieldID|citebody|<zt-endnote|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|<value|endnote-nr>><arg|citebody>>>>

  <assign|render-zcite|<macro|fieldID|citebody|<case|<or|<value|zotero-pref-noteType0>|<value|zt-option-this-zcite-in-text>>|<zt-zcite-in-text|<arg|fieldID>|<arg|citebody>>|<and|<value|zotero-pref-noteType1>|<value|zt-not-inside-footnote>>|<zt-zcite-as-footnote|<arg|fieldID>|<arg|citebody>>|<value|zotero-pref-noteType1>|<zt-zcite-in-text|<arg|fieldID>|<arg|citebody>>|<value|zotero-pref-noteType2>|<zt-zcite-as-endnote|<arg|fieldID>|<arg|citebody>>>>>

  \;

  <assign|transform-bibitem|<macro|body|>>

  \;

  <\active*>
    <\src-comment>
      Juris-M / Zotero Citations (do not use zcite* yet. I don't know if it
      will be part of the final thing.)
    </src-comment>
  </active*>

  <assign|zcite*|<macro|fieldID|fieldCode|fieldRawText|fieldText|<flag|Hidden
  zcite|green>>>

  <assign|zcite|<macro|fieldID|fieldCode|fieldRawText|fieldText|<render-zcite|<arg|fieldID>|<arg|fieldText>>>>

  <assign|zbibliography|<\macro|fieldID|fieldCode|fieldRawText|fieldText>
    <surround|<set-binding|<merge|zotero|<arg|fieldID>|-noteIndex>|0>|<hflush>|<arg|fieldText>>
  </macro>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>