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
      hlink and href \ display rendering options.
    </src-comment>
  </active*>

  <assign|zt-pref-hrefs-as-footnotes|on>

  <assign|zt-pref-hlinks-with-href-footnotes?|on>

  <assign|zt-option-this-cite-in-text?|off>

  <\active*>
    <\src-comment>
      Setup for special handling of in-text citations inside of footnotes and
      endnotes; And for hlinks with extra href's displayed as footnotes, and
      hrefs that display as footnotes rather than in-text.
    </src-comment>
  </active*>

  <assign|zt-orig-footnote|<value|footnote>>

  <assign|zt-footnote|<macro|body|<with|zt-pref-hrefs-as-footnotes?|off|zt-pref-hlinks-with-href-footnotes?|off|zt-option-this-zcite-in-text?|on|<zt-orig-footnote|<arg|body>>>>>

  <if|<unequal|<value|zt-footnote>|<value|footnote>>|<assign|footnote|<value|zt-footnote>>>

  \;

  <assign|zt-orig-href|<value|href>>

  <assign|zt-href|<macro|dest|<if|<equal|<value|zt-pref-hrefs-as-footnotes?>|on>|<zt-footnote|<zt-orig-href|<arg|dest>>>|<zt-orig-href|<arg|dest>>>>>

  <if|<unequal|<value|zt-href>|<value|href>>|<assign|href|<value|zt-href>>>

  \;

  <assign|zt-orig-hlink|<value|hlink>>

  <assign|zt-hlink|<macro|linktext|dest|<if|<equal|<value|zt-pref-hlinks-with-href-footnotes?>|on>|<zt-orig-hlink|<arg|linktext>|<arg|dest>><zt-footnote|<zt-orig-href|<arg|dest>>|<zt-orig-hlink|<arg|linktext>|<arg|dest>>>>>>

  <if|<unequal|<value|zt-hlink>|<value|hlink>>|<assign|hlink|<value|zt-hlink>>>

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

  <assign|zt-zcite-in-text|<macro|fieldID|citebody|
  <arg|citebody><set-binding|<merge|zotero-|<arg|fieldID>|-noteIndex>|0>>>

  <assign|zt-zcite-as-footnote|<macro|fieldID|citebody|<zt-footnote|<arg|citebody><set-binding|<merge|zotero-|<arg|fieldID>|-noteIndex>|<value|footnote-nr>>>>>

  <assign|zt-zcite-as-endnote|<macro|fieldID|citebody|<zt-endnote|<arg|citebody><set-binding|<merge|zotero-|<arg|fieldID>|-noteIndex>|<value|endnote-nr>>>>>

  <assign|render-zcite|<macro|fieldID|citebody|<case|<or|<equal|<value|zotero-pref-noteType>|0>|<equal|<value|zt-option-this-zcite-in-text?>|on>>|<zt-zcite-in-text|<arg|fieldID>|<arg|citebody>>|<and|<equal|<value|zotero-pref-noteType>|1>|<unequal|<value|zt-option-this-zcite-in-text?>|on>>|<zt-zcite-as-footnote|<arg|fieldID>|<arg|citebody>>|<equal|<value|zotero-pref-noteType>|2>|<zt-zcite-as-endnote|<arg|fieldID>|<arg|citebody>>>>>

  <\active*>
    <\src-comment>
      Juris-M / Zotero Citations (do not use zcite* yet. I don't know if it
      will be part of the final thing.)
    </src-comment>
  </active*>

  <assign|zcite*|<macro|fieldID|fieldCode|fieldText|<flag|Hidden zcite|>>>

  <assign|zcite|<macro|fieldID|fieldCode|fieldText|<compound|render-zcite|<arg|fieldID>|<arg|fieldText>>>>
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>