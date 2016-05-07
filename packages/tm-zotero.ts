<TeXmacs|1.99.4>

<style|source>

<\body>
  <active*|<\src-title>
    <src-package|tm-zotero|1.0>

    <\src-purpose>
      This package contains extended macros for citations and provides a
      <TeXmacs> bridge to the Juris-M or Zotero reference manager for
      Firefox.
    </src-purpose>

    <src-copyright|2016|Karl Martin Hegbloom>

    <src-copyright|2006|Joris van der Hoeven (cite-author-year.ts)>

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
      Helper macros
    </src-comment>
  </active*>

  <assign|bib-prefix|bib>

  <assign|with-bib|<macro|bib|body|<with|bib-prefix|<arg|bib>|<arg|body>>>>

  \;

  <assign|fieldAdd|<macro|fieldID|>>

  <assign|set-fieldText|<macro|fieldID|fieldText|>>

  <assign|set-fieldCode|<macro|fieldID|fieldCode|>>

  \;

  <assign|get-fieldText|<macro|fieldID|>>

  <assign|get-fieldCode|<macro|fieldID|>>

  \;

  \;

  <assign|cite|<macro|>>

  \;

  \;

  <assign|cite-add|<macro|key|<write|<value|bib-prefix>|<arg|key>>>>

  <assign|cite-data|<macro|key|<get-binding|<merge|<value|bib-prefix>|-|<arg|key>>>>>

  <assign|cite-link|<macro|key|text|<style-with|src-compact|none|<locus|<id|<hard-id|<arg|key>>>|<link|hyperlink|<id|<hard-id|<arg|key>>>|<url|<merge|#|<value|bib-prefix>|-|<arg|key>>>>|<arg|text>>>>>

  <\active*>
    <\src-comment>
      Main commands for citations
    </src-comment>
  </active*>

  <assign|render-cite|<macro|x|(<arg|x>)>>

  <assign|cite-sep|<macro|; >>

  \;

  <assign|bibitem*|<macro|text|<style-with|src-compact|none|<assign|bibitem-nr|<plus|<value|bibitem-nr>|1>><render-natbibitem|<transform-natbibitem|<natbib-author*|<arg|text>>,
  <natbib-year|<arg|text>>>><set-binding|<arg|text>>>>>

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>