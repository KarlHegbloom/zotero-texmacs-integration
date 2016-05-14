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

  <assign|zcite*|<macro|fieldID|fieldCode|<write|<value|bib-prefix>|<arg|fieldID>><set-binding|<merge|<value|bib-prefix>|-fields>|<merge|<get-binding|<merge|<value|bib-prefix>|-fields>>|<tuple|<arg|fieldID>>>><set-binding|<merge|<value|bib-prefix>|-|<arg|fieldID>|-code>|<arg|fieldCode>><extern|zotero-set-fields-list!|<get-binding|<merge|<value|bib-prefix>|-fields>>>>>

  <assign|zcite|<macro|fieldID|fieldCode|<zcite*|<arg|fieldID>|<arg|fieldCode>><reference|<merge|<value|bib-prefix>|-|<arg|fieldID>|-code>>>>

  \;

  <assign|zcite-get-fields|<macro|<get-binding|<merge|<value|bib-prefix>|-fields>>>>

  <assign|zcite-get-fieldCode|<macro|fieldID|<get-binding|<merge|<value|bib-prefix>|-|<arg|fieldID>|-code>>>>

  <assign|zcite-get-fieldText|<macro|fieldID|<get-binding|<merge|<value|bib-prefix>|-|<arg|fieldID>|-|>>>>

  \;

  <assign|cite-link|<macro|key|text|<style-with|src-compact|none|<locus|<id|<hard-id|<arg|key>>>|<link|hyperlink|<id|<hard-id|<arg|key>>>|<url|<merge|#|<value|bib-prefix>|-|<arg|key>>>>|<arg|text>>>>>

  <assign|transform-bibitem|<macro|body|<arg|body>>>

  <assign|render-bibitem|<macro|text|<style-with|src-compact|none|<with|par-first|<minus|1tmpt|<value|bibitem-width>>|<yes-indent>><resize|<arg|text>|||<maximum|1r|<value|bibitem-width>>|>>>>

  <assign|bibitem-with-key|<macro|text|key|<style-with|src-compact|none|<bibitem*|<arg|text>><label|<merge|<value|bib-prefix>|-|<arg|key>>>>>>

  <assign|bibitem*|<macro|text|<style-with|src-compact|none|<render-bibitem|<transform-bibitem|<arg|text>>><set-binding|<arg|text>>>>>

  <\active*>
    <\src-comment>
      Main commands for citations
    </src-comment>
  </active*>

  <assign|render-cite|<macro|x|<arg|x>>>

  \;

  \;
</body>

<\initial>
  <\collection>
    <associate|preamble|true>
  </collection>
</initial>