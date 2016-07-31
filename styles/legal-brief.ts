<TeXmacs|1.99.4>

<style|source>

<\body>
  <\active*>
    <\src-title>
      <src-style-file|legal-brief|1.1>

      <\src-purpose>
        The legal-brief document style.
      </src-purpose>

      <\src-copyright|2015, 2016>
        Karl M.<space|1spc>Hegbloom
      </src-copyright>

      <\src-license>
        This software falls under the <hlink|GNU general public license,
        version 3 or later|$TEXMACS_PATH/LICENSE>. It comes WITHOUT ANY
        WARRANTY WHATSOEVER. You should have received a copy of the license
        which the software. If not, see <hlink|http://www.gnu.org/licenses/gpl-3.0.html|http://www.gnu.org/licenses/gpl-3.0.html>.
      </src-license>
    </src-title>
  </active*>

  <\active*>
    <\src-comment>
      This is sort of a work in progress. I've been using it for a while, but
      don't consider it to be feature-complete. Any ideas?
    </src-comment>
  </active*>

  <use-package|english|std|env|title-generic|header-generic|header-article|header-letter|section-article|tm-zotero>

  <use-module|(insert-legal-templates)>

  <\active*>
    <\src-comment>
      Page size and margins per Utah Rules.
    </src-comment>
  </active*>

  <assign|page-type|letter>

  <assign|page-medium|paper>

  <assign|page-screen-margin|false>

  <assign|page-width-margin|false>

  <assign|page-height-margin|false>

  <assign|page-bot|1in>

  <assign|page-even|1in>

  <assign|page-odd|1in>

  <assign|page-right|1in>

  <assign|page-top|1.5in>

  \;

  <assign|font-base-size|12>

  <assign|XXXfont-base-size|13>

  <assign|font|TeX Gyre Termes>

  \;

  <assign|legal-brief-quote-env-font-size|0.83333>

  <assign|legal-brief-quote-env-par-sep|0.25fn>

  <assign|legal-brief-quote-env-par-line-sep|0.025fns>

  <assign|XXXlegal-brief-quote-env-par-par-sep|<value|quote-interparagraph>>

  <assign|legal-brief-quote-env-par-par-sep|0.25fn>

  \;

  <assign|par-first|0fn>

  <\active*>
    <\src-comment>
      par-sep is "interline separation"
    </src-comment>
  </active*>

  <assign|XXXpar-sep|0.2fn>

  <assign|par-sep|0.5fn>

  <\active*>
    <\src-comment>
      par-line-sep is "interline space"
    </src-comment>
  </active*>

  <assign|XXXpar-line-sep|0.025fns>

  <assign|par-line-sep|0.025fns>

  <\active*>
    <\src-comment>
      par-par-sep is "interparagraph space"
    </src-comment>
  </active*>

  <assign|XXXpar-par-sep|0.5fns>

  <assign|par-par-sep|0.25fn>

  <\active*>
    <\src-comment>
      These should be parameterized and made into options in the menu,
      eventually. The main thing is that when I change this, I don't want to
      break my old documents...
    </src-comment>
  </active*>

  <assign|legal-brief-footnote-size|<value|normal-size>>

  <assign|XXXlegal-brief-footnote-size|<value|small>>

  <assign|legal-brief-footnote-par-sep|0.25fn>

  <assign|legal-brief-footnote-par-line-sep|<value|par-line-sep>>

  <assign|legal-brief-footnote-par-par-sep|<value|par-par-sep>>

  <assign|page-fnote-barlen|1par>

  <assign|page-fnote-sep|1.5fn>

  <assign|par-fnote-sep|0.5fn>

  \;

  <assign|locus-color|black>

  <assign|visited-color|black>

  \;

  <assign|chapter-display-numbers|<macro|true>>

  <assign|chapter-title|<macro|name|<style-with|src-compact|none|<sectional-centered-bold|<vspace*|1.0fn><huge|<arg|name>><vspace|0.5fn>>>>>

  \;

  <assign|bibliography-text|Table of Authorities>

  <assign|appendix-text|Exhibit>

  \;

  <assign|hide-redacted|false>

  <assign|redact|<macro|body|<with|hide-redactions|<value|hide-redacted>|<if*|<not|<value|hide-redactions>>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      This must be part of the style sheet unless there's a reliable way to
      determine the amount to shift it. Without shifting it, it will not be
      displayed right since the master document's margins apply to the
      location of the box the included file is typeset into.

      <with|color|red|Don't use this. It does not work right. The resulting
      document is too slow to type in and multiple page includes don't work
      as expected. Some code needs to be written in C++ to add this feature
      properly via the pdfhummus library.>
    </src-comment>
  </active*>

  <assign|xinclude-pdfpages|<macro|incl|<page-break*><blanc-page><shift|<include|<arg|incl>>|<plus|1l|-1in>|<plus|1b|1.5in>>>>

  <assign|xxinclude-pdfpages|<macro|incl|<include|<arg|incl>>>>

  <assign|include-pdfpages|<value|include>>

  <assign|par-fnote-sep|<macro|0.1fn>>

  <assign|XXXfootnote|<macro|body|<next-footnote><render-footnote|<the-footnote>|<with|par-sep|0.12fn|<arg|body>>><space|0spc><label|<merge|footnr-|<the-footnote>>><rsup|<with|font-shape|right|<reference|<merge|footnote-|<the-footnote>>>>>>>

  <assign|XXXrender-parnote*|<macro|sym|body|<style-with|src-compact|none|<\float|float|h>
    <smaller|<with|par-mode|justify|par-sep|0.12fn|font-shape|right|dummy|<value|page-fnote-sep>|dummy|<value|page-fnote-barlen>|<arg|sym><footnote-sep><arg|body>>>
  </float>>>>

  <assign|XXXdagger-parnote|<macro|body|<compound|render-parnote*|\<dagger\>|<arg|body>><rsup|<with|font-shape|right|\<dagger\>>>>>

  <assign|XXXddagger-parnote|<macro|body|<compound|render-parnote*|\<dagger\>\<dagger\>|<arg|body>><rsup|<with|font-shape|right|\<dagger\>\<dagger\>>>>>

  <assign|XXXasterisk-parnote|<macro|body|<compound|render-parnote*|*|<arg|body>><rsup|<with|font-shape|right|*>>>>

  <assign|XXXaasterisk-parnote|<macro|body|<compound|render-parnote*|**|<arg|body>><rsup|<with|font-shape|right|**>>>>

  \;

  <assign|quote-env|<\macro|body>
    <\padded>
      <\indent-both|<value|quote-left-indentation>|<value|quote-right-indentation>>
        <with|par-first|0fn|font-size|<value|legal-brief-quote-env-font-size>|par-par-sep|<value|legal-brief-quote-env-par-par-sep>|par-sep|<value|legal-brief-quote-env-par-sep>|par-line-sep|<value|legal-brief-quote-env-par-line-sep>|<arg|body>>
      </indent-both>
    </padded>
  </macro>>

  <assign|verse|<\macro|body>
    <\padded>
      <\indent-both|<plus|<value|quote-left-indentation>|<value|verse-hangover>>|<value|quote-right-indentation>>
        <with|par-first|<minus|<value|verse-hangover>>|font-size|<value|legal-brief-quote-env-font-size>|par-sep|<value|legal-brief-quote-env-par-sep>|par-line-sep|<value|legal-brief-quote-env-par-line-sep>|par-par-sep|<value|legal-brief-quote-env-par-par-sep>|<surround|<yes-indent>||<arg|body>>>
      </indent-both>
    </padded>
  </macro>>

  \;

  <assign|section-display-numbers|<macro|true>>

  <assign|subsection-display-numbers|<macro|true>>

  <assign|subsubsection-display-numbers|<macro|true>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|true>>

  \;

  <assign|paragraph-sep|<macro|<space|0.5spc>>>

  <assign|subparagraph-sep|<macro|<space|0.5spc>>>

  <\active*>
    <\src-comment>
      Paragraphs with a paragraphsign before the paragraph number, and also
      with section numbering. As a workaround for a problem with PDF
      formation, it must wrap a Unicode paragraph sign with a font that is
      not Cork encoded, so that it translates properly to a PDF string that
      can be displayed in the PDF outlines. For some reason the unicode
      paragraphsign, which shows up as such in palatino font, shows up as a
      sectionsign in the computer modern roman font that this source file is
      displayed with while editting it. In the document itself when it' s
      displayed in palatino, it's a unicode paragraphsign.
    </src-comment>
  </active*>

  <assign|unicode-paragraphsign|<macro|<with|font|palatino|\<#00B6\>>>>

  <assign|display-paragraph|<macro|nr|<if|<unequal|<subsubsection-nr>|0>|<subsubsection-prefix>|<if|<unequal|<subsection-nr>|0>|<subsection-prefix>|<if|<unequal|<section-nr>|0>|<section-prefix>>>><unicode-paragraphsign><space|0.2spc><arg|nr>>>

  <assign|P|<unicode-paragraphsign>>

  \;

  <assign|paragraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.25fn><arg|name><paragraph-sep>>>>>

  \;

  <assign|subparagraph-title|<macro|name|<style-with|src-compact|none|<sectional-short-bold|<vspace*|0.125fn><arg|name><subparagraph-sep>>>>>

  <\active*>
    <\src-comment>
      Must redefine these to make sure the numbering resets properly.
    </src-comment>
  </active*>

  <assign|chapter-clean|<macro|<reset-section><section-clean>>>

  <assign|section-clean|<macro|<reset-subsection><subsection-clean>>>

  <assign|subsection-clean|<macro|<reset-subsubsection><subsubsection-clean>>>

  <assign|subsubsection-clean|<macro|<reset-paragraph><paragraph-clean>>>

  <assign|paragraph-clean|<macro|<reset-subparagraph><subparagraph-clean>>>

  \;

  <assign|item-hsep|<macro|0.5tab>>

  <assign|item-vsep|<macro|0.1fns>>

  <assign|aligned-dot-item|<macro|name|<aligned-item|<arg|name>.>>>

  <assign|aligned-bracket-item|<macro|name|<aligned-item|<arg|name><with|font-shape|right|)>>>>

  <assign|aligned-2-bracket-item|<macro|name|<aligned-item|<with|font-shape|right|(><arg|name><with|font-shape|right|)>>>>

  \;

  <assign|enumerate-level|0>

  <assign|enumerate-levels|9>

  <assign|enumerate-reduce|<macro|nr|<plus|<mod|<minus|<arg|nr>|1>|<minimum|<value|enumerate-levels>|9>>|1>>>

  <new-list|enumerate-1|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|Roman>>>

  <new-list|enumerate-2|<value|aligned-dot-item>|<macro|name|<number|<arg|name>|Alpha>>>

  <new-list|enumerate-3|<value|aligned-dot-item>|<value|identity>>

  <new-list|enumerate-4|<value|aligned-bracket-item>|<macro|name|<with|font-shape|italic|<number|<arg|name>|alpha>>>>

  <new-list|enumerate-5|<value|aligned-2-bracket-item>|<value|identity>>

  <new-list|enumerate-6|<value|aligned-2-bracket-item>|<macro|name|<with|font-shape|italic|<number|<arg|name>|alpha>>>>

  <new-list|enumerate-7|<value|aligned-bracket-item>|<macro|name|<with|font-shape|italic|<number|<arg|name>|roman>>>>

  <new-list|enumerate-8|<value|aligned-dot-item>|<macro|name|<with|font-shape|left-slanted|<arg|name>>>>

  <new-list|enumerate-9|<value|aligned-bracket-item>|<macro|name|<with|font-shape|left-slanted|<number|<arg|name>|alpha>>>>

  \;

  <assign|sectional-short-style|<macro|true>>

  <assign|heading-toc|<macro|name|<toc-normal-2|<arg|name>>>>

  \;

  <assign|ltoc|<macro|name|<flag|ltoc|brown|<style-with|src-compact|all|name>><case|<style-with|src-compact|all|<equal|<value|enumerate-level>|1>>|<toc-normal-2|<the-item>.
  <arg|name>>|<equal|<value|enumerate-level>|2>|<toc-normal-3|<the-item>.
  <arg|name>>|<equal|<value|enumerate-level>|3>|<toc-small-1|<the-item>.
  <arg|name>>|<equal|<value|enumerate-level>|4>|<style-with|src-compact|all|<toc-small-2|<the-item>.
  <arg|name>>>|<equal|<value|enumerate-level>|5>|<toc-small-3|<the-item>.
  <arg|name>>|<equal|<value|enumerate-level>|6>|<toc-small-4|<the-item>.
  <arg|name>>>>>

  \;

  <assign|toc-small-3|<macro|what|<toc-entry|toc-6|<arg|what>>>>

  <assign|toc-small-4|<macro|what|<toc-entry|toc-7|<arg|what>>>>

  <assign|toc-5|<macro|left|right|<with|par-left|5tab|<arg|left><toc-dots><no-break><arg|right><vspace|0.15fn>>>>

  <assign|toc-6|<macro|left|right|<with|par-left|6tab|<arg|left><toc-dots><no-break><arg|right><vspace|0.15fn>>>>

  <assign|toc-7|<macro|left|right|<with|par-left|7tab|<arg|left><toc-dots><no-break><arg|right><vspace|0.15fn>>>>

  \;

  <assign|nbspace-extra|<macro|x|<space|0.2spc><arg|x>>>

  <assign|nbspace-separated|<xmacro|args|<arg|args|0><map-args|nbspace-extra|concat|args|1>>>

  <assign|casenum|<macro|a|b|c|<nbspace-separated|<arg|a>|<arg|b>|<arg|c>>>>

  <\active*>
    <\src-comment>
      <inactive|<use-package|casenick>>\ 
    </src-comment>
  </active*>

  \;

  <\active*>
    <\src-comment>
      This is from std-markup.ts and it shows how I can maybe make the Utah
      Code references into hyperlinks to the web site, with a little work...

      This is from before tm-zotero.ts was developed. It is stale and left
      here only so my old documents still work. I leave it here to show, to
      give people ideas and perhaps so I can use it later, maybe integrated
      with tm-zotero somehow, to make clickable links to the statutes?
    </src-comment>
  </active*>

  <assign|xxxhref|<macro|body|<hlink|<with|font-family|tt|language|verbatim|<arg|body>>|<arg|body>>>>

  \;

  <assign|dash-extra|<macro|x|<no-break>-<no-break><arg|x>>>

  <assign|dash-separated|<xmacro|args|<arg|args|0><map-args|dash-extra|concat|args|1>>>

  <assign|TC|<macro|Title|Chapter|Ÿ<space|0.2spc><dash-separated|<arg|Title>|<arg|Chapter>>>>

  <assign|TCS|<\macro|Title|Chapter|Section>
    <dash-separated|<TC|<arg|Title>|<arg|Chapter>>|<arg|Section>>
  </macro>>

  <assign|TCSp|<\macro|Title|Chapter|Section|p1>
    <TCS|<arg|Title>|<arg|Chapter>|<arg|Section>>(<arg|p1>)
  </macro>>

  <assign|TCSpp|<\macro|Title|Chapter|Section|p1|p2>
    <TCSp|<arg|Title>|<arg|Chapter>|<arg|Section>|<arg|p1>>(<arg|p2>)
  </macro>>

  <assign|TCSppp|<\macro|Title|Chapter|Section|p1|p2|p3>
    <TCSpp|<arg|Title>|<arg|Chapter>|<arg|Section>|<arg|p1>|<arg|p2>>(<with|font-shape|italic|<arg|p3>>)
  </macro>>

  <assign|TCSpppp|<\macro|Title|Chapter|Section|p1|p2|p3|p4>
    <TCSppp|<arg|Title>|<arg|Chapter>|<arg|Section>|<arg|p1>|<arg|p2>|<arg|p3>>(<arg|p4>)
  </macro>>

  <\active*>
    <\src-comment>
      Footnotes.

      In Utah courts, they want the footnotes to be the same size as the
      text. I don't think that looks good, but that's what they want.
    </src-comment>
  </active*>

  <assign|render-footnote*|<macro|sym|nr|body|<style-with|src-compact|none|<\float|footnote|>
    <legal-brief-footnote-size|<with|par-sep|<value|legal-brief-footnote-par-sep>|par-line-sep|<value|legal-brief-footnote-par-line-sep>|par-par-sep|<value|legal-brief-footnote-par-par-sep>|par-mode|justify|par-left|0cm|par-right|0cm|font-shape|right|dummy|<value|par-fnote-sep>|dummy|<value|page-fnote-sep>|dummy|<value|page-fnote-barlen>|dummy|<value|page-fnote-barsep>|<style-with|src-compact|none|<surround|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<merge|#footnr-|<arg|nr>>>>|<arg|sym>><footnote-sep>|<set-binding|<merge|footnote-|<arg|nr>>|<value|the-label>|body><right-flush>|<style-with|src-compact|none|<arg|body>>>>>>
  </float>>>>
</body>

<\initial>
  <\collection>
    <associate|font|TeX Gyre Termes>
    <associate|page-type|letter>
    <associate|src-style|angular>
  </collection>
</initial>