<TeXmacs|1.99.9>

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

  <use-module|(legal-brief)>

  <assign|is-in-legal-brief-style|<macro|<extern|lb-ext:is-in-legal-brief-style?>>>

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

  <inactive|<assign|XXXfont-base-size|13>>

  <assign|font|TeX Gyre Termes>

  \;

  <assign|legal-brief-quote-env-font-size|0.83333>

  <assign|legal-brief-quote-env-par-sep|0.25fn>

  <assign|legal-brief-quote-env-par-line-sep|0.025fns>

  <inactive|<assign|XXXlegal-brief-quote-env-par-par-sep|<value|quote-interparagraph>>>

  <assign|legal-brief-quote-env-par-par-sep|0.25fn>

  \;

  <assign|locus-color|black>

  <assign|visited-color|black>

  \;

  <assign|bibliography-text|Table of Authorities>

  <assign|appendix-text|Exhibit>

  \;

  <assign|par-first|0fn>

  \;

  <assign|compact-lists|<macro|true>>

  \;

  <assign|sectional-short-style|<macro|true>>

  <assign|heading-toc|<macro|name|<toc-normal-2|<arg|name>>>>

  \;

  <assign|chapter-display-numbers|<macro|true>>

  <assign|section-display-numbers|<macro|true>>

  <assign|subsection-display-numbers|<macro|true>>

  <assign|subsubsection-display-numbers|<macro|true>>

  <assign|paragraph-display-numbers|<macro|true>>

  <assign|subparagraph-display-numbers|<macro|true>>

  \;

  <assign|subsubsection-clean-resets-paragraph|false>

  <assign|display-paragraph-always-use-long-form|false>

  <\active*>
    <\src-comment>
      par-sep is "interline separation"
    </src-comment>
  </active*>

  <inactive|<assign|XXXpar-sep|0.2fn>>

  <assign|par-sep|0.4fn>

  <\active*>
    <\src-comment>
      par-line-sep is "interline space"
    </src-comment>
  </active*>

  <assign|par-line-sep|0.025fns>

  <\active*>
    <\src-comment>
      par-par-sep is "interparagraph space"
    </src-comment>
  </active*>

  <assign|par-par-sep|0.6666fn>

  \;

  <assign|ztbibSubHeadingTextSize|1.2>

  <assign|ztbibSubHeadingVspace*|0.6666fn>

  \;

  <assign|part-vsep*|<macro|<vspace*|<value|par-par-sep>>>>

  <assign|part-vsep|<macro|<vspace|0.0fns>>>

  <assign|chapter-vsep*|<macro|<vspace*|<value|par-par-sep>>>>

  <assign|chapter-vsep|<macro|<vspace|0.0fns>>>

  <assign|section-vsep*|<macro|<vspace*|<value|par-par-sep>>>>

  <assign|section-vsep|<macro|<vspace|0.0fns>>>

  <assign|subsection-vsep*|<macro|<vspace*|<value|par-par-sep>>>>

  <assign|subsection-vsep|<macro|<vspace|0.0fns>>>

  <assign|subsubsection-vsep*|<macro|<vspace*|<value|par-par-sep>>>>

  <assign|subsubsection-vsep|<macro|<vspace|0.0fns>>>

  \;

  <assign|paragraph-vsep*|<macro|<vspace*|0.0fns>>>

  <inactive|<assign|XXparagraph-sep|<macro|<space|0.6666spc>>>>

  <assign|subparagraph-vsep*|<macro|<vspace*|0.0fns>>>

  <inactive|<assign|XXsubparagraph-sep|<macro|<space|0.6666spc>>>>

  \;

  <assign|zt-extra-surround-before|<vspace*|1fn>>

  <\active*>
    <\src-comment>
      These should be parameterized and made into options in the menu,
      eventually. The main thing is that when I change this, I don't want to
      break my old documents...
    </src-comment>
  </active*>

  <assign|with-legal-brief-footnote-size|<macro|body|<with|font-size|<value|legal-brief-footnote-size>|<arg|body>>>>

  <assign|legal-brief-footnote-size|1>

  <assign|legal-brief-footnote-par-sep|0.25fn>

  <assign|legal-brief-footnote-par-line-sep|<value|par-line-sep>>

  <assign|legal-brief-footnote-par-par-sep|<value|par-par-sep>>

  <assign|page-fnote-barlen|1par>

  <assign|page-fnote-sep|1.5fn>

  <assign|par-fnote-sep|0.5fn>

  \;

  <\active*>
    <\src-comment>
      This <with|font-series|bold|<with|font-family|tt|redact>> macro does
      not work right on block content. It only works right on in-line
      content. I think that usually or normally you would not redact as you
      write; it's something done later on\<ldots\> though perhaps for some
      documents, if the plan includes a press release or whatever with names
      hidden you might write the redact macro around things to start
      with\<ldots\> The problem becomes that since it won't let you redact an
      entire paragraph at once, you have to light up and wrap with a redact
      each <with|font-series|bold|line> of the text,
      <with|font-series|bold|after> the line-breaking has happened\<ldots\>
      It would be better to have a way for this to work for both in-line and
      block context content. How?
    </src-comment>
  </active*>

  <assign|hide-redacted|false>

  <assign|redact|<macro|body|<active*|><with|hide-redactions|<value|hide-redacted>|<if|<value|hide-redactions>|<with|distorted-frequency|0.666|distorted-strength|6.66|gnawed-frequency|1.0|gnawed-strength|0.666|<gnawed|<distorted|<repeat|<style-with|src-compact|none|<arg|body>>|<with|font-series|bold|\<equiv\>>>>>>|<arg|body>>>>>

  <inactive|<assign|I_GIVE_UP_redact|<\macro|body>
    <\with|distorted-frequency|0.666|distorted-strength|6.66|gnawed-frequency|1.0|gnawed-strength|0.666>
      <style-with|src-compact|none|<datoms|<macro|x|<distorted|<repeat|<arg|x>|<with|font-series|bold|\<equiv\>>>>>|<phantom|<arg|body>>>>

      \;
    </with>

    \;
  </macro>>>

  \;

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

  <inactive|<assign|XXXinclude-pdfpages|<macro|incl|<page-break*><blanc-page><shift|<include|<arg|incl>>|<plus|1l|-1in>|<plus|1b|1.5in>>>>>

  <inactive|<assign|XXXXinclude-pdfpages|<macro|incl|<include|<arg|incl>>>>>

  <inactive|<assign|XXXinclude-pdfpages|<value|include>>>

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

  \;

  \;

  <assign|ztbibSubHeading|<macro|name|<with|subheading-vspace|<value|ztbibSubHeadingVspace*>|font-size|<value|ztbibSubHeadingTextSize>|<compound|sectional-centered|<vspace*|<value|subheading-vspace>><with|font-shape|small-caps|<arg|name>>>>>>

  \;

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

  <assign|P|<unicode-paragraphsign>>

  \;

  \;

  <assign|display-paragraph-long|<macro|nr|<if|<unequal|<subsubsection-nr>|0>|<subsubsection-prefix>|<if|<unequal|<subsection-nr>|0>|<subsection-prefix>|<if|<unequal|<section-nr>|0>|<section-prefix>>>><unicode-paragraphsign><space|0.2spc><arg|nr>>>

  <assign|display-paragraph-short|<macro|nr|<unicode-paragraphsign><space|0.2spc><arg|nr>>>

  \;

  <assign|display-paragraph|<macro|nr|<if|<or|<value|display-paragraph-always-use-long-form>|<value|subsubsection-clean-resets-paragraph>>|<display-paragraph-long|<arg|nr>>|<display-paragraph-short|<arg|nr>>>>>

  \;

  <assign|display-subparagraph|<macro|nr|<if|<unequal|<paragraph-nr>|0>|<paragraph-prefix>|<unicode-paragraphsign><space|0.2spc>0.><arg|nr>>>

  \;

  <inactive|<assign|XXdisplay-part|<macro|nr|<number|<arg|nr>|Roman>>>>

  <inactive|<assign|XXdisplay-section|<macro|nr|<if|<sectional-short-style>|<arg|nr>|<chapter-prefix><arg|nr>>>>>

  <inactive|<assign|XXdisplay-section|<macro|nr|<if|<sectional-short-style>|<number|<arg|nr>|Roman>|<chapter-prefix><number|<arg|nr>|Roman>>>>>

  <inactive|<assign|XXdisplay-subsection|<macro|nr|<section-prefix><number|<arg|nr>|Alpha>>>>

  <inactive|<assign|XXdisplay-subsubsection|<macro|nr|<subsection-prefix><arg|nr>>>>

  <inactive|<assign|XXdisplay-paragraph|<macro|nr|<subsubsection-prefix><arg|nr>>>>

  <inactive|<assign|XXdisplay-subparagraph|<macro|nr|<paragraph-prefix><arg|nr>>>>

  \;

  \;

  <assign|part-title|<macro|name|<sectional-centered-bold|<part-vsep*><really-huge|<with|font-shape|small-caps|<arg|name>>><part-vsep>>>>

  <assign|chapter-title|<macro|name|<sectional-centered|<chapter-vsep*><really-huge|<with|font-shape|small-caps|<arg|name>>><chapter-vsep>>>>

  \;

  <assign|section-title|<macro|title|<sectional-centered|<section-vsep*><huge|<with|font-shape|small-caps|<arg|title>>><section-vsep>>>>

  <assign|subsection-title|<macro|title|<sectional-centered|<subsection-vsep*><larger|<with|font-shape|small-caps|<arg|title>>><subsection-vsep>>>>

  <assign|subsubsection-title|<macro|title|<sectional-centered|<subsubsection-vsep*><large|<with|font-shape|small-caps|<arg|title>>><subsubsection-vsep>>>>

  \;

  <inactive|<assign|paragraph-title|<macro|name|<with|dummy|<value|subsubsection-clean-resets-paragraph>|dummy|<value|display-paragraph-always-use-long-form>|<sectional-short-bold|<paragraph-vsep*><arg|name><paragraph-sep>>>>>>

  <assign|paragraph-title|<macro|name|<sectional-short-bold|<paragraph-vsep*><arg|name><paragraph-sep>>>>

  <assign|subparagraph-title|<macro|name|<sectional-short-bold|<subparagraph-vsep*><arg|name><subparagraph-sep>>>>

  <inactive|<assign|XXXzbibliography-heading|<macro|<principal-section|<bibliography-text>>>>>

  <\active*>
    <\src-comment>
      Must redefine these to make sure the numbering resets properly.

      In order to facilitate pin citation of parts of legal documents, it is
      recommended that the court-issued information be used, and that
      documents paragraphs are numbered consequitivly from start of the
      document to the end, and not reset per section, subsection, or
      subsubsection.
    </src-comment>
  </active*>

  <assign|chapter-clean|<macro|<reset-section><section-clean>>>

  <assign|section-clean|<macro|<reset-subsection><subsection-clean>>>

  <assign|subsection-clean|<macro|<reset-subsubsection><subsubsection-clean>>>

  <assign|subsubsection-clean|<macro|<if|<value|subsubsection-clean-resets-paragraph>|<reset-paragraph><paragraph-clean>|>>>

  <assign|paragraph-clean|<macro|<reset-subparagraph><subparagraph-clean>>>

  <\active*>
    <\src-comment>
      Nested enumeration lists for legal documents.
    </src-comment>
  </active*>

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

  <\active*>
    <\src-comment>
      ltoc is for adding a table of contents entry for a nested itemized list
      item. You have to add it manually only where you want it, since it's
      not easy to write a program that can take the first sentence of the
      list item and automatically turn it into a suitable label for the table
      of contents entry. That's different from the section, subsection,
      \<ldots\> since the same string can be the heading in the TOC and the
      heading in-text.
    </src-comment>
  </active*>

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

  <assign|nbspace-extra|<macro|x|<space|0.3333spc><arg|x>>>

  <assign|nbspace-separated|<xmacro|args|<arg|args|0><map-args|nbspace-extra|concat|args|1>>>

  <assign|casenum|<macro|a|b|c|<rigid|<nbspace-separated|<arg|a>|<arg|b>|<arg|c>>>>>

  <\active*>
    <\src-comment>
      <inactive|<use-package|casenick>>
    </src-comment>
  </active*>

  \;

  <\active*>
    <\src-comment>
      This is from std-markup.ts and it shows how I can maybe make the Utah
      Code references into hyperlinks to the web site, with a little work...
    </src-comment>
  </active*>

  <inactive|<assign|xxxhref|<macro|body|<hlink|<with|font-family|tt|language|verbatim|<arg|body>>|<arg|body>>>>>

  <\active*>
    <\src-comment>
      This is from the early development of tm-zotero.ts. It is stale and
      left here only so my old documents still work. I leave it here to show,
      to give people ideas and perhaps so I can use it later, maybe
      integrated with <with|font-series|bold|<with|font-family|tt|zotero-texmacs-integration>>
      somehow, to make clickable links to the statutes?

      The Utah Code is up on a web site managed by the legislature. There is
      a URL scheme they use for the #anchors as well as for the URL itself,
      such that if the program knows the Title, Chapter, Section, ... of the
      statute, it ought to be able to generate the correct hyperlink URL to
      it. The only problem that I <with|font-series|bold|foresee<with|font-series|bold|>>
      (plenty will arise that I do not) is the part of the hyperlink that's
      for choosing which <with|font-series|bold|version<with|font-series|bold|>>
      of the law to reference. They have it set up so that today, in 2016, if
      I cite the statutes, the link should not change if the statute is
      amended, since the link will be to the 2016 version of that statute.
      (Wikipedia does a similar thing, right?)

      So, for the purpose of <with|font-family|tt|tm-zotero.ts>, when someone
      inserts a zcite to a statute, it would be cool if it could use the
      information that is handed back by Juris-M or Zotero to build that
      link. Of course, there will need to be a customized one for each
      jurisdiction, and thus, it can only be enabled when that exists and
      it's not disabled via a per-document option setting. The Juris-M
      modular style sheets use a scheme for automatically finding
      jurisdiction-specific overrides for the modular CSL style sheets.
      Perhaps the same jurisdiction strings or whatever can be used in order
      to form predictable names for the routines that do this?
    </src-comment>
  </active*>

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
    <with-legal-brief-footnote-size|<with|par-sep|<value|legal-brief-footnote-par-sep>|par-line-sep|<value|legal-brief-footnote-par-line-sep>|par-par-sep|<value|legal-brief-footnote-par-par-sep>|par-mode|justify|par-left|0cm|par-right|0cm|font-shape|right|dummy|<value|legal-brief-footnote-size>|dummy|<value|par-fnote-sep>|dummy|<value|page-fnote-sep>|dummy|<value|page-fnote-barlen>|dummy|<value|page-fnote-barsep>|<style-with|src-compact|none|<surround|<locus|<id|<hard-id|<arg|body>>>|<link|hyperlink|<id|<hard-id|<arg|body>>>|<url|<merge|#footnr-|<arg|nr>>>>|<arg|sym>><footnote-sep>|<set-binding|<merge|footnote-|<arg|nr>>|<value|the-label>|body><right-flush>|<style-with|src-compact|none|<arg|body>>>>>>
  </float>>>>
</body>

<\initial>
  <\collection>
    <associate|font|TeX Gyre Termes>
    <associate|page-type|letter>
    <associate|preamble|true>
    <associate|src-style|angular>
  </collection>
</initial>