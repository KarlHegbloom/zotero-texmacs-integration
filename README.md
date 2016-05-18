Zotero - TeXmacs integration plugin and citation styles.
========================================================

This is a work in progress. The README is not up to date. I'll fix it
later... Some of the notes are very naive. I'm leaving them there for
now.

-------------------------------------------------------------------------------

This package contains a drop-in for `bibtex` that performs a `jsonrpc`
call to a running **Juris-M** or **Zotero** with the **Better BibTeX
for Zotero** plugin (extended with new methods for the purpose; pull
request pending). Despite that I developed it with **TeXmacs** in
mind, this utility will be generally useful as a drop-in replacement
for `bibtex` to anyone using **LaTeX**. It parses a `.aux` file, uses
`jsonrpc` to talk to the `schomd` interface of **Better BibTeX for
Zotero** (extended to have a `.bbl` output form), and writes the
`.bbl` file for **TeXmacs** or **LaTeX** to include.

I think that in order to support in-text citation styles and create
the actual in-text citations, that it will need to be able to print
out **BibLaTeX** bbl files and be used with a **BibLaTeX** style, or
else it will need to print out the stuff that **lexitex** needs. I
have not studied that system. I'm working more with **TeXmacs** and so
I will be focused on that more than on a drop-in for `bibtex`.


Requirements
------------

  * **TeXmacs** <http://www.texmacs.org/>
  * **Juris-M** <http://juris-m.github.io/>
  * **Guile** v2.0 <http://www.gnu.org/software/guile/>

**TeXmacs** is presently (2016/03) buildable only with guile
1.8. Guile 2 support is only partly complete, and on hold for the time
being. The drop-in replacement for `bibtex` in this package requires
Guile 2, for it's web client module, and:

  * **guile-json** <https://github.com/aconchillo/guile-json>

There may be other dependencies not yet listed here. We tend to
develop code faster than documentation, right?

References
----------

  * <https://en.wikibooks.org/wiki/LaTeX/Bibliography_Management#Natbib>


Todo
----

  * Setting: Zotero zcite and bibliograph by default in all new documents?
  
  * 


Ideas
-----

*Haec verba* to case law citation in document tracking, so like when I
reference a piece of caselaw, maybe highlight the citation and text
around it, and execute a menu function, and have it put that as a note
into zotero with this document's tag associated with it?

In general, perhaps each document ought to have an entry in Zotero for
the purpose of associating it with everything cited by it, for both
semantic / searching and for reference / citation usage mapping?

**Semantic Web** of documents? Explore: semantic mediawiki, org-mode,
etc.

For citations within a single document, I don't expect that there's
ever going to be a need for more than one citation style per
document. But some documents may want to have all of the citations in
one big bibliography, and others might want a separate one for
caselaw, perhaps broken down by jurisdiction according to some
configurable grouping, and then journal articles, then textbooks,
etc. The McGill style already sorts the bibliography according to
that. I don't know (as of this writing) if the CSL citeproc produces
sub-section headings or anything for that... So **support for multiple
bibliographies or for classification-grouped ones**; per-chapter,
per-document, footnote, endnote...
