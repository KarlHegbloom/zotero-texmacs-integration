Zotero - TeXmacs integration plugin and citation styles.
========================================================

This is a work in progress. The README is not up to date. I'll fix it
later. I need to take about a week or two off from work on this
because I need to prepare for a child custody trial. When that's done
I'll be getting back to this while he's in school. I need the citation
support for the document I'll be working on after the trial is
over... so don't worry, I will finish what I started.

This package contains a drop-in for `bibtex` that performs a `jsonrpc`
call to a running **Juris-M** or **Zotero** with the **Better BibTeX
for Zotero** plugin (extended with new methods for the purpose; pull
request pending). Despite that I developed it with **TeXmacs** in
mind, this utility will be generally useful as a drop-in replacement
for `bibtex` to anyone using **LaTeX**. It parses a `.aux` file, uses
`jsonrpc` to talk to the `schomd` interface of **Better BibTeX for
Zotero** (extended to have a `.bbl` output form), and writes the
`.bbl` file for **TeXmacs** or **LaTeX** to include.

There is also a style file for citation macros that calls on the
**Better BibTeX for Zotero** plugin's `cayw` (*cite as you write*)
`jsonrpc` interface to pop-up the citation chooser, the way it does
when writing using `libreoffice` and it's `zotero` integration plugin,
or when using the `markdown` support for the **Atom**
editor. Initially, there is only, for sure, support for the
`jm-babyblue` CSL style, but while writing it, I'll keep in mind the
potential for use with and and all CSL styles supported by **Juris-M**
and **Zotero**. As I am in sort-of a hurry to get this working due to
court deadlines, the next iteration may not happen for several months.

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

Ideas
-----

What if the bibliography was an included tmfs:// subdocument? Can each
and every citation in the document be a document subtree of that
bibliography subdocument, sort of "showing through" at each location
where a citation happens, and then the actual bibliography being
another subtree? So there would be a tmfs handler where the document
includes the bibliography... I think for the citations it's simpler to
have them be stored in a hidden aux data section as tuples.

Can there be a listener that watches a .bib file or for connections
coming from the reference manager, to automatically keep them all
updated? i think yes, but not until guile-2 is part of TeXmacs, for a
separate thread and faster execution.

For citations within a single document, I don't expect that there's
ever going to be a need for more than one citation style per
document. But some documents may want to have all of the citations in
one big bibliography, and others might want a separate one for
caselaw, perhaps broken down by jurisdiction according to some
configurable grouping, and then journal articles, then textbooks,
etc. The McGill style already sorts the bibliography according to
that. I don't know (as of this writing) if the CSL citeproc produces
section headings or anything for that.
