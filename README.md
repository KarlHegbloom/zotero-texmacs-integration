Zotero - TeXmacs integration plugin and citation styles.
========================================================

This is a work in progress, but it is working. I am now using it to
write a document with. You must run it from a git clone for
now. Instructions are below.

Not all of the CSL styles produce good results at this point. In
particular, ones that display URL's are not quite right yet. I will
work on that as time permits. For the time being, my primary focus is
on having this work for jm-indigobook in-text legal citations, with a
formatted bibliography at the end of the document. I need to write a
couple of important lawsuits. I will keep track of the issue tracker,
and try to fix things when requests come it, but be aware that it's
secondary priority... though often a welcome break from the writing.

NEWS
----

  * The name of the branch of my clone of Juris-M / Zotero that you
    need to checkout has changed. I've edited the instructions
    below to reflect that. The branch is called:
    
    karlhegbloom-integration-for-texmacs
  
  * Release 1.6.60 of Better BibTeX for Zotero is out. This contains
    the necessary support for this TeXmacs integration with Zotero.
        
  * The pull-request for my changes to Juris-M are under review.

----------------------------------------------------------------------

Requirements
------------

  * **TeXmacs** <http://www.texmacs.org/>

    * Pre-release snapshot builds: <https://github.com/KarlHegbloom/texmacs/releases>

  * **Juris-M** <http://juris-m.github.io/>

    * Until and unless they accept the modification that makes it
      useable from TeXmacs, you'll need to run a patched copy of
      Juris-M / Zotero. Instructions are below. I will maintain this
      README file, and when they accept the patch, I'll note it
      here. At that point, a new release of Juris-M or Zotero will
      contain the modification that this connector for TeXmacs
      requires.
  
    * You can see exactly what I've modified using the Github commit
      viewer. You'll see that I've not monkeyed with anything that
      might affect the integrity of your references collection.
      
    * I will try to be diligent with regards to keeping this up to
      date relative to ongoing development of Juris-M.
      
  * **Better BibTeX for Zotero** <https://github.com/retorquere/zotero-better-bibtex/wiki/Installation>
  
    * In order for the bbl output format to be defined, you must
      install a recent release of Better BibTeX for Zotero. You'll
      need at least version 1.6.60.
      
      * If you were running from a github checkout, then you can keep
      using it if you like, or:


    cd ~/.mozilla/firefox/*.default/extensions;
    rm better-bibtex@iris-advies.com


    * Now start Firefox and install the new version of Better BibTeX
      for Zotero.

Setup and Options
-----------------

Uninstall or move aside your presently installed Juris-M. First, close
Firefox. Then:

    cd ~/.mozilla/firefox/*.default/extensions
    mkdir ~/saved
    mv juris-m@juris-m.github.io* ~/saved
    echo ~/src/zotero > juris-m@juris-m.github.io
    cd ..
    cp -p prefs.js prefs-SAVED.js
    echo 'user_pref("xpinstall.signatures.required", false);' >> prefs.js
    echo 'user_pref("extensions.zotero.integration.outputFormat", "bbl");' >> prefs.js
    echo 'user_pref("extensions.zotero.integration.maxmaxOffset", 16);' >> prefs.js

Don't start Firefox yet. Next, clone the modified version of Juris-M:

    cd ~
    mkdir src || true
    cd ~/src
    git clone https://github.com/KarlHegbloom/zotero.git
    cd zotero
    git checkout karlhegbloom-integration-for-texmacs


Starting Firefox should give you your normal Juris-M, and you should
notice no changes (unless you try to use the LibreOffice connector
without first setting the `extensions.zotero.integration.outputFormat`
to "rtf"). (Later I will put a configuration panel onto the
configuration dialog. That's slated for a future development
iteration, pending acceptance of my patch to `integration.js`.)

Now clone this repository, and then symlink to it from your TeXmacs
home directory to enable it:

    cd ~/src
    git clone https://github.com/KarlHegbloom/zotero-texmacs-integration.git
    cd ~/.TeXmacs/plugins
    ln -s ~/src/zotero-texmacs-integration zotero

Now when you start TeXmacs, it will find the style and the scheme
program that makes it work. Start a new document, and add the
`tm-zotero` package. A **Zotero** menu will appear on the TeXmacs menu
bar. Try adding a citation. If I've done my job right, and you've
followed the instructions correctly, it ought to work.

Please use the Github issue tracker to report any problems. That will
assist me in not losing any trouble-tickets:

<https://github.com/KarlHegbloom/zotero-texmacs-integration/issues>



References
----------

  * <https://en.wikibooks.org/wiki/LaTeX/Bibliography_Management#Natbib>


Todo
----

  * Setting: Zotero zcite and bibliograph by default in all new documents?
  
  * Any ideas?


Extras
------

This package also contains a drop-in for `bibtex` that performs a
`jsonrpc` call to a running **Juris-M** or **Zotero** with the
**Better BibTeX for Zotero** plugin (extended with new methods for the
purpose; pull request pending). Despite that I developed it with
**TeXmacs** in mind, this utility will be generally useful as a
drop-in replacement for `bibtex` to anyone using **LaTeX**. It parses
a `.aux` file, uses `jsonrpc` to talk to the `schomd` interface of
**Better BibTeX for Zotero** (extended to have a `.bbl` output form),
and writes the `.bbl` file for **TeXmacs** or **LaTeX** to include.

I think that in order to support in-text citation styles and create
the actual in-text citations, that it will need to be able to print
out **BibLaTeX** bbl files and be used with a **BibLaTeX** style, or
else it will need to print out the stuff that **lexitex** needs. I
have not studied that system. I'm working more with **TeXmacs** and so
I will be focused on that more than on a drop-in for `bibtex`.


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
