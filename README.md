# Zotero - TeXmacs integration plugin and citation styles. #

__This branch is the active (Α) development branch of this program.__

__It is ready to try! I need alpha testers. Please report!__

## Documentation and YouTube Screencast Demos ##

RTFM: [tm-zotero-tutorial.en.pdf on Github](https://github.com/KarlHegbloom/zotero-texmacs-integration/blob/performance-improvments-wip/doc/tm-zotero-tutorial.en.pdf)

[Juris-M / Zotero and TeXmacs Integration Screencasts Playlist](https://www.youtube.com/playlist?list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

__NEW__ [Juris-M / Zotero and TeXmacs Integration Screencast 2017 02 17](https://www.youtube.com/watch?v=5Fy1Mw0GSKQ&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

[Juris-M / Zotero and TeXmacs Integration Screencast 03](https://www.youtube.com/watch?v=LAjLk7rDGi8&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

[Juris-M / Zotero and TeXmacs Integration Screencast 02](https://www.youtube.com/watch?v=74tzA2OCu4I&index=2&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

[Juris-M / Zotero and TeXmacs Integration Screencast 01](https://www.youtube.com/watch?v=ZhOton-p3T8&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)


## Important Changes for this branch: ##

### To install this, you have to clone this repository and checkout this branch. ###

The reason for the symlink tests is that I've changed some file names and some
module names. If you had symlinks to the source in `~/TeXmacs/plugins` before,
they should be removed, and then only one created named `tm-zotero` that points
to the top directory of a clone of the source from github, *e.g.,*

    cd ~;
    mkdir --parents ~/src/Juris-M || true;
    cd ~/src/Juris-M;
    git clone --recursive https://github.com/KarlHegbloom/zotero-texmacs-integration;
    cd ~;
    mkdir --parents ~/.TeXmacs/plugins || true;
    cd ~/.TeXmacs/plugins;
    [[ -L "legal-brief" -a -L "zotero" -a $(realpath "legal-brief") = $(realpath "zotero" ]] && rm legal-brief zotero;
    [[ -L "zotero" ]] && rm zotero;
    ln -s ~/src/Juris-M/zotero-texmacs-integration tm-zotero;


## How to get this up and running: ##

  * Install a recent (development snapshot of) TeXmacs. This program
    is untested with older versions of TeXmacs. If you can not build
    your own copy or use the Ubuntu deb package of TeXmacs that I have
    available, you'll have to use whatever version is available to
    you... let me know if it works alright. I think it will be
    compatible with the last official release version of TeXmacs, as
    long as it's built with Guile 1.8. I doubt it will work with older
    versions of Guile.

    https://github.com/KarlHegbloom/texmacs/releases

    If you have trouble with one build, try an older one. I'll try and
    keep them fresh and remove ones that are not working right. They
    are reasonably stable, but under development right now.

  * Install Juris-M from:

    https://juris-m.github.io

    * This works with Juris-M standalone. The last tested version is

      [v4.0.29.12m98](https://github.com/Juris-M/zotero-standalone-build/releases/download/v4.0.29.12m98/jurism-for-linux-64bit-4.0.29.12m98.tar.bz2)

  * Install the OpenOffice plugin via Preferences > Cite > Word Processors.

  * Install the propachi-texmacs xpi from:

    https://github.com/KarlHegbloom/propachi-texmacs/releases

    This monkey-patch loads a citeproc into Juris-M that has the right
    outputFormat defined for the TeXmacs integration. It also ensures that the
    integration uses that outputFormat by monkey-patching it. You can disable
    propachi-texmacs and restart Juris-M when you want to use the OpenOffice
    plugin instead of TeXmacs... (as if, right?)

    Check there for updates or "watch" the github repository to get email when
    I update it. I will try to get automatic updates to function when I have
    time for that.

  * Now clone this repository, and then symlink to it from your
    TeXmacs home directory to enable it. I normally clone it into a
    source code directory and use a symlink from the TeXmacs
    directory:

        cd ~;
        mkdir --parents ~/src/Juris-M || true;
        cd ~/src/Juris-M;
        git clone --recursive https://github.com/KarlHegbloom/zotero-texmacs-integration;
        cd ~;
        mkdir --parents ~/.TeXmacs/plugins || true;
        cd ~/.TeXmacs/plugins;
        [[ -L "legal-brief" -a -L "zotero" -a $(realpath "legal-brief") = $(realpath "zotero" ]] && rm legal-brief zotero;
        [[ -L "zotero" ]] && rm zotero;
        ln -s ~/src/Juris-M/zotero-texmacs-integration tm-zotero;

    You could download a zip from github, but then you won't have the
    easy update functionality you get by using git. To update the code
    when I change it, you run:

        cd ~/src/Juris-M/zotero-texmacs-integration;
        git checkout master;
        git pull;

Now when you start TeXmacs, it will be able to find the style and the
scheme program that makes it work. Start a new document, and add the
`tm-zotero` style package. (Document menu, or bottom left toolbar
icons.) A **Zotero** menu will appear on the TeXmacs menu bar. Try
adding a citation. If I've done my job right, and you've followed the
instructions correctly, it ought to work.

I've also included my `legal-brief` style for people to try. Open a
new document, and set the main document style to that. Then try the
hybrid LaTeX-like commands:

    \Legal-Heading

    or

    \Cert-of-Service

The `jm-indigobook-catsort-bib.csl` can be added using the Juris-M preferences
dialog on the `Cite | Styles` tab.

Please use the Github issue tracker to report any problems. That will
assist me in not losing any trouble-tickets:

https://github.com/KarlHegbloom/zotero-texmacs-integration/issues


## Other potentially interesting media ##

*Yes, after it's all working again, I'll update the documentation!*

This article is very interesting. It talks about verified mathematical
documents... it makes me wonder if there can be verified legal documents?

http://www.sciencedirect.com/science/article/pii/S1571066107001727
