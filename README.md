# Zotero - TeXmacs integration plugin and citation styles. #

__This branch is the active (Α) development branch of this program. It is ready to use with Juris-M 5.0 now.__

__I need beta testers. Please report both success and failure to this [issue tracker](https://github.com/KarlHegbloom/zotero-texmacs-integration/issues)!__

__In particular, I have not yet tested this with Zotero 5.0, since I work primarily with Juris-M 5.0. Testing this is on my todo list.__

## News ##

  * You must, as always, keep your Juris-M / Zotero updated to the newest release, and stay updated to the latest [propachi-texmacs](https://github.com/KarlHegbloom/propachi-texmacs/releases). This addon installs a modified citeproc-js that (a) defines the ~tmzoterolatex~ output format, since the default, ~RTF~, sent via the editor integration protocol, does not carry the necessary information to make this TeXmacs plugin work right, and mainly,  TeXmacs already can import a LaTeX document, but has no existing translator for RTF; (b) installs a =variableWrapper= function into Juris-M / Zotero that semantically enhances and linkifies citations texts. It also "monkey patches" the editor integration to ensure that the correct output format is sent to TeXmacs. Thus, the [propachi-texmacs](https://github.com/KarlHegbloom/propachi-texmacs/releases) addon is necessary for Juris-M / Zotero to integrate correctly with this TeXmacs plugin.
  * __You no longer need to disable the propachi-texmacs addin when you want to use Juris-M with OpenOffice or Word.__ That is because the setting of the outputFormat is moved into the document prefs, defaulting to RTF when the editor plugin does not specify otherwise. That means it works with both OpenOffice / Word (~RTF~ outputFormat) as well as with the zotero-texmacs-integration (~tmzoterolatex~ outputFormat). You can have both on the screen using Juris-M at the same time if you like.
  * We are working on having installable software bundle packages of TeXmacs for MacOS and Windows that contains the necessary support for this plugin.
  * Key bindings changed: In the `zcite` context, `Tab` now calls `affirmCitation` and `Ctrl-Enter` calls `editCitation`. In the `zbibliography` context, `Tab` and `Ctrl-Enter` both call `editBibliography`.
  * `clipboard-copy`, `clipboard-cut`, and `clipboard-paste` now operate on new `zcite` sub-dividing tags, `zciteLayoutPrefix`, `zciteLayoutDelimiter`, `zciteLayoutSuffix`, and `zsubCite`. So when a `zcite` is disactivated via `Backspace` or the focus toolbar button, you have access to selection of a region for `clipboard-copy` or `clipboard-cut`, and when that region consists only of complete `zsubCite` and any of the `zciteLayout*` tags, then those `zsubCite`'s will get put onto the clipboard wrapped inside of a full `zcite`, ready to be pasted into the document's main text. Any clipping that is of just one `zcite` can be pasted into a disactivated `zcite` as well, as long as the cursor is *between* the `zsubCite` and `zciteLayout*` tags!


## Documentation and YouTube Screencast Demos ##

RTFM: [tm-zotero-tutorial.en.pdf on Github](https://github.com/KarlHegbloom/zotero-texmacs-integration/blob/master/doc/tm-zotero-tutorial.en.pdf)


[Juris-M / Zotero and TeXmacs Integration Screencasts Playlist](https://www.youtube.com/playlist?list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

__NEW__ [Juris-M / Zotero and TeXmacs Integration Screencast 2017 03 08](https://www.youtube.com/watch?v=iQXlESwdYwE&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy&t=195s)

[Juris-M / Zotero and TeXmacs Integration Screencast 2017 02 17](https://www.youtube.com/watch?v=5Fy1Mw0GSKQ&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

[Juris-M / Zotero and TeXmacs Integration Screencast 03](https://www.youtube.com/watch?v=LAjLk7rDGi8&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

[Juris-M / Zotero and TeXmacs Integration Screencast 02](https://www.youtube.com/watch?v=74tzA2OCu4I&index=2&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)

[Juris-M / Zotero and TeXmacs Integration Screencast 01](https://www.youtube.com/watch?v=ZhOton-p3T8&index=1&list=PLN9Ht5SDLPrbPHHyRvTK7bw1awqTsllWy)


## Important Changes for this branch: ##

### To install this, you have to clone this repository and checkout this branch. You do not need to clone the git submodules for a working checkout of this TeXmacs plugin. ###

The reason for the symlink tests is that I've changed some file names and some
module names. If you had symlinks to the source in `~/TeXmacs/plugins` before,
they should be removed, and then only one created named `tm-zotero` that points
to the top directory of a clone of the source from github, *e.g.,*

    cd ~;
    mkdir --parents ~/src/Juris-M || true;
    cd ~/src/Juris-M;
    git clone https://github.com/KarlHegbloom/zotero-texmacs-integration;
    cd zotero-texmacs-integration;
    git checkout master;
    cd ~;
    mkdir --parents ~/.TeXmacs/plugins || true;
    cd ~/.TeXmacs/plugins;
    [[ -L "legal-brief" -a -L "zotero" -a $(realpath "legal-brief") = $(realpath "zotero" ]] && rm legal-brief zotero;
    [[ -L "zotero" ]] && rm zotero;
    ln -s ~/src/Juris-M/zotero-texmacs-integration tm-zotero;

You do not need to clone the submodules unless you are curious or plan to help
develop this program. The are only required for development or bugfix work, not
for normal use of this plugin. Everything that TeXmacs needs to run this plugin
is already in the toplevel git project. The citeproc-js and propachi-texmacs
submodules are the source to the propachi-texmacs XPI that is required to be
installed in Juris-M for this plugin to function properly.


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

      [v5.0.37m10](https://our.law.nagoya-u.ac.jp/download/client/Jurism-5.0.37m10_linux-x86_64.tar.bz2)

  * [Should be optional] Install the OpenOffice plugin via Preferences > Cite > Word Processors.

  * Install the latest propachi-texmacs xpi from:

    https://github.com/KarlHegbloom/propachi-texmacs/releases
    
    Download the XPI file, then from inside of Juris-M, select Tools ->
    Add-ins..., and from there, use the drop-down from the gear icon on the
    upper-right of the add-in's dialog to select "Install add-in from file...",
    find the downloaded XPI, and install it.

    This monkey-patch loads a citeproc into Juris-M that has the right
    outputFormat defined for the TeXmacs integration. It also ensures that the
    integration uses that outputFormat by monkey-patching it so that it
    defaults to RTF as before, but when the document prefs carry an
    outputFormat setting for a particular document (set by the editor plugin),
    it uses that.

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
        git clone https://github.com/KarlHegbloom/zotero-texmacs-integration;
        cd zotero-texmacs-integration;
        git checkout master;
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
