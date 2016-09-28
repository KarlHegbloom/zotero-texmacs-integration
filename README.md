Zotero - TeXmacs integration plugin and citation styles.
========================================================

NEWS
----

  * 2016-09-28: propachi-texmacs is now a signed xpi, and so you no
    longer need to use about:plugins to set
    xpinstall.signatures.required. You can reset it to its default now
    if you wish (unless you are running it from a git checkout rather
    than by installing the xpi, but if you're running it that way, you
    probably know what you're doing already.)
    
    * It is up-to-date with the latest citeproc-js and Juris-M.

  * 2016-08-04: I think that this will function on Mac OS-X and
    Windows already because it's the LibreOffice Integration plugin
    that opens the TCP port on localhost:23116 which is what
    zotero.scm connects to. The code inside the Firefox LibreOffice
    Integration plugin where it opens that port does not look to see
    what OS it's running on first, so I think it opens that port no
    matter what OS it's on.
    
    * If you run Mac OS-X or Windows, please test this and let me know
      how it works. I have limited time and equipment and require this
      small amount of assistance. Please use the "Issues" tracking
      when you report a successful or failed test. Thanks.
      
      https://github.com/KarlHegbloom/zotero-texmacs-support/issues
      
      
    * When a citation or the bibliography is in-focus, try the
      "wrench" icon. There are a lot of settings available now.

  * 2016-07-30: The first 4 characters of each citation are now a link
    to the bibliography entry. Each bibliography entry has a list of
    pagerefs appended to it, so that you can click to the point in the
    document where the citation appears. The first 4 characters of
    each bibliography entry are a hyperlink to the DOI or URL
    associated with that zotero item.
    
    * To fix up the pagerefs & etc., use the Document->Update... menu.
    
    * It only uses the first 4 characters because the hlink loci do
      not line-break, and so when they are long, they can either mess
      up the paragraph line-breaking, or even stick out into the
      margin. By using a shorter one, it avoids this.
    
    * By the way, it's faster when you don't put a bibliography into
      the document until finishing stages. Just insert citations as
      needed, and when it's close to done, insert a bibliography and
      check it over.
    
TODO
----

  * There should only be a hyperlink to the bibliography entry when
    the document contains a bibliography. So when it has a
    bibliography,

This is a work in progress, but it is working. I am now using it to
write a document with. You can watch this screencast to learn a little
bit about what this is and how to use it:

[Juris-M / Zotero and TeXmacs Integration Screencast 01](https://www.youtube.com/watch?v=4Ssik5qyt5w)

RTFM: [tm-zotero-tutorial.en.pdf on Github](https://github.com/KarlHegbloom/zotero-texmacs-integration/blob/master/doc/tm-zotero-tutorial.en.pdf)

How to get this up and running:
-------------------------------

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
    
    (We need somebody to build OS-X and Windows packages.)

  * Install Juris-M from:
  
    https://juris-m.github.io

    * I've tested this with the Juris-M plugin in Firefox, but I think
      it will work fine using the standalone version.
      
  * Install all of the additional support packages as recommended by
    the Juris-M site. I recommend installing zotfile also.
      
  * Install the propachi-texmacs xpi from:
 
    https://github.com/KarlHegbloom/propachi-texmacs/releases
    
    This monkey-patch loads a citeproc into Juris-M that has the right
    outputFormat defined for the TeXmacs integration. It also ensures
    that the integration uses that outputFormat by monkey-patching
    it. You can disable propachi-texmacs and restart Firefox or
    Juris-M when you want to use the OpenOffice plugin instead of
    TeXmacs... (as if, right?)

    Whenever you update zotero-texmacs-integration, be sure to check
    for an update to the propachi-texmacs also. They are inter-
    dependent.

  * Now clone this repository, and then symlink to it from your
    TeXmacs home directory to enable it. I normally clone it into a
    source code directory and use a symlink from the TeXmacs
    directory:

        cd ~/src;
        git clone https://github.com/KarlHegbloom/zotero-texmacs-integration.git;
        cd ~/.TeXmacs/plugins;
        ln -s ~/src/zotero-texmacs-integration zotero;
    
    You could download a zip from github, but then you won't have the
    easy update functionality you get by using git. To update the code
    when I change it, you run:
    
 
        cd ~/src/zotero-texmacs-integration;
        git pull;
    
    You may also need to update the propachi-texmacs from time to
    time. It does not presently upgrade automatically when I release a
    new version.


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

The jm-indigobook-catsort-bib.csl can be dropped onto Firefox from
Nautilus or added using the Juris-M preferences dialog on the Cite |
Styles tab. I'll add a sample document that uses it after a while.

Please use the Github issue tracker to report any problems. That will
assist me in not losing any trouble-tickets:

<https://github.com/KarlHegbloom/zotero-texmacs-integration/issues>
