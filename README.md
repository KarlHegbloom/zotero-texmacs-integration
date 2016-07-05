Zotero - TeXmacs integration plugin and citation styles.
========================================================

This is a work in progress, but it is working. I am now using it to
write a document with.

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
      
  * Use Firefox "about:config" to set: xpinstall.signatures.required
    to false. Alternatively, with Firefox not running, you can run:

        echo 'user_pref("xpinstall.signatures.required", false);' >> prefs.js
    
    Make sure you use two ">" there, to append to the file, or you'll
    blow away your prefs.js. I recommend using about:config. The
    reason for this is that this next part is not signed since I don't
    know how to do that yet...

  * Install the propachi-texmacs xpi from:
 
    https://github.com/KarlHegbloom/propachi-texmacs/releases
    
    This monkey-patch loads a citeproc into Juris-M that has the right
    outputFormat defined for the TeXmacs integration. It also ensures
    that the integration uses that outputFormat by monkey-patching
    it. You can disable propachi-texmacs and restart Firefox or
    Juris-M when you want to use the OpenOffice plugin instead of
    TeXmacs... (as if, right?)

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
