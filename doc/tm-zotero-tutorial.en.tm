<TeXmacs|1.99.4>

<style|<tuple|tmdoc|english|tm-zotero>>

<\body>
  <\tmdoc-title>
    <TeXmacs> <math|\<longleftrightarrow\>> Juris-M / Zotero integration.

    <normal-size|Tutorial Introduction>
  </tmdoc-title>

  <section|Initial Setup>

  <\framed>
    During the early stages of development, and for this very early
    stage<math|\<ldots\>> not all of the support it needs is already part of
    Juris-M / Zotero, and so you must follow the instructions given by the
    README on my github page. As things progress, hopefully that will no
    longer be required.

    At this time, it is only working on Linux. I don't have Windows or a Mac
    to develop it on. I will accept pull-requests from anyone willing and
    able to make it work on those platforms.

    Someday this will all be an easily installable package for everyday
    people who don't have a degree or hobby in computer science. For now,
    it's a kit.
  </framed>

  <\enumerate>
    <item>Install Juris-M according to the instructions given above. For now,
    you must use the customized version from my Github repository.

    <item>Install a recent version of <TeXmacs>. There are pre-release builds
    available for Ubuntu via my github mirror of the upstream subversion
    repository:

    <center|<href|https://github.com/KarlHegbloom/texmacs/releases/>>

    With those, there are not guarantees; they are very fresh and sometimes
    crashy. If the latest one does not work right, try a slightly older one.
    When I discover that it's not working, I'll delete them. They won't harm
    your computer or lose your work, but might crash in new less-tested code.
    These are pre-release builds from the trunk of the subversion source code
    repository!

    <item>Install the OpenOffice plugin for Zotero. This is only necessary
    because the integration.js in Juris-M / Zotero checks for the plugin
    being installed. I will try and change this later, after I create a
    configuration GUI overlay to support this <TeXmacs> integration.
    (<with|font-shape|italic|e.g.,> the \Phidden\Q prefs that for now must be
    added to <code*|prefs.js> by hand or via <tt|about:prefs>.

    <\enumerate>
      <item>Be aware that the OpenOffice plugin won't work right with the
      <tt|outputFormat> set to <tt|bbl>, which is what this <TeXmacs>
      integration requires. You can switch it back to the setting that
      OpenOffice requires by changing <tt|*zotero.integration.outputFormat>
      to <tt|rtf>.
    </enumerate>

    <item>Launch Firefox, then click the Juris-M icon to make sure it's
    working properly. Add a few references to your collection if you have
    nothing in it yet. Try visiting Scholar.Google.com and searching for a
    legal case. There will be an icon in the URL bar that you can click to
    add the case to Juris-M.

    <item>Launch TeXmacs, and open a new document. At the left end of the
    toolbar that is just above the document body, it will say \PGeneric\Q.
    You may click that for a menu, to set the document type to anything you
    like. Next click the \P<math|+>\Q and from that menu, pick
    <tmstyle|tm-zotero> to add it's features to your document.

    <item>A <menu|Zotero> menu item will appear in the toplevel menu. You
    will see that this menu is present while viewing this documentation in
    front of you now, since I've added the <tmstyle|tm-zotero> style package
    to it.

    <item>Select \PaddCitation\Q, or type <key|\\zcite> and push <key|Enter>
    to add a new citation to the document. You may abreviate it to <key|\\zc>
    if you prefer. Zotero should open a dialogue for you to set the document
    settings. Choose the citation and bibliography style you want.
    <todo|endnotes don't work yet, but footnotes do.> At any time, you may
    open that document settings again via the <menu|Zotero>
    <math|\<Rightarrow\>> <submenu|Zotero|Set Document Prefs> menu. You'll
    find that when you change the citation style, all of the citations in the
    document change style automatically. They will flip from being in-text to
    footnote citations depending on what style you choose.

    <item>After the document settings are saved, the citation selection
    dialog box will be displayed. Type a substring of the item you're looking
    for to search for it. Click on the various elements to try them. If you
    have the \PIndigo Signals\Q package installed, pressing <key|Ctrl-s>
    while in the \Pprefix\Q box that appears when you click the mouse on a
    citation will cause a menu of citation signals to appear. The text you
    type in those boxes may be styled with simple HTML-like tags, such as
    <verbatim|\<less\>i\<gtr\>italic\<less\>/i\<gtr\>,
    \<less\>b\<gtr\>bold\<less\>/b\<gtr\>, \<less\>sc\<gtr\>small
    caps\<less\>/sc\<gtr\>>. It may also support entry of <LaTeX>, but you
    must follow each open brace with a ! so it won't be escaped away before
    it reaches <TeXmacs>. After you press <key|Enter>, your citation will be
    inserted into the document.

    <item>You may insert a bibliography using either the menu, or the
    <key|\\zbibliography> or <key|\\zb> keyboard shortcuts.

    <item>When the cursor is just to the right of a citation or just inside
    of the \Pzbibliography\Q, you can press the <key|Tab> key to open the
    Zotero citation or bibliography edit dialogues. You will also see that
    there are settings available for each citation when you click the
    \Pwrench\Q icon.\ 

    <item>When you are using a \Pnote\Q citation style, you can tell it to
    make a citation be in-text by clicking the drop-down that will appear
    when the cursor is just inside the right edge of the citation's footnote
    number.

    <item>Pressing <key|Backspace> will deactivate a <key|\\zcite> tag,
    allowing you to hand-edit the text there. Press <key|Enter> to activate
    it again. You'll notice that when you've modified the text of a citation
    from the value that was automatically produced by the citation processor,
    that the flag at the left end of the field turns from green to red. If
    you then activate the \PeditCitation\Q by pressing <key|Tab> or using the
    menu item while the citation field is in focus, Zotero will prompt you,
    telling you that your hand-modifications will be lost if you proceed. It
    is usually better to adjust the contents of your reference database or
    abbrevs than to hand-edit individual citations.

    <item>If you cut and then paste a citation into a new location in the
    document, perhaps moving one from near the end of a paragraph to the
    start of it or whatever<math|\<ldots\>> the formatted citation may no
    longer be correct for it's new position in the document, since when the
    same source is cited twice in a row, most styles will return an
    <with|font-shape|italic|ibib> form. Some have a first-citation form as
    well as a hereinafter form. The <menu|Refresh> menu item will cause any
    moved citations to be updated. A keyboard shortcut for doing that is to
    use <key|Backspace> followed by <key|Enter> to disactivate and then
    reactivate the tag, which will trigger the refresh. The refresh is fairly
    efficient, selectively changing only the few that must be changed.

    <\explain>
      <compound|markup|>
    </explain|>

    <compound|markup|>
  </enumerate>

  <tmdoc-copyright|2016|Karl M.<space|1spc>Hegbloom
  <hlink|<tt|\<less\>karl.hegbloom@gmail.com\<gtr\>>|mailto:karl.hegbloom@gmail.com>>

  <tmdoc-license|Permission is granted to copy, distribute and/or modify this
  document under the terms of the GNU Free Documentation License, Version 1.1
  or any later version published by the Free Software Foundation; with no
  Invariant Sections, with no Front-Cover Texts, and with no Back-Cover
  Texts. A copy of the license is included in the section entitled "GNU Free
  Documentation License".>
</body>

<\initial>
  <\collection>
    <associate|preamble|false>
    <associate|section-display-numbers|false>
  </collection>
</initial>