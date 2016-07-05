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

  <\itemize-arrow>
    <item>Install the Juris-M xpi plugin for Firefox or Juris-M stand-alone,
    according to the instructions given in the README on my github
    repository:

    <center|<slink|https://github.com/KarlHegbloom/zotero-texmacs-integration>>

    Juris-M is available via:

    <center|<slink|http://juris-m.github.io>>

    Be sure to also install the <tt|abbrevs-filter> and other add-ons, such
    as <tt|zotfile>.

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

    <item>Install my <tt|propachi-texmacs> plugin for Firefox or stand-alone
    Juris-M:

    <center|<slink|https://github.com/KarlHegbloom/propachi-texmacs/releases/>>

    It will not auto-update, and from time to time, you may need to update it
    by-hand. Because it is not signed, you must use <tt|about:config> in
    Firefox to set <tt|xpinstall.signatures.required> to <tt|false> in order
    for it to work. Someday perhaps it will be signed, or it's functionality
    rolled into upstream Juris-M / Zotero.

    <item>Install the OpenOffice plugin for Juris-M / Zotero. This is only
    necessary because the integration.js in Juris-M / Zotero checks for the
    plugin being installed. I will try and change this later, after I create
    a configuration GUI overlay to support this <TeXmacs> integration.

    <\enumerate>
      <item>Be aware that the OpenOffice plugin won't work right with the
      <tt|outputFormat> set to <tt|bbl>, which is what this <TeXmacs>
      integration requires. You can switch it back to the setting that
      OpenOffice requires by changing <tt|*zotero.integration.outputFormat>
      to <tt|rtf>, or by disabling <tt|propachi-texmacs>.
    </enumerate>

    <item>Clone the git repository of <tt|zotero-texmacs-integration> and
    symlink it into your <tt|$TEX<no-break>MACS_HOME> like this:

    <\shell-code>
      mkdir ~/src

      cd ~/src

      git clone https://github.com/KarlHegbloom/zotero-texmacs-integration.git

      mkdir ~/.TeXmacs/plugins \|\| true

      cd ~/.TeXmacs/plugins

      ln -s ~/src/zotero-texmacs-integration zotero
    </shell-code>

    <item>Launch Firefox, then click the Juris-M icon to make sure it's
    working properly. Add a few references to your collection if you have
    nothing in it yet. Try visiting Scholar.Google.com and searching for a
    legal case. There will be an icon in the URL bar that you can click to
    add the case to Juris-M.

    <item>Launch TeXmacs, and open a new document. At the left end of the
    toolbar that is just above the document body, it will say \PGeneric\Q.
    You may click that for a menu, to set the document type to anything you
    like. Next click the \P<math|+>\Q and from that menu, pick
    <tmstyle|tm-zotero> to add it's features to your document. You can also
    choose the <tmstyle|legal-brief> document style if you wish.

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
    footnote citations depending on what style you choose, and the
    bibliography will also be automatically updated.

    <zcite|+WoBHoEj0Jm92ki|<#4954454D2043534C5F4349544154494F4E207B226369746174696F6E4944223A226C50436234516453222C2270726F70657274696573223A7B22666F726D61747465644369746174696F6E223A227B5C5C727466205C5C7465787469747B4164616D7320762E2053746174657D2C20323030352055542036322C2031323320502E3364203430302028303223405574616855746168582D582D5820303123405375702E2043742E5375702E2043742E582D582D582032303035292E7D222C22706C61696E4369746174696F6E223A2228636F6E636174202877697468205C22666F6E742D73686170655C22205C226974616C69635C222028636F6E636174205C224164616D73205C22202861626272205C22762E5C2229205C222053746174655C222929205C222C20323030352055542036322C2031323320502E336420343030202832303035292E5C2229227D2C226369746174696F6E4974656D73223A5B7B226964223A333138372C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224164616D7320762E205374617465222C22636F6E7461696E65722D7469746C65223A225554222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A223632222C22766F6C756D65223A2232303035222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D3133363639323631323637333335383433303630222C226E6F7465223A226D6C7A73796E63313A303035347B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A75745C227D7D222C22697373756564223A7B22726177223A2253657074656D6265722032332C2032303035227D7D7D2C7B226964223A333536342C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224164616D7320762E205374617465222C22636F6E7461696E65722D7469746C65223A22502E203364222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A22343030222C22766F6C756D65223A22313233222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D3133363639323631323637333335383433303630222C226E6F7465223A226D6C7A73796E63313A303035347B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A75745C227D7D7B3A6A7572697364696374696F6E3A20557461687D222C22697373756564223A7B22726177223A2253657074656D6265722032332C2032303035227D7D7D5D2C22736368656D61223A2268747470733A2F2F6769746875622E636F6D2F6369746174696F6E2D7374796C652D6C616E67756167652F736368656D612F7261772F6D61737465722F63736C2D6369746174696F6E2E6A736F6E227D>|<with|font-shape|italic|Adams
    <abbr|v.> State>, 2005 UT 62, 123 P.3d 400 (2005).>

    Notice that the above is a <em|parallel citation>. There are two entries
    in the Juris-M reference database for the same case. I cite one then the
    other immediately after it in the same reference cluster, and it outputs
    as a parallel citation.

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
    moved citations as well as the bibliography to be updated. A keyboard
    shortcut for doing that is to use <key|Backspace> followed by <key|Enter>
    to disactivate and then reactivate the tag, which will trigger the
    refresh. The refresh is fairly efficient, selectively changing only the
    few that must be changed.

    <item>When the citation dialog is open, there is an <menu|Abbrevs> button
    available that will open up the abbreviation editor. There are some
    special abbreviations and field name prefixes that can be applied when
    using Juris-M / Zotero with <TeXmacs>.

    <\itemize>
      <item><verbatim|!here\<gtr\>\<gtr\>\<gtr\>><space|1tab>in any Juris-M /
      Zotero reference database field will cause that field to be ommitted
      from citations and bibliographies. This is an internal part of
      <tt|citeproc.js>.

      <item><verbatim|X-X-X><space|1tab>as an abbrev will cause the thing
      abbreviated and any trailing space to be removed. If empty parentheses
      are left behind, they will be removed as well.

      <item><verbatim|Sup. Ct.Sup.Ct.X-X-X><space|1tab>any string repeated
      twice and followed by <tt|X-X-X> will be removed along with any
      trailing space. Use this when the result of the abbreviation must be
      used to determine the item's sort-order within the bibliography, but
      you don't want it to appear in the final output. If empty parentheses
      are left behind, they will be removed as well.

      <item><tt|00#@><space|1tab>is a sorting prefix. It consists of two
      numeric digits, followed by <tt|#@>. The <tt|zotero.scm> program inside
      of <TeXmacs> will remove it prior to display, but Juris-M / Zotero will
      have used it during the bibliography sorting process. This hack was
      designed to make it possible to sort \P<abbr|Sup.> <abbr|Ct.>\Q legal
      cases ahead of \P<abbr|Ct.> of <abbr|App.>\Q legal cases in a
      category-sorted bibliography. This sorting-prefix may be combined with
      any of the other abbreviations except for
      <verbatim|!here\<gtr\>\<gtr\>\<gtr\>>, since it's internal to
      <tt|citeproc.js> rather than to <tt|zotero.scm>.

      So, for example, to sort Supreme Court cases ahead of Court of Appeals
      cases, but not have \P<abbr|Sup.> <abbr|Ct.>\Q or \P<abbr|Ct.> of
      <abbr|App.>\Q appear in the output, I define abbreviations:
      <verbatim|Supreme Court> <math|\<Rightarrow\>> <verbatim|01#@Sup.
      Ct.Sup. Ct.X-X-X> and <verbatim|Court of Appeals> <math|\<Rightarrow\>>
      <verbatim|04#@Ct. of App.Ct. of App.X-X-X> and now both that prefix
      number as well as the \PSup. Ct.\Q and \PCt. of App.\Q strings are part
      of what <tt|citeproc.js> uses internally when sorting the items, but
      they don't appear in the displayed result within your document.

      With the <tt|jm-indigobook-catsort-bib.csl> style, combined with an
      abbreviation like <verbatim|United States\|US> <math|\<Rightarrow\>>
      <verbatim|01#@USUSX-X-X> or <verbatim|US\|Utah> <math|\<Rightarrow\>>
      <verbatim|02#@UtahUtahX-X-X>, a citation to a United States Supreme
      Court case will display only the year inside of the parentheses, rather
      than <with|font-shape|italic|e.g.,> \P(US <abbr|Sup.> <abbr|Ct.>
      2016)\Q or \P(Utah <abbr|Sup.> <abbr|Ct.> 2016)\Q, yet the categorized
      table of authorities will have tossed those into the correct bins
      according to the abbreviated jurisdictions user-defined sort-order\Vin
      this example, placing United States federal cases ahead of Utah state
      cases.

      <item><tt|000000000@#\\ztbibSubHeading{!Title of Subheading
      Here}><space|1tab>is for creating a category subheading in your
      bibliography. It is used in the <tt|title> field in the Juris-M /
      Zotero database. The item type should be set to an appropriate type to
      match one of the item types for the particular category. For legal
      cases, you must also set the jurisdiction and court in order to make
      the subheading appear in the right part of the bibliography.

      Notice the <tt|!> which preserves the preceding <tt|{> which would
      otherwise be \Pescaped\Q before being sent to <TeXmacs> in order to
      <em|prevent> it from being parsed as a <LaTeX> syntactic element, for
      the braces that surround the argument to a macro. Normally we want
      certain characters to be escaped, so we can write them as we please
      into bibliography annotations or titles, but sometimes we might want to
      pass <LaTeX> through to <TeXmacs> in this fashion to achieve special
      effects.

      The <tt|ztbibSubHeading> macro is defined in the <tt|legal-brief.ts>
      style. You may visit that file to see what it looks like, and you may
      redefine it in your document preamble if you like. There are other
      \Psettings\Q sort of macros to be found in there that can also be
      overrided in your document preamble when you need that.

      <item>Those will work with the included
      <tt|jm-indigobook-catsort-bib.csl>, which must be installed into
      Juris-M / Zotero via it's configuration dialog, and then selected as
      the document's bibliography type. If you look inside of that file, way
      down at the bottom you will find the <tt|bibliography> CSL macro, and
      at the top of that macro, you'll find the <tt|sort> definition, which
      has a <tt|key> calling a macro that categorizes the bibliographic
      items. The same sort of sorting prefixes and subheadings can of course
      be defined for your own modified versions of other CSL styles. It is
      not difficult to define a categorized sorting macro for any CSL style
      you desire to utilize.

      <item>To insert the subheadings into your bibliography, use
      <menu|Zotero> <math|\<Rightarrow\>> <submenu|Zotero|Edit Bibliography>,
      and select the dummy reference database items that have their titles
      defined as shown. They should be inserted into the correct place in the
      bibliography.

      <item>You can see what I'm talking about by watching the screencast:
      <ztHref|https://www.youtube.com/watch?v=4Ssik5qyt5w|https://www.youtube.com/watch?v=4Ssik5qyt5w><math|\<ldots\>>
      and by opening the sample document found in the
      <tt|~/src/zotero-texmacs-integration> directory.
    </itemize>

    <compound|markup|>
  </itemize-arrow>

  Notice that in the following bibliography, the tossing of the items into
  categories is not perfect. You must watch for that, and figure out how to
  tweak the abbrevs or reference database entries to make it work. This
  method of creating a categorized bibliography is imperfect; it's kind of a
  \Phack\Q. With a little munging, they usually will land where they belong.
  I've found that sometimes changing the document's CSL style from one to
  another, then back again, will cause them to be sorted into the correct
  place the second time. I don't know why it does that; maybe it was just too
  late at night, right?

  <todo|When I tried to use \PeditBibliography\Q on this, I get a stack
  overflow: <verbatim|scm-\<gtr\>json-string>.>

  <\zbibliography|+WoBHoEj0Jm92kh|<#4249424C207B22756E6369746564223A5B5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F455A514637494350225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F584A484643444E50225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F45344837394D5432225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5747335243515043225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4733525535423755225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F46584A5A334B5144225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3537424E5A423947225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5349575833345232225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A545A355A465648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F47374E55434B5655225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F32395044374E5647225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F324E565746333251225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4851353645463637225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5732415038575254225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5835354E47495745225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F42374B5041583842225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5432324454333844225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F39465855524A355A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4641524544544555225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4941524A5433454B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F324D483747354B50225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4252433852454753225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F41474246384B5A47225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5749354155565151225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F52505A3742374843225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F39414E4235464135225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3657534E47354A48225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3855485134495243225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3239524A47495143225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F414D5A37484E5534225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F43354144324E3833225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F474857555134565A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D3736544A414546225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56364A5647495547225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4542464A33545834225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F365557324E365356225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F48545A5634514E35225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3458324835374442225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F32525249434E5841225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F54494A4B35504749225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F39424B4158354147225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F444E335848544454225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524A464E4E364D50225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F50574A4E32374937225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3850394338534148225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A46583443463742225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F35575632345A5A44225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5548463755354E39225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F49535A32544B434A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F514B413755564D46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3942524D45364246225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F44494B39374B465A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F58434A4857533841225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3254365056453445225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F37553538444D5645225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33523844475A3453225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F574B47335554454D225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4239344144504E36225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4143394237354D52225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5053464932425A4D225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5254585A56375145225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F36585A3349514D44225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F41525A5734524638225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F473843564D433656225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F53514A4957454242225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5436413446494854225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A36355050354843225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5556543651445455225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4741585038325A38225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4354445434465139225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F434B565543335256225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3532445A4D564944225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4632384152455356225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D454B354D585333225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F50555434584D4432225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5232344B57363333225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F57495A58495A4258225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F574D564E465A4956225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F44434E4B374B5751225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E4A383843503853225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F42414A564D463552225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F58564D4B45535047225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F51505A484E4D3233225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5234365A47423450225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3649325356575653225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4458374636573343225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F424D4D3349505345225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5133393242435442225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F394B38384A45444B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4857584745543245225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4333523937563436225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3638414B505A4B44225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3541555045504D53225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B48444849524E45225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5037453952525237225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F374341354B374332225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524753555450414A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56553452494D4238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F484B4D3548495238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E385339554D4453225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F52483444484E3436225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3849425341534B46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3753453537465033225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4447384945325046225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F584B465547474E52225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F333856454A335850225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D515A4A50475235225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3658473343504352225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F49344E5039494A56225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3535554A42334E58225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5443534D344E3648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F46373333394B4544225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3752434B52384947225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3532325356345353225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3350343842354D43225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5056415452443748225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4546374E5036484A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F414856354E455057225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524133374B455657225D5D2C22637573746F6D223A5B5D7D2043534C5F4249424C494F475241504859>>
    <ztbibSubHeading|United States Code>\ 

    <ztbibItemText|<zbibCitationItemID|3434><ztbibitem|sys<rsub|i>d<rsub|3>434><with|font-shape|italic|Definition
    of ``scheme or artifice to defraud,''> 18 <abbr|<abbr|U.>S.C.>>

    <ztbibSubHeading|Constitution of Utah>\ 

    <ztbibItemText|<zbibCitationItemID|3474><ztbibitem|sys<rsub|i>d<rsub|3>474><with|font-shape|italic|Utah
    Const, <abbr|Art.> I, <SectionSignGlyph|>11>,
    <with|font-shape|small-caps|Open courts provision>>

    <ztbibItemText|<zbibCitationItemID|374><ztbibitem|sys<rsub|i>d<rsub|3>74><with|font-shape|italic|Utah
    Const, <abbr|Art.> I, <SectionSignGlyph|>12>,
    <with|font-shape|small-caps|Rights of accused persons>>

    <ztbibItemText|<zbibCitationItemID|3471><ztbibitem|sys<rsub|i>d<rsub|3>471><with|font-shape|italic|Utah
    Const, <abbr|Art.> I, <SectionSignGlyph|>24>,
    <with|font-shape|small-caps|Uniform operation of law>>

    <ztbibItemText|<zbibCitationItemID|3469><ztbibitem|sys<rsub|i>d<rsub|3>469><with|font-shape|italic|Utah
    Const, <abbr|Art.> I, <SectionSignGlyph|>27>,
    <with|font-shape|small-caps|Frequent recurrence to fundamental
    principles>>

    <ztbibSubHeading|Utah Code>\ 

    <ztbibItemText|<zbibCitationItemID|3429><ztbibitem|sys<rsub|i>d<rsub|3>429><with|font-shape|italic|Utah
    Code <SectionSignGlyph|>68-3-2: Statutes in derogation of common law not
    strictly construed <emdash> Rules of equity prevail.> (2014).>

    <ztbibItemText|<zbibCitationItemID|3427><ztbibitem|sys<rsub|i>d<rsub|3>427><with|font-shape|italic|Utah
    Code <SectionSignGlyph|>68-3: Construction Statutes> (1953).>

    <ztbibItemText|<zbibCitationItemID|3457><ztbibitem|sys<rsub|i>d<rsub|3>457><with|font-shape|italic|Utah
    Code <SectionSignGlyph|>77-7-3: Arrest by private persons> (1980).>

    <ztbibItemText|<zbibCitationItemID|3455><ztbibitem|sys<rsub|i>d<rsub|3>455><with|font-shape|italic|Utah
    Code <SectionSignGlyph|>77-7-23: Delivery of prisoner arrested without
    warrant to magistrate <emdash> Transfer to court with jurisdiction
    <emdash> Violation as misdemeanor> (1997).>

    <ztbibItemText|<zbibCitationItemID|3459><ztbibitem|sys<rsub|i>d<rsub|3>459><with|font-shape|italic|Utah
    Code <SectionSignGlyph|>77-36-2.6: Appearance of defendant required
    <emdash> Determinations by court <emdash> Pretrial protective order>
    (2010).>

    <ztbibSubHeading|Utah Rules of Criminal Procedure>\ 

    <ztbibItemText|<zbibCitationItemID|3461><ztbibitem|sys<rsub|i>d<rsub|3>461><with|font-shape|italic|URCrP
    7>, <with|font-shape|italic|Proceedings before a magistrate> (2014).>

    <ztbibSubHeading|Utah Rules of Appellate Procedure>\ 

    <ztbibItemText|<zbibCitationItemID|3421><ztbibitem|sys<rsub|i>d<rsub|3>421><with|font-shape|italic|URAP
    3>, <with|font-shape|italic|Appeal as of right: how taken>>

    <ztbibItemText|<zbibCitationItemID|3440><ztbibitem|sys<rsub|i>d<rsub|3>440><with|font-shape|italic|URAP
    4>, <with|font-shape|italic|Appeal as of right: when taken>>

    <ztbibItemText|<zbibCitationItemID|3484><ztbibitem|sys<rsub|i>d<rsub|3>484><with|font-shape|italic|URAP
    5>, <with|font-shape|italic|Discretionary appeals of interlocutory
    orders>>

    <ztbibItemText|<zbibCitationItemID|3482><ztbibitem|sys<rsub|i>d<rsub|3>482><with|font-shape|italic|URAP
    11>, <with|font-shape|italic|The record on appeal>>

    <ztbibItemText|<zbibCitationItemID|3486><ztbibitem|sys<rsub|i>d<rsub|3>486><with|font-shape|italic|URAP
    19>, <with|font-shape|italic|Extraordinary writs> (2015).>

    <ztbibSubHeading|United States Supreme Court Cases>\ 

    <ztbibItemText|<zbibCitationItemID|2075><ztbibitem|sys<rsub|i>d<rsub|2>075><with|font-shape|italic|Addington
    <abbr|v.> Texas>, 441 US 418 (1979).>

    <ztbibItemText|<zbibCitationItemID|2021><ztbibitem|sys<rsub|i>d<rsub|2>021><with|font-shape|italic|Albright
    <abbr|v.> Oliver>, 510 US 266 (1994).>

    <ztbibItemText|<zbibCitationItemID|1252><ztbibitem|sys<rsub|i>d<rsub|1>252><with|font-shape|italic|Blakely
    <abbr|v.> Washington>, 542 US 296 (2004).>

    <ztbibItemText|<zbibCitationItemID|2275><ztbibitem|sys<rsub|i>d<rsub|2>275><with|font-shape|italic|Brady
    <abbr|v.> Maryland>, 373 US 83 (1963).>

    <ztbibItemText|<zbibCitationItemID|211><ztbibitem|sys<rsub|i>d<rsub|2>11><with|font-shape|italic|DeShaney
    <abbr|v.> Winnebago County Dept. of Social Servs.>, 489 US 189 (1989).>

    <ztbibItemText|<zbibCitationItemID|2085><ztbibitem|sys<rsub|i>d<rsub|2>085><with|font-shape|italic|Foucha
    <abbr|v.> Louisiana>, 504 US 71 (1992).>

    <ztbibItemText|<zbibCitationItemID|1170><ztbibitem|sys<rsub|i>d<rsub|1>170><with|font-shape|italic|Friends
    of Earth, <abbr|Inc.> <abbr|v.> Laidlaw Environmental Services (TOC),
    <abbr|Inc.>>, 528 US 167 (2000).>

    <ztbibItemText|<zbibCitationItemID|1579><ztbibitem|sys<rsub|i>d<rsub|1>579><with|font-shape|italic|Gerstein
    <abbr|v.> Pugh>, 420 US 103 (1975).>

    <ztbibItemText|<zbibCitationItemID|2309><ztbibitem|sys<rsub|i>d<rsub|2>309><with|font-shape|italic|Giglio
    <abbr|v.> United States>, 405 US 150 (1972).>

    <ztbibItemText|<zbibCitationItemID|3555><ztbibitem|sys<rsub|i>d<rsub|3>555><with|font-shape|italic|Griffin
    <abbr|v.> California>, 380 US 609 (1965).>

    <ztbibItemText|<zbibCitationItemID|1671><ztbibitem|sys<rsub|i>d<rsub|1>671><with|font-shape|italic|Hazel-Atlas
    Co. <abbr|v.> Hartford Co.>, 322 US 238 (1944).>

    <ztbibItemText|<zbibCitationItemID|1342><ztbibitem|sys<rsub|i>d<rsub|1>342><with|font-shape|italic|Mooney
    <abbr|v.> Holohan>, 294 US 103 (1935).>

    <ztbibItemText|<zbibCitationItemID|102><ztbibitem|sys<rsub|i>d<rsub|1>02><with|font-shape|italic|Napue
    <abbr|v.> Illinois>, 360 US 264 (1959).>

    <ztbibItemText|<zbibCitationItemID|1185><ztbibitem|sys<rsub|i>d<rsub|1>185><with|font-shape|italic|Roe
    <abbr|v.> Wade>, 410 US 113 (1973).>

    <ztbibSubHeading|Utah Supreme Court Cases>\ 

    <ztbibItemText|<zbibCitationItemID|3187><ztbibitem|sys<rsub|i>d<rsub|3>187><with|font-shape|italic|Adams
    <abbr|v.> State>, 2005 UT 62, 123 P.3d 400 (2005).>

    <ztbibItemText|<zbibCitationItemID|1241><ztbibitem|sys<rsub|i>d<rsub|1>241><with|font-shape|italic|State
    <abbr|v.> Hernandez>, 268 P.3d 822 (2011).>

    <ztbibItemText|<zbibCitationItemID|3279><ztbibitem|sys<rsub|i>d<rsub|3>279><with|font-shape|italic|State
    <abbr|v.> Kohl>, 999 P.2d 7 (2000).>

    <ztbibItemText|<zbibCitationItemID|1575><ztbibitem|sys<rsub|i>d<rsub|1>575><with|font-shape|italic|Utah
    Farm Bur. Ins. Co. <abbr|v.> Utah Ins. Guar. Ass'n>, 564 P.2d 751
    (1977).>

    <ztbibSubHeading|Utah Court of Appeals Cases>\ 

    <ztbibItemText|<zbibCitationItemID|1523><ztbibitem|sys<rsub|i>d<rsub|1>523><with|font-shape|italic|State
    <abbr|v.> Sery>, 758 P.2d 935 (1988).>

    <ztbibItemText|<zbibCitationItemID|920><ztbibitem|sys<rsub|i>d<rsub|9>20><with|font-shape|italic|Wiscombe
    <abbr|v.> Wiscombe>, 744 P.2d 1024 (1987).>

    <ztbibItemText|<zbibCitationItemID|93><ztbibitem|sys<rsub|i>d<rsub|9>3><with|font-shape|italic|Gonzales
    <abbr|v.> City of Castle Rock>, 307 F.3d 1258 (10th Cir. <abbr|Ct.> of
    <abbr|App.> 2002).>

    <ztbibSubHeading|Other Jurisdictions' Court Cases>\ 

    <ztbibItemText|<zbibCitationItemID|1166><ztbibitem|sys<rsub|i>d<rsub|1>166><with|font-shape|italic|Wallace
    <abbr|v.> Van Pelt>, 969 SW 2d 380 (Mo. <abbr|Ct.> of <abbr|App.> 1998).>

    <ztbibItemText|<zbibCitationItemID|230><ztbibitem|sys<rsub|i>d<rsub|2>30><with|font-shape|italic|Wright
    <abbr|v.> Wright>, 54 NY 437 (N.<abbr|Y.> <abbr|Ct.> of <abbr|App.>
    1873).>

    <ztbibItemText|<zbibCitationItemID|1540><ztbibitem|sys<rsub|i>d<rsub|1>540><with|font-shape|italic|White
    <abbr|v.> Dist. Court, Fourth Dist, Utah Co.>, 232 <abbr|P.> 2d 785 (Utah
    7702).>

    <ztbibItemText|<zbibCitationItemID|1705><ztbibitem|sys<rsub|i>d<rsub|1>705>1
    <with|font-shape|small-caps|Antonin Scalia & Brian <abbr|A.> Garner>,
    <with|font-shape|small-caps|Reading Law: The Interpretation of Legal
    Texts> (2012).>

    <ztbibItemText|<zbibCitationItemID|224><ztbibitem|sys<rsub|i>d<rsub|2>24><with|font-shape|small-caps|Frank
    <abbr|G.> Bennett>, <with|font-shape|small-caps|Citations, Out of the
    Box: Adapting Zotero for Legal and Multilingual Research> (2013).>

    <ztbibItemText|<zbibCitationItemID|1713><ztbibitem|sys<rsub|i>d<rsub|1>713><with|font-shape|small-caps|Dawn
    Watkins & Mandy Burton>, <with|font-shape|small-caps|Research Methods in
    Law> (2013).>

    <ztbibItemText|<zbibCitationItemID|1151><ztbibitem|sys<rsub|i>d<rsub|1>151><with|font-shape|small-caps|Edlin,
    Douglas E.>, <with|font-shape|small-caps|Judges and Unjust Laws: Common
    Law Constitutionalism and the Foundations of Judicial Review> (2008).>

    <ztbibItemText|<zbibCitationItemID|3151><ztbibitem|sys<rsub|i>d<rsub|3>151><with|font-shape|small-caps|George
    Pólya>, <with|font-shape|small-caps|How to Solve It> (Doubleday Anchor
    Books, Second ed. 1957).>

    <ztbibItemText|<zbibCitationItemID|1418><ztbibitem|sys<rsub|i>d<rsub|1>418>1
    <with|font-shape|small-caps|Karen Houppert>,
    <with|font-shape|small-caps|Chasing Gideon: The Elusive Quest for Poor
    People's Justice> (2013).>

    <ztbibItemText|<zbibCitationItemID|1714><ztbibitem|sys<rsub|i>d<rsub|1>714><with|font-shape|small-caps|Jim
    Fay et al.>, <with|font-shape|small-caps|Becoming a Love and Logic
    Parent: Parent Handbook> (2000).>

    <ztbibItemText|<zbibCitationItemID|226><ztbibitem|sys<rsub|i>d<rsub|2>26>1
    <with|font-shape|small-caps|Martin Loughlin>,
    <with|font-shape|small-caps|Foundations of Public Law> (2010).>

    <ztbibItemText|<zbibCitationItemID|2059><ztbibitem|sys<rsub|i>d<rsub|2>059>1
    <with|font-shape|small-caps|Thomas Lundmark>,
    <with|font-shape|small-caps|Charting the Divide Between Common and Civil
    Law> (2012).>

    <ztbibItemText|<zbibCitationItemID|1137><ztbibitem|sys<rsub|i>d<rsub|1>137>1
    <with|font-shape|small-caps|James W.<abbr|H.> McCord & Sandra <abbr|L.>
    McCord>, <with|font-shape|small-caps|Criminal Law and Procedure for the
    Paralegal: A Systems Approach> (2006).>

    <ztbibItemText|<zbibCitationItemID|2583><ztbibitem|sys<rsub|i>d<rsub|2>583><with|font-shape|small-caps|Janet
    O'Sullivan & Jonathan Hilliard>, <with|font-shape|small-caps|Law of
    Contract, The> (Core text series, 5. ed ed. 2012).>

    <ztbibItemText|<zbibCitationItemID|1712><ztbibitem|sys<rsub|i>d<rsub|1>712>1
    <with|font-shape|small-caps|Darrell <abbr|L.> Ross>,
    <with|font-shape|small-caps|Civil Liability in Criminal Justice> (2009).>

    <ztbibItemText|<zbibCitationItemID|1704><ztbibitem|sys<rsub|i>d<rsub|1>704><with|font-shape|small-caps|Thomas
    Bustamante & Christian Dahlman>, <with|font-shape|small-caps|Argument
    Types and Fallacies in Legal Argumentation> (Law and Philosophy Library,
    volume 112, 2015).>

    <ztbibItemText|<zbibCitationItemID|2041><ztbibitem|sys<rsub|i>d<rsub|2>041>1
    <with|font-shape|small-caps|John <abbr|L.> Worrall>,
    <with|font-shape|small-caps|Changing Role of the American Prosecutor,
    The> (2008).>

    <ztbibSubHeading|Books, Journal Articles, and Other Documents>\ 

    <ztbibItemText|<zbibCitationItemID|3371><ztbibitem|sys<rsub|i>d<rsub|3>371>Frank
    <abbr|G.> Bennett Jr, <with|font-shape|italic|LEXITS: Context-Sensitive
    Legal Citations for LATEX> (1993).>

    <ztbibItemText|<zbibCitationItemID|201><ztbibitem|sys<rsub|i>d<rsub|2>01><abbr|H.>
    Mitchell Caldwell, <with|font-shape|italic|Coercive Plea Bargaining: The
    Unrecognized Scourge of the Justice System>, 61
    <with|font-shape|small-caps|<abbr|Cath.> <abbr|<abbr|U.>> <abbr|L.>
    <abbr|Rev.>> 63 (2012).>

    <ztbibItemText|<zbibCitationItemID|1485><ztbibitem|sys<rsub|i>d<rsub|1>485>Joseph
    Di Luca, <with|font-shape|italic|Expedient McJustice or Principled
    Alternative Dispute Resolution - A Review of Plea Bargaining in Canada>,
    50 <with|font-shape|small-caps|<abbr|Crim.> <abbr|L.> <abbr|Q.>> 14
    (2005).>

    <ztbibItemText|<zbibCitationItemID|1977><ztbibitem|sys<rsub|i>d<rsub|1>977>Mayer
    <abbr|G.> Freed, <with|font-shape|italic|Executive Official Immunity for
    Constitutional Violations: An Analysis and a Critique>, 72
    <with|font-shape|small-caps|Nw. UL Rev.> 526 (1977).>

    <ztbibItemText|<zbibCitationItemID|1762><ztbibitem|sys<rsub|i>d<rsub|1>762>Russell
    <abbr|W.> Galloway Jr, <with|font-shape|italic|Basic Constitutional
    Analysis>, 28 <with|font-shape|small-caps|Santa Clara <abbr|L.>
    <abbr|Rev.>> 775 (1988).>

    <ztbibItemText|<zbibCitationItemID|1642><ztbibitem|sys<rsub|i>d<rsub|1>642>Russell
    <abbr|W.> Galloway Jr, <with|font-shape|italic|Basic Equal Protection
    Analysis>, 29 <with|font-shape|small-caps|Santa Clara <abbr|L.>
    <abbr|Rev.>> 121 (1989).>

    <ztbibItemText|<zbibCitationItemID|1766><ztbibitem|sys<rsub|i>d<rsub|1>766>Russell
    <abbr|W.> Galloway Jr, <with|font-shape|italic|Basic Free Speech
    Analysis>, 31 <with|font-shape|small-caps|Santa Clara <abbr|L.>
    <abbr|Rev.>> 883 (1990).>

    <ztbibItemText|<zbibCitationItemID|1809><ztbibitem|sys<rsub|i>d<rsub|1>809>Russell
    <abbr|W.> Galloway Jr, <with|font-shape|italic|Basic Justiciability
    Analysis>, 30 <with|font-shape|small-caps|Santa Clara <abbr|L.>
    <abbr|Rev.>> 911 (1990).>

    <ztbibItemText|<zbibCitationItemID|1978><ztbibitem|sys<rsub|i>d<rsub|1>978>Russell
    <abbr|W.> Galloway Jr, <with|font-shape|italic|Basic Substantive Due
    Process Analysis>, 26 <with|font-shape|small-caps|USFL Rev.> 625 (1991).>

    <ztbibItemText|<zbibCitationItemID|1612><ztbibitem|sys<rsub|i>d<rsub|1>612>Richard
    <abbr|K.> Greenstein, <with|font-shape|italic|Bridging the Mootness Gap
    in Federal Court Class Actions>, 35 <with|font-shape|small-caps|Stan.
    <abbr|L.> <abbr|Rev.>> 897 (1983).>

    <ztbibItemText|<zbibCitationItemID|209><ztbibitem|sys<rsub|i>d<rsub|2>09>Richard
    Klein, <with|font-shape|italic|Due Process Denied: Judicial Coercion in
    the Plea Bargaining Process>, 32 <with|font-shape|small-caps|Hofstra
    <abbr|L.> <abbr|Rev.>> 1349 (2004).>

    <ztbibItemText|<zbibCitationItemID|1158><ztbibitem|sys<rsub|i>d<rsub|1>158>Carolyn
    <abbr|N.> Ko, <with|font-shape|italic|Civil Restraining Orders for
    Domestic Violence: The Unresolved Question of ``Efficacy,''> 11
    <with|font-shape|small-caps|<abbr|S.> Cal. Interdisc. L.J.> 361 (2002).>

    <ztbibItemText|<zbibCitationItemID|3296><ztbibitem|sys<rsub|i>d<rsub|3>296>Michael
    <abbr|C.> McLaughlin, <with|font-shape|italic|It Adds Up: Ineffective
    Assistance of Counsel and the Cumulative Deficiency Doctrine>, 30
    <with|font-shape|small-caps|Ga. St. UL Rev.> 859 (2014).>

    <ztbibItemText|<zbibCitationItemID|2028><ztbibitem|sys<rsub|i>d<rsub|2>028>James
    <abbr|E.> Pfander & David <abbr|R.> Pekarek Krohn,
    <with|font-shape|italic|Interlocutory Review by Agreement of the Parties:
    A Preliminary Analysis>, 105 <with|font-shape|small-caps|Nw. <abbr|U.>
    <abbr|L.> <abbr|Rev.>> 1043 (2011).>

    <ztbibItemText|<zbibCitationItemID|1771><ztbibitem|sys<rsub|i>d<rsub|1>771>Robert
    Allen Sedler, <with|font-shape|italic|Assertion of Constitutional Jus
    Tertii: A Substantive Approach, The>, 70
    <with|font-shape|small-caps|Calif. <abbr|L.> <abbr|Rev.>> 1308 (1982).>

    <ztbibItemText|<zbibCitationItemID|2013><ztbibitem|sys<rsub|i>d<rsub|2>013>Merrill
    Stone, <with|font-shape|italic|Ineffective Assistance of Counsel and
    Post-Conviction Relief in Criminal Cases: Changing Standards and
    Practical Consequences>, 7 <with|font-shape|small-caps|Colum. Hum. Rts.
    <abbr|L.> <abbr|Rev.>> 427 (1975).>

    <ztbibItemText|<zbibCitationItemID|1987><ztbibitem|sys<rsub|i>d<rsub|1>987>Author
    Unkown, <with|font-shape|italic|Mootness and Ripeness: The Postman Always
    Rings Twice>, 65 <with|font-shape|small-caps|<abbr|Colum.> <abbr|L.>
    <abbr|Rev.>> 867 (1965).>

    <ztbibItemText|<zbibCitationItemID|2441><ztbibitem|sys<rsub|i>d<rsub|2>441><abbr|W.>
    Dean Wagner, <with|font-shape|italic|Invalidating a Judgment for Fraud...
    and the Significance of Federal Rule 60 (B)>,
    <with|font-shape|small-caps|Duke Bar Journal> 41 (1952).>

    <ztbibItemText|<zbibCitationItemID|2000><ztbibitem|sys<rsub|i>d<rsub|2>000>Lauren
    Waite, <with|font-shape|italic|Public Interest Exception to Mootness: A
    Moot Point in Texas, The>, 41 <with|font-shape|small-caps|Tex. Tech
    <abbr|L.> <abbr|Rev.>> 681 (2008).>

    <ztbibItemText|<zbibCitationItemID|2338><ztbibitem|sys<rsub|i>d<rsub|2>338>Stewart
    <abbr|M.> Weintraub & Scott <abbr|L.> Austin,
    <with|font-shape|italic|Rebirth of Retroactivity in Judicial
    Decisionmaking: A Pennsylvania Analysis, The>, 1
    <with|font-shape|small-caps|. & Loc. Tax Law.> 99 (1996).>

    <ztbibItemText|<zbibCitationItemID|2890><ztbibitem|sys<rsub|i>d<rsub|2>890>Nick
    Klenow, Due Process: Protecting the Confrontation Right in Civil Cases
    (njklenow@umich.edu 2015).>

    <ztbibItemText|<zbibCitationItemID|1266><ztbibitem|sys<rsub|i>d<rsub|1>266>James
    <abbr|W.> Prescott, <with|font-shape|italic|Body Pleasure and the Origins
    of Violence>, <with|font-shape|small-caps|Article: Body Pleasure and the
    Origins of Violence> (<abbr|Nov.> 1975),<next-line><small|<ztHref|http://violence.de/prescott/bulletin/article.html|http://violence.de/prescott/bulletin/article.html>>.>

    <ztbibItemText|<zbibCitationItemID|1470><ztbibitem|sys<rsub|i>d<rsub|1>470>SAVE,
    Accountability and Oversight of Federally-Funded Domestic Violence
    Programs: Analysis and Recommendations (Stop Abusive and Violent
    Environments 2010).>

    <ztbibItemText|<zbibCitationItemID|1406><ztbibitem|sys<rsub|i>d<rsub|1>406>SAVE,
    Domestic Violence Programs Discriminate Against Male Victims (Stop
    Abusive and Violent Environments 2010).>

    <ztbibItemText|<zbibCitationItemID|1616><ztbibitem|sys<rsub|i>d<rsub|1>616>SAVE,
    Incentives to Make False Allegations of Domestic Violence (Stop Abusive
    and Violent Environments 2010).>

    <ztbibItemText|<zbibCitationItemID|3246><ztbibitem|sys<rsub|i>d<rsub|3>246>UTBar,
    <with|font-shape|italic|Ethics Advisory Opinion No. 00-06 \| Utah State
    Bar>, <with|font-shape|small-caps|Ethics Advisory Opinions> (<abbr|Sept.>
    29, 2000),<next-line><small|<ztHref|https://www.utahbar.org/ethics-advisory-opinions/ethics-advisory-opinion-no-00-06/|https://www.utahbar.org/ethics-advisory-opinions/ethics-advisory-opinion-no-00-06/>>.>

    <ztbibItemText|<zbibCitationItemID|1279><ztbibitem|sys<rsub|i>d<rsub|1>279>Abuse
    Of Protection Orders by Charles <abbr|E.> Corry,
    Ph.D,<next-line><small|<ztHref|http://www.dvmen.org/dv-16.htm|http://www.dvmen.org/dv-16.htm>>.>

    <ztbibItemText|<zbibCitationItemID|2082><ztbibitem|sys<rsub|i>d<rsub|2>082>Annotea
    project,<next-line><small|<ztHref|http://www.w3.org/2001/Annotea/|http://www.w3.org/2001/Annotea/>>.>

    <ztbibItemText|<zbibCitationItemID|3145><ztbibitem|sys<rsub|i>d<rsub|3>145><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Antipattern>
    (2016),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Anti-pattern&oldid=702862794|https://en.wikipedia.org/w/index.php?title=Anti-pattern&oldid=702862794>>.>

    <ztbibItemText|<zbibCitationItemID|1046><ztbibitem|sys<rsub|i>d<rsub|1>046><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Barratry (common
    law)>,<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Barratry_(common_law)&oldid=527579509|http://en.wikipedia.org/w/index.php?title=Barratry_(common_law)&oldid=527579509>>.>

    <ztbibItemText|<zbibCitationItemID|2413><ztbibitem|sys<rsub|i>d<rsub|2>413><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Baumes law>
    (2011),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Baumes_law&oldid=462992829|https://en.wikipedia.org/w/index.php?title=Baumes_law&oldid=462992829>>.>

    <ztbibItemText|<zbibCitationItemID|509><ztbibitem|sys<rsub|i>d<rsub|5>09><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Broken windows
    theory>,<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Broken_windows_theory&oldid=529252026|http://en.wikipedia.org/w/index.php?title=Broken_windows_theory&oldid=529252026>>.>

    <ztbibItemText|<zbibCitationItemID|1163><ztbibitem|sys<rsub|i>d<rsub|1>163>Bully
    OnLine: bullying in the workplace, school, family and community, action
    you can take, stress, psychiatric injury, PTSD, resources, case
    histories, news and contact the media
    (2015),<next-line><small|<ztHref|http://www.bullyonline.org/|http://www.bullyonline.org/>>.>

    <ztbibItemText|<zbibCitationItemID|1380><ztbibitem|sys<rsub|i>d<rsub|1>380>Center
    for Prosecutor Integrity \| Working to preserve the presumption of
    innocence, assure equal treatment under law, and bring an end to wrongful
    convictions (2015),<next-line><small|<ztHref|http://www.prosecutorintegrity.org/|http://www.prosecutorintegrity.org/>>.>

    <ztbibItemText|<zbibCitationItemID|3158><ztbibitem|sys<rsub|i>d<rsub|3>158><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Citation signal>
    (2015),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Citation_signal&oldid=688915368|https://en.wikipedia.org/w/index.php?title=Citation_signal&oldid=688915368>>.>

    <ztbibItemText|<zbibCitationItemID|2083><ztbibitem|sys<rsub|i>d<rsub|2>083>CiteEvidence
    Wiki,<next-line><small|<ztHref|http://www.citeevidence.org/wiki/index.php/Main_Page|http://www.citeevidence.org/wiki/index.php/Main_Page>>.>

    <ztbibItemText|<zbibCitationItemID|2866><ztbibitem|sys<rsub|i>d<rsub|2>866><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Comparative negligence>
    (2014),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Comparative_negligence&oldid=626901800|https://en.wikipedia.org/w/index.php?title=Comparative_negligence&oldid=626901800>>.>

    <ztbibItemText|<zbibCitationItemID|1000><ztbibitem|sys<rsub|i>d<rsub|1>000><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Confidence
    trick>,<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Confidence_trick&oldid=530907178|http://en.wikipedia.org/w/index.php?title=Confidence_trick&oldid=530907178>>.>

    <ztbibItemText|<zbibCitationItemID|3381><ztbibitem|sys<rsub|i>d<rsub|3>381>dev:client_coding:libreoffice_plugin_wire_protocol
    [Zotero Documentation],<next-line><small|<ztHref|https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol|https://www.zotero.org/support/dev/client_coding/libreoffice_plugin_wire_protocol>>.>

    <ztbibItemText|<zbibCitationItemID|3544><ztbibitem|sys<rsub|i>d<rsub|3>544><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Ex turpi causa non oritur
    actio> (2016),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Ex_turpi_causa_non_oritur_actio&oldid=714644748|https://en.wikipedia.org/w/index.php?title=Ex_turpi_causa_non_oritur_actio&oldid=714644748>>.>

    <ztbibItemText|<zbibCitationItemID|3152><ztbibitem|sys<rsub|i>d<rsub|3>152>How
    the Drug War Disappeared the Jury Trial (<abbr|Jan.> 15,
    2014),<next-line><small|<ztHref|http://www.outsidethebeltway.com/how-the-drug-war-disappeared-the-jury-trial/|http://www.outsidethebeltway.com/how-the-drug-war-disappeared-the-jury-trial/>>.>

    <ztbibItemText|<zbibCitationItemID|490><ztbibitem|sys<rsub|i>d<rsub|4>90><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Legal fiction>
    (2015),<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Legal_fiction&oldid=644149316|http://en.wikipedia.org/w/index.php?title=Legal_fiction&oldid=644149316>>.>

    <ztbibItemText|<zbibCitationItemID|3177><ztbibitem|sys<rsub|i>d<rsub|3>177>Legal
    XML,<next-line><small|<ztHref|http://www.legalxml.org/|http://www.legalxml.org/>>.>

    <ztbibItemText|<zbibCitationItemID|227><ztbibitem|sys<rsub|i>d<rsub|2>27><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Noble lie>
    (2015),<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Noble_lie&oldid=633689799|http://en.wikipedia.org/w/index.php?title=Noble_lie&oldid=633689799>>.>

    <ztbibItemText|<zbibCitationItemID|3175><ztbibitem|sys<rsub|i>d<rsub|3>175>OASIS
    LegalDocumentML (LegalDocML) TC \| OASIS,<next-line><small|<ztHref|https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=legaldocml#technical|https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=legaldocml#technical>>.>

    <ztbibItemText|<zbibCitationItemID|3179><ztbibitem|sys<rsub|i>d<rsub|3>179>OASIS
    LegalXML Electronic Court Filing TC \|
    OASIS,<next-line><small|<ztHref|https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=legalxml-courtfiling|https://www.oasis-open.org/committees/tc_home.php?wg_abbrev=legalxml-courtfiling>>.>

    <ztbibItemText|<zbibCitationItemID|2900><ztbibitem|sys<rsub|i>d<rsub|2>900><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Observer-expectancy
    effect> (2015),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Observer-expectancy_effect&oldid=687490834|https://en.wikipedia.org/w/index.php?title=Observer-expectancy_effect&oldid=687490834>>.>

    <ztbibItemText|<zbibCitationItemID|2072><ztbibitem|sys<rsub|i>d<rsub|2>072>Overlay
    Web - P2P Foundation,<next-line><small|<ztHref|http://p2pfoundation.net/Overlay_Web?title=Overlay_Web|http://p2pfoundation.net/Overlay_Web?title=Overlay_Web>>.>

    <ztbibItemText|<zbibCitationItemID|2965><ztbibitem|sys<rsub|i>d<rsub|2>965><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Perverse incentive>
    (2015),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Perverse_incentive&oldid=685561404|https://en.wikipedia.org/w/index.php?title=Perverse_incentive&oldid=685561404>>.>

    <ztbibItemText|<zbibCitationItemID|1699><ztbibitem|sys<rsub|i>d<rsub|1>699><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Principle of double
    effect> (2015),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Principle_of_double_effect&oldid=670761708|https://en.wikipedia.org/w/index.php?title=Principle_of_double_effect&oldid=670761708>>.>

    <ztbibItemText|<zbibCitationItemID|1072><ztbibitem|sys<rsub|i>d<rsub|1>072><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Random walk>
    (2015),<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Random_walk&oldid=637474906|http://en.wikipedia.org/w/index.php?title=Random_walk&oldid=637474906>>.>

    <ztbibItemText|<zbibCitationItemID|3342><ztbibitem|sys<rsub|i>d<rsub|3>342><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Scire Facias>
    (2016),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Scire_facias&oldid=707644683|https://en.wikipedia.org/w/index.php?title=Scire_facias&oldid=707644683>>.>

    <ztbibItemText|<zbibCitationItemID|3561><ztbibitem|sys<rsub|i>d<rsub|3>561>The
    Indigo Book: A Manual of Legal Citation,<next-line><small|<ztHref|https://law.resource.org/pub/us/code/blue/IndigoBook.html#R6|https://law.resource.org/pub/us/code/blue/IndigoBook.html#R6>>.>

    <ztbibItemText|<zbibCitationItemID|2960><ztbibitem|sys<rsub|i>d<rsub|2>960>The
    Plover Blog: Steno 101, Lesson Zero,<next-line><small|<ztHref|http://plover.stenoknight.com/2010/06/steno-101-lesson-zero.html|http://plover.stenoknight.com/2010/06/steno-101-lesson-zero.html>>.>

    <ztbibItemText|<zbibCitationItemID|1066><ztbibitem|sys<rsub|i>d<rsub|1>066><with|font-shape|small-caps|Wikipedia,
    the free encyclopedia> <with|font-shape|italic|Watts
    Riots>,<next-line><small|<ztHref|http://en.wikipedia.org/w/index.php?title=Watts_Riots&oldid=531152328|http://en.wikipedia.org/w/index.php?title=Watts_Riots&oldid=531152328>>.>

    <ztbibItemText|<zbibCitationItemID|2074><ztbibitem|sys<rsub|i>d<rsub|2>074>Wikalong
    Firefox Extension,<next-line><small|<ztHref|http://wikalong.org/|http://wikalong.org/>>.>
  </zbibliography>

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
    <associate|zotero-BibliographyStyle_arrayList|<tuple>>
    <associate|zotero-BibliographyStyle_bodyIndent|0.0000tab>
    <associate|zotero-BibliographyStyle_entrySpacing|1.0000>
    <associate|zotero-BibliographyStyle_firstLineIndent|0.0000tab>
    <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
    <associate|zotero-BibliographyStyle_tabStopCount|0>
    <associate|zotero-data-data-version|3>
    <associate|zotero-data-zotero-version|4.0.29.10m85>
    <associate|zotero-pref-automaticJournalAbbreviations|true>
    <associate|zotero-pref-citationAffixes|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|>
    <associate|zotero-pref-citationLangPrefsInstitutions|orig>
    <associate|zotero-pref-citationLangPrefsJournals|orig>
    <associate|zotero-pref-citationLangPrefsPersons|orig>
    <associate|zotero-pref-citationLangPrefsPlaces|orig>
    <associate|zotero-pref-citationLangPrefsPublishers|orig>
    <associate|zotero-pref-citationLangPrefsTitles|orig>
    <associate|zotero-pref-citationSort|en>
    <associate|zotero-pref-citationTranslation|en>
    <associate|zotero-pref-citationTransliteration|en>
    <associate|zotero-pref-extractingLibraryID|0>
    <associate|zotero-pref-extractingLibraryName|No group selected>
    <associate|zotero-pref-fieldType|ReferenceMark>
    <associate|zotero-pref-noteType|0>
    <associate|zotero-pref-noteType0|true>
    <associate|zotero-pref-noteType1|false>
    <associate|zotero-pref-noteType2|false>
    <associate|zotero-pref-projectName|>
    <associate|zotero-pref-storeReferences|true>
    <associate|zotero-pref-suppressTrailingPunctuation|false>
    <associate|zotero-session-id|zVlmm01d>
    <associate|zotero-style-bibliographyStyleHasBeenSet|1>
    <associate|zotero-style-hasBibliography|1>
    <associate|zotero-style-id|http://juris-m.github.io/styles/jm-indigobook-catsort-bib>
    <associate|zotero-style-locale|en-US>
    <associate|zoteroDocumentData|\<data data-version="3"
    zotero-version="4.0.29.10m85"\>\<session id="zVlmm01d"/\>\<style
    id="http://juris-m.github.io/styles/jm-indigobook-catsort-bib"
    locale="en-US" hasBibliography="1" bibliographyStyleHasBeenSet="1"/\>\<prefs\>\<pref
    name="citationTransliteration" value="en"/\>\<pref
    name="citationTranslation" value="en"/\>\<pref name="citationSort"
    value="en"/\>\<pref name="citationLangPrefsPersons" value="orig"/\>\<pref
    name="citationLangPrefsInstitutions" value="orig"/\>\<pref
    name="citationLangPrefsTitles" value="orig"/\>\<pref
    name="citationLangPrefsJournals" value="orig"/\>\<pref
    name="citationLangPrefsPublishers" value="orig"/\>\<pref
    name="citationLangPrefsPlaces" value="orig"/\>\<pref
    name="citationAffixes" value="\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|\|"/\>\<pref
    name="projectName" value=""/\>\<pref name="extractingLibraryID"
    value="0"/\>\<pref name="extractingLibraryName" value="No group
    selected"/\>\<pref name="fieldType" value="ReferenceMark"/\>\<pref
    name="storeReferences" value="true"/\>\<pref
    name="automaticJournalAbbreviations" value="true"/\>\<pref
    name="noteType" value="0"/\>\<pref name="suppressTrailingPunctuation"
    value="false"/\>\</prefs\>\</data\>>
  </collection>
</initial>