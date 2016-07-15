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
    moved citations as well as the bibliography to be updated. The refresh is
    fairly efficient, selectively changing only the few that must be changed.
    You may also use <key|M-C-r> (or <key|Esc C-r>) to call for the refresh
    operation.

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

  <\zbibliography|+DvR2qZelkLrxq7|<#4249424C207B22756E6369746564223A5B5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3242373434343643225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A53464D3857434A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F47374E55434B5655225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F393634555A384748225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4851353645463637225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F41474246384B5A47225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4552504B544D5A52225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F36434D334E464151225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3239524A47495143225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F46534B5541545243225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F54494B4E54323544225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3939584658455353225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D3946413455324B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F483546345A4E4947225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4753505256364949225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5350444342494156225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E5A484E48464E55225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4745363639375349225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F48545A5634514E35225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F564B384241344346225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F48365657534E374A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F54475A4448585238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5437373556514956225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F39424B4158354147225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5154394B36374E46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3949574337344A48225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F49535A32544B434A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F514B413755564D46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3942524D45364246225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F44494B39374B465A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3254365056453445225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4344383244465034225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F514E54514D565455225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5858363651514835225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F473843564D433656225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524836543858524D225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F555747584B393851225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3856523438524437225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5556543651445455225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4644415455485051225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F53444E4B4E5A344D225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F50494D41504B5641225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F574A454632564141225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3550343937553748225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A45524E32343458225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B57533257554645225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F50464741374A4458225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F574B374A555A4755225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F58414B503534445A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F364D4744514B534A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E4A383843503853225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F42414A564D463552225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3946385844465432225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4454534B34504643225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B57584D53374341225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D473933554B5357225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F464A35334D45424A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3638414B505A4B44225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3541555045504D53225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B48444849524E45225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5037453952525237225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F374341354B374332225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4635575238425639225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A444E41394D3848225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4A47513347344E4B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524753555450414A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56553452494D4238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F484B4D3548495238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E385339554D4453225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F543542485633354B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F52483444484E3436225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3849425341534B46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3753453537465033225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4447384945325046225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F584B465547474E52225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F333856454A335850225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3535554A42334E58225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5443534D344E3648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F46373333394B4544225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3752434B52384947225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3532325356345353225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5258374444554B51225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3350343842354D43225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5056415452443748225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F414856354E455057225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524133374B455657225D5D2C22637573746F6D223A5B5D7D2043534C5F4249424C494F475241504859>>
    <ztbibSubHeading|United States Code>\ 

    <ztbibItemText|<zbibCitationItemID|3434><ztbibitem|sys<rsub|i>d<rsub|3>434><with|font-shape|italic|Definition
    of \0\0scheme or artifice to defraud,''> 18 <abbr|<abbr|U.>S.C.>>

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

    <ztbibSubHeading|Utah Rules of Civil Procedure>\ 

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

    <ztbibItemText|<zbibCitationItemID|2039><ztbibitem|sys<rsub|i>d<rsub|2>039><with|font-shape|italic|Allen
    <abbr|v.> McCurry>, 449 US 90 (1980).>

    <ztbibItemText|<zbibCitationItemID|2275><ztbibitem|sys<rsub|i>d<rsub|2>275><with|font-shape|italic|Brady
    <abbr|v.> Maryland>, 373 US 83 (1963).>

    <ztbibItemText|<zbibCitationItemID|1170><ztbibitem|sys<rsub|i>d<rsub|1>170><with|font-shape|italic|Friends
    of Earth, <abbr|Inc.> <abbr|v.> Laidlaw Environmental Services (TOC),
    <abbr|Inc.>>, 528 US 167 (2000).>

    <ztbibItemText|<zbibCitationItemID|1342><ztbibitem|sys<rsub|i>d<rsub|1>342><with|font-shape|italic|Mooney
    <abbr|v.> Holohan>, 294 US 103 (1935).>

    <ztbibItemText|<zbibCitationItemID|1624><ztbibitem|sys<rsub|i>d<rsub|1>624><with|font-shape|italic|Ross
    <abbr|v.> Moffitt>, 417 US 600 (1974).>

    <ztbibItemText|<zbibCitationItemID|3187><ztbibitem|sys<rsub|i>d<rsub|3>187><with|font-shape|italic|Adams
    <abbr|v.> State>, 2005 UT 62, 123 P.3d 400 (2005).>

    <ztbibItemText|<zbibCitationItemID|1575><ztbibitem|sys<rsub|i>d<rsub|1>575><with|font-shape|italic|Utah
    Farm Bur. Ins. Co. <abbr|v.> Utah Ins. Guar. Ass'n>, 564 P.2d 751
    (1977).>

    <ztbibSubHeading|Utah Court of Appeals Cases>\ 

    <ztbibItemText|<zbibCitationItemID|1324><ztbibitem|sys<rsub|i>d<rsub|1>324><with|font-shape|italic|Crouse
    <abbr|v.> Crouse>, 817 P.2d 836 (1991).>

    <ztbibItemText|<zbibCitationItemID|920><ztbibitem|sys<rsub|i>d<rsub|9>20><with|font-shape|italic|Wiscombe
    <abbr|v.> Wiscombe>, 744 P.2d 1024 (1987).>

    <ztbibSubHeading|Other Jurisdictions' Court Cases>\ 

    <ztbibItemText|<zbibCitationItemID|2530><ztbibitem|sys<rsub|i>d<rsub|2>530><with|font-shape|italic|Woods
    <abbr|v.> Shewry>, No. C056072 (Cal. Court of Appeal <abbr|Oct.> 14,
    2008).>

    <ztbibItemText|<zbibCitationItemID|3349><ztbibitem|sys<rsub|i>d<rsub|3>349><with|font-shape|italic|Associated
    Provincial Picture Houses Ltd <abbr|v.> Wednesbury Corporation>, 1 KB 223
    (England & Wales Civil Division, Court of Appeal 1947).>

    <ztbibItemText|<zbibCitationItemID|3468><ztbibitem|sys<rsub|i>d<rsub|3>468><with|font-shape|italic|<abbr|Dr.>
    Bonham's Case>, 77 English Reports 646 (England & Wales Masters, Chancery
    Division, High Court 1861).>

    <ztbibItemText|<zbibCitationItemID|212><ztbibitem|sys<rsub|i>d<rsub|2>12><with|font-shape|italic|Commissioner
    of Probation <abbr|v.> Adams>, 65 <abbr|<abbr|Mass.>> <abbr|<abbr|App.>>
    <abbr|<abbr|Ct.>> 725 (<abbr|Mass.> <abbr|App.> <abbr|Ct.> 2006).>

    <ztbibItemText|<zbibCitationItemID|1607><ztbibitem|sys<rsub|i>d<rsub|1>607><with|font-shape|italic|Brandt
    <abbr|v.> Gooding>, 630 S.E.2d 259 (S.<abbr|C.> 2006).>

    <ztbibSubHeading|Utah Legislature Bills>\ 

    <ztbibItemText|<zbibCitationItemID|3237><ztbibitem|sys<rsub|i>d<rsub|3>237>HB0317
    HB0317, H.<abbr|R.> (2016th Sess. 2016).>

    <ztbibItemText|<zbibCitationItemID|3450><ztbibitem|sys<rsub|i>d<rsub|3>450>HB0148
    HB0148, Utah State House of Representatives (2016).>

    <ztbibItemText|<zbibCitationItemID|3452><ztbibitem|sys<rsub|i>d<rsub|3>452>NP
    Mar 10, 2016 HB0399, Utah State House of Representatives (2016).>

    <ztbibItemText|<zbibCitationItemID|3285><ztbibitem|sys<rsub|i>d<rsub|3>285>SB0263
    SB0263, Utah State Senate (2013).>

    <ztbibItemText|<zbibCitationItemID|3448><ztbibitem|sys<rsub|i>d<rsub|3>448>SB0090
    SB0090, Utah State Senate (2016th Sess. 2016).>

    <ztbibSubHeading|Books, Journal Articles, and Other Documents>\ 

    <ztbibItemText|<zbibCitationItemID|1395><ztbibitem|sys<rsub|i>d<rsub|1>395><with|font-shape|small-caps|Isaac
    Asimov>, <with|font-shape|small-caps|The Currents of Space> (Reprint
    edition ed. 1952).>

    <ztbibItemText|<zbibCitationItemID|3151><ztbibitem|sys<rsub|i>d<rsub|3>151><with|font-shape|small-caps|George
    Pólya>, <with|font-shape|small-caps|How to Solve It> (Doubleday Anchor
    Books, Second ed. 1957).>

    <ztbibItemText|<zbibCitationItemID|2860><ztbibitem|sys<rsub|i>d<rsub|2>860><with|font-shape|small-caps|Jürgen
    Habermas & Thomas McCarthy>, <with|font-shape|small-caps|The Theory of
    Communicative Action: Reason and the Rationalization of Society> (1985).>

    <ztbibItemText|<zbibCitationItemID|2968><ztbibitem|sys<rsub|i>d<rsub|2>968>1
    <with|font-shape|small-caps|John <abbr|L.> Worrall>,
    <with|font-shape|small-caps|The Changing Role of the American Prosecutor>
    (2008).>

    <ztbibItemText|<zbibCitationItemID|1516><ztbibitem|sys<rsub|i>d<rsub|1>516>Thomas
    <abbr|C.> Ackerman Jr, <with|font-shape|italic|Standards of Punishment in
    Contempt Cases>, 39 <with|font-shape|small-caps|Calif. <abbr|L.>
    <abbr|Rev.>> 552 (1951).>

    <ztbibItemText|<zbibCitationItemID|2333><ztbibitem|sys<rsub|i>d<rsub|2>333>Jonathan
    <abbr|H.> Adler, <with|font-shape|italic|Standing Still in the Roberts
    Court>, 59 <with|font-shape|small-caps|Case <abbr|W.> Res. <abbr|L.>
    <abbr|Rev.>> 1061 (2008).>

    <ztbibItemText|<zbibCitationItemID|1735><ztbibitem|sys<rsub|i>d<rsub|1>735>Angela
    <abbr|J.> Davis, <with|font-shape|italic|The Legal System's Failure to
    Discipline Unethical Prosecutors>, 36 <with|font-shape|small-caps|Hofstra
    <abbr|L.> <abbr|Rev.>> 275 (2007).>

    <ztbibItemText|<zbibCitationItemID|2149><ztbibitem|sys<rsub|i>d<rsub|2>149>Jeffrey
    Bellin, <with|font-shape|italic|The Incredible Shrinking Confrontation
    Clause>, 92 <with|font-shape|small-caps|B.<abbr|U.> <abbr|L.>
    <abbr|Rev.>> 1865 (2012).>

    <ztbibItemText|<zbibCitationItemID|3241><ztbibitem|sys<rsub|i>d<rsub|3>241>Albert
    Broderick, <with|font-shape|italic|Warth Optional Standing Doctrine:
    Return to Judicial Supremacy, The>, 25 <with|font-shape|small-caps|Cath.
    UL Rev.> 467 (1975).>

    <ztbibItemText|<zbibCitationItemID|1614><ztbibitem|sys<rsub|i>d<rsub|1>614>Jean
    Wegman Burns, <with|font-shape|italic|Standing and Mootness in Class
    Actions: A Search for Consistency>, 22
    <with|font-shape|small-caps|<abbr|U.><abbr|C.> Davis <abbr|L.>
    <abbr|Rev.>> 1239 (1988\U1989).>

    <ztbibItemText|<zbibCitationItemID|1401><ztbibitem|sys<rsub|i>d<rsub|1>401>Charles
    Clark, <with|font-shape|italic|The Union of Law and Equity>, 25
    <with|font-shape|small-caps|<abbr|Colum.> <abbr|L.> <abbr|Rev.>> 1
    (1925).>

    <ztbibItemText|<zbibCitationItemID|2019><ztbibitem|sys<rsub|i>d<rsub|2>019>Peter
    Finn, <with|font-shape|italic|Statutory Authority in the Use and
    Enforcement of Civil Protection Orders against Domestic Abuse>, 23
    <with|font-shape|small-caps|Fam. LQ> 43 (1989).>

    <ztbibItemText|<zbibCitationItemID|1417><ztbibitem|sys<rsub|i>d<rsub|1>417>Richard
    <abbr|D.> Friedman, <with|font-shape|italic|The Confrontation Clause
    Re-Rooted and Transformed>, <with|font-shape|small-caps|Cato Supreme
    Court Review> 439 (2004).>

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

    <ztbibItemText|<zbibCitationItemID|1529><ztbibitem|sys<rsub|i>d<rsub|1>529>Bennett
    <abbr|L.> Gershman, <with|font-shape|italic|The \0\0Perjury Trap,''> 129
    <with|font-shape|small-caps|<abbr|U.> Pa. <abbr|L.> <abbr|Rev.>> 624
    (1981).>

    <ztbibItemText|<zbibCitationItemID|2139><ztbibitem|sys<rsub|i>d<rsub|2>139>Sally
    <abbr|F.> Goldfarb, <with|font-shape|italic|The Supreme Court, the
    Violence Against Women Act, and the Use and Abuse of Federalism>, 71
    <with|font-shape|small-caps|Fordham <abbr|L.> <abbr|Rev.>> 57 (2002).>

    <ztbibItemText|<zbibCitationItemID|1577><ztbibitem|sys<rsub|i>d<rsub|1>577>Morton
    <abbr|J.> Horwitz, <with|font-shape|italic|The History of the
    Public/Private Distinction>, 130 <with|font-shape|small-caps|<abbr|U.>
    Pa. <abbr|L.> <abbr|Rev.>> 1423 (1982).>

    <ztbibItemText|<zbibCitationItemID|1991><ztbibitem|sys<rsub|i>d<rsub|1>991>John
    <abbr|H.> Jackson, <with|font-shape|italic|Status of Treaties in Domestic
    Legal Systems: A Policy Analysis>, <with|font-shape|small-caps|Am.
    <abbr|I.> Int'l L.> 310 (1992).>

    <ztbibItemText|<zbibCitationItemID|2156><ztbibitem|sys<rsub|i>d<rsub|2>156>Daniel
    <abbr|S.> Medwed, <with|font-shape|italic|The Zeal Deal: Prosecutorial
    Resistance to Post-Conviction Claims of Innocence>, 84
    <with|font-shape|small-caps|B.<abbr|U.> <abbr|L.> <abbr|Rev.>> 125
    (2004).>

    <ztbibItemText|<zbibCitationItemID|2340><ztbibitem|sys<rsub|i>d<rsub|2>340>Gene
    <abbr|R.> Nichol Jr, <with|font-shape|italic|Standing for Privilege: The
    Failure of Injury Analysis>, 82 <with|font-shape|small-caps|BUL Rev.> 301
    (2002).>

    <ztbibItemText|<zbibCitationItemID|1533><ztbibitem|sys<rsub|i>d<rsub|1>533>John
    Reginald Nizol, <with|font-shape|italic|Sacrificing the Constitution on
    the Altar of Victim Advocacy: Due Process, the Warrant Clause and the
    Immediate Enforceability of Ex Parte Protection Orders>,
    <with|font-shape|small-caps|Student Scholarship> (2005).>

    <ztbibItemText|<zbibCitationItemID|2341><ztbibitem|sys<rsub|i>d<rsub|2>341>Colleen
    <abbr|M.> O'Connor, <with|font-shape|italic|Statutory Surrogate Consent
    Provisions: An Overview and Analysis>, 20
    <with|font-shape|small-caps|Mental & Physical Disability <abbr|L.> Rep.>
    128 (1996).>

    <ztbibItemText|<zbibCitationItemID|1410><ztbibitem|sys<rsub|i>d<rsub|1>410>Zygmunt
    JB Plater, <with|font-shape|italic|Statutory Violations and Equitable
    Discretion>, <with|font-shape|small-caps|Calif. <abbr|L.> <abbr|Rev.>>
    524 (1982).>

    <ztbibItemText|<zbibCitationItemID|2344><ztbibitem|sys<rsub|i>d<rsub|2>344>Arthur
    <abbr|F.> Sampson III, <with|font-shape|italic|Substitution under Federal
    Rule of Civil Procedure 25 (D): Mootness and Related Problems>,
    <with|font-shape|small-caps|The University of Chicago Law Review> 192
    (1975).>

    <ztbibItemText|<zbibCitationItemID|1771><ztbibitem|sys<rsub|i>d<rsub|1>771>Robert
    Allen Sedler, <with|font-shape|italic|Assertion of Constitutional Jus
    Tertii: A Substantive Approach, The>, 70
    <with|font-shape|small-caps|Calif. <abbr|L.> <abbr|Rev.>> 1308 (1982).>

    <ztbibItemText|<zbibCitationItemID|2343><ztbibitem|sys<rsub|i>d<rsub|2>343>Walter
    <abbr|M.> Weber, <with|font-shape|italic|Substituted Judgment Doctrine: A
    Critical Analysis>, 1 <with|font-shape|small-caps|Issues <abbr|L.> &
    Med.> 131 (1985).>

    <ztbibItemText|<zbibCitationItemID|229><ztbibitem|sys<rsub|i>d<rsub|2>29>Jill
    Burke, <with|font-shape|italic|Will Alaska's Plea Bargain Plan Serve
    Justice, or Cause It to Grind to a Halt?>,
    <with|font-shape|small-caps|Alaska Dispatch>, <abbr|Aug.> 13,
    2013,<next-line><small|<ztHref|http://www.adn.com/article/20130813/will-alaskas-plea-bargain-plan-serve-justice-or-cause-it-grind-halt|http://www.adn.com/article/20130813/will-alaskas-plea-bargain-plan-serve-justice-or-cause-it-grind-halt>>.>

    <ztbibItemText|<zbibCitationItemID|1662><ztbibitem|sys<rsub|i>d<rsub|1>662>James
    <abbr|H.> Feeney, PhD, Testing the False Teachers and Their Erroneous
    Ways (2015),<next-line><small|<ztHref|http://www.jimfeeney.org/testing-false-teachers-heresy.html|http://www.jimfeeney.org/testing-false-teachers-heresy.html>>.>

    <ztbibItemText|<zbibCitationItemID|1348><ztbibitem|sys<rsub|i>d<rsub|1>348>Kate
    Pickert, What's Wrong with the Violence Against Women Act? (<abbr|Feb.>
    27, 2013),<next-line><small|<ztHref|http://nation.time.com/2013/02/27/whats-wrong-with-the-violence-against-women-act/|http://nation.time.com/2013/02/27/whats-wrong-with-the-violence-against-women-act/>>.>

    <ztbibItemText|<zbibCitationItemID|3315><ztbibitem|sys<rsub|i>d<rsub|3>315>Joseph
    <abbr|C.> O'Keefe & Daniel <abbr|L.> Saperstein, Third Circuit
    \0\0Clarifies'' Continuing Violation Doctrine (<abbr|Feb.> 22,
    2013),<next-line><small|<ztHref|http://www.proskauer.com/publications/client-alert/third-circuit-clarifies-continuing-violation-doctrine/|http://www.proskauer.com/publications/client-alert/third-circuit-clarifies-continuing-violation-doctrine/>>.>

    <ztbibItemText|<zbibCitationItemID|1532><ztbibitem|sys<rsub|i>d<rsub|1>532>SAVE,
    The Use and Abuse of Domestic Restraining Orders (Stop Abusive and
    Violent Environments 2011).>

    <ztbibItemText|<zbibCitationItemID|1336><ztbibitem|sys<rsub|i>d<rsub|1>336>SAVE,
    What is the Cost of False Allegations of Domestic Violence? (Stop Abusive
    and Violent Environments 2010).>

    <ztbibItemText|<zbibCitationItemID|3114><ztbibitem|sys<rsub|i>d<rsub|3>114>Todd
    Garvey, The Take Care Clause and Executive Discretion in the Enforcement
    of Law (Congressional Research Service 2014).>

    <ztbibItemText|<zbibCitationItemID|3152><ztbibitem|sys<rsub|i>d<rsub|3>152>How
    the Drug War Disappeared the Jury Trial (<abbr|Jan.> 15,
    2014),<next-line><small|<ztHref|http://www.outsidethebeltway.com/how-the-drug-war-disappeared-the-jury-trial/|http://www.outsidethebeltway.com/how-the-drug-war-disappeared-the-jury-trial/>>.>

    <ztbibItemText|<zbibCitationItemID|2883><ztbibitem|sys<rsub|i>d<rsub|2>883><with|font-shape|small-caps|Wikipedia>
    <with|font-shape|italic|Self-fulfilling prophecy>
    (2015),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Self-fulfilling_prophecy&oldid=688780424|https://en.wikipedia.org/w/index.php?title=Self-fulfilling_prophecy&oldid=688780424>>.>

    <ztbibItemText|<zbibCitationItemID|2962><ztbibitem|sys<rsub|i>d<rsub|2>962><with|font-shape|small-caps|Wikipedia>
    <with|font-shape|italic|Streetlight effect>
    (2015),<next-line><small|<ztHref|https://en.wikipedia.org/w/index.php?title=Streetlight_effect&oldid=683183525|https://en.wikipedia.org/w/index.php?title=Streetlight_effect&oldid=683183525>>.>

    <ztbibItemText|<zbibCitationItemID|751><ztbibitem|sys<rsub|i>d<rsub|7>51>Teague
    <abbr|v.> Lane \| Casebriefs (1989),<next-line><small|<ztHref|http://www.casebriefs.com/blog/law/criminal-procedure/criminal-procedure-keyed-to-saltzburg/basic-principles/teague-v-lane-2/|http://www.casebriefs.com/blog/law/criminal-procedure/criminal-procedure-keyed-to-saltzburg/basic-principles/teague-v-lane-2/>>.>
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
    <associate|zotero-data-zotero-version|4.0.29.10m88alpha>
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
    <associate|zotero-session-id|c8cbCZXT>
    <associate|zotero-style-bibliographyStyleHasBeenSet|1>
    <associate|zotero-style-hasBibliography|1>
    <associate|zotero-style-id|http://juris-m.github.io/styles/jm-indigobook-catsort-bib>
    <associate|zotero-style-locale|en-US>
    <associate|zoteroDocumentData|\<data data-version="3"
    zotero-version="4.0.29.10m88alpha"\>\<session id="c8cbCZXT"/\>\<style
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