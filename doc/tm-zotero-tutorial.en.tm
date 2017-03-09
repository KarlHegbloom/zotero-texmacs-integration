<TeXmacs|1.99.9>

<style|<tuple|legal-brief|english|doc>>

<\body>
  <\hide-preamble>
    <assign|zbibItemRef|<macro|label|<pageref|<arg|label>>>>

    <assign|bibliography-text|<macro|Sample <localize|Bibliography>>>
  </hide-preamble>

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

    At this time, it is only <strong|verified> working on Linux. I don't have
    Windows or a Mac to develop it on. I will accept pull-requests from
    anyone willing and able to make it work on those platforms. Having said
    that, it's possible that it might work on Windows or Mac OS-X as-is.
    Somebody needs to try it.

    Someday this will all be an easily installable package for everyday
    people who don't have a degree or hobby in computer science. For now,
    it's a kit.
  </framed>

  <\itemize-arrow>
    <item>Install the Juris-M stand-alone, according to the instructions
    given in the README on my github repository:

    <center|<slink|https://github.com/KarlHegbloom/zotero-texmacs-integration>>

    Juris-M is available via:

    <center|<slink|http://juris-m.github.io>>

    The Juris-M standalone already has the <tt|abbrevs-filter> and other
    add-ons, such as <tt|zotfile>, installed. The <tt|propachi-texmacs> still
    must be installed. (See below.)

    You should also install the Juris-M \Pconnector\Q plugin for Firefox, and
    configure it to use the same data directory as Juris-M standalone.
    <strong|Always start Juris-M first, wait for it to post a GUI window, and
    then start Firefox.>

    <item>Install a recent version of <TeXmacs>. There are pre-release builds
    available for Ubuntu via my github mirror of the upstream subversion
    repository:

    <center|<href|https://github.com/KarlHegbloom/texmacs/releases/>>

    With those, there are not guarantees; they are very fresh and sometimes
    crashy. For now they may depend on versions of Qt5 that are from a ppa,
    rather than from the primary Ubuntu distribution. <strong|You can always
    build TeXmacs yourself, from source.> If the latest one does not work
    right, try a slightly older one. When I discover that it's not working,
    I'll delete them. They won't harm your computer or lose your work, but
    might crash in new less-tested code. These are pre-release builds from
    the trunk of the subversion source code repository!

    <item>Install my <tt|propachi-texmacs> plugin for Firefox or stand-alone
    Juris-M:

    <center|<slink|https://github.com/KarlHegbloom/propachi-texmacs/releases/>>

    It will probably not auto-update, and from time to time, you may need to
    update it by-hand. <strong|The plugin is signed now, so you don't need to
    make any special settings.> You can reset
    <verbatim|xpinstall.signatures.required> to it's default now, if you had
    it set for an earlier (pre-release) version.

    <item>If it is not pre-installed, install the OpenOffice plugin for
    Juris-M / Zotero. This is necessary because it contains the service code
    that opens the localhost TCP network port and associates it with the
    protocol handlers that connects Juris-M / Zotero's
    <tt|xpcom/integration.js> to OpenOffice. The \Pmonkey patch\Q in
    <tt|propachi-texmacs> fixes it up so that it can talk to <TeXmacs>,
    primarily by changing the outputformat from RTF, which it uses when
    integrating with OpenOffice or Word, to a set of <LaTeX> macros, which
    are readily translated and understood by <TeXmacs>.

    <\itemize>
      <item>Be aware that the OpenOffice plugin won't work right with the
      <tt|outputFormat> set to <tt|bbl>, which is what this <TeXmacs>
      integration requires. You can switch it back to the setting that
      OpenOffice requires by changing <tt|*zotero.integration.outputFormat>
      to <tt|rtf>, or by disabling <tt|propachi-texmacs>.
    </itemize>

    <item>Clone the git repository of <tt|zotero-texmacs-integration> and
    symlink it into your <tt|$TEX<no-break>MACS_HOME> like this:

    <\shell-code>
      mkdir ~/src

      cd ~/src

      git clone https://github.com/KarlHegbloom/zotero-texmacs-integration.git

      cd zotero-texmacs-integration

      git checkout branch-v1.2a-next-wip

      mkdir --parents ~/.TeXmacs/plugins

      cd ~/.TeXmacs/plugins

      ln -s ~/src/zotero-texmacs-integration tm-zotero
    </shell-code>

    <item>Launch Juris-M and make sure it's working properly. Always launch
    Juris-M standalone <em|before> you launch Firefox. Add a few references
    to your collection if you have nothing in it yet. Try visiting
    Scholar.Google.com and searching for a legal case. There will be an icon
    in the URL bar that you can click to add the case to Juris-M.

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

    <item>Be aware that sometimes operations between <TeXmacs> and Juris-M /
    Zotero can take a while. Be patient, and watch the CPU meter. It does set
    the status-bar message, but sometimes that doesn't work quite the way I'd
    like, and there's not really any indication from <TeXmacs> that it's in
    the middle of a set of transactions with Juris-M / Zotero. The main
    message to wait and watch for is the one for <markup|Document_complete>.

    <\itemize>
      <item>The reason for this is that for any one of the several commands
      sent by <TeXmacs> to Juris-M / Zotero, it's integration program will
      need to cause the editor to perform a number of integration commands.
      For example, in order to insert the first citation, the document
      preferences may be automatically prompted for and set. When any given
      citation is changed, or a new one inserted into the document, citations
      nearby must be updated with, <with|font-shape|italic|e.g.,> new
      note-indexes, changing to say \P<with|font-shape|italic|id.>\Q or
      \P<with|font-shape|italic|supra>\Q for repeat citations, and then for
      each field needing updating, it must compel the editor to put the
      cursor in that citation field, and then set both the visible text as
      well as the hidden metadata. So once you initiate a command sequence,
      <with|font-shape|italic|i.e.,> <markup|addCitation>, Juris-M takes
      control of <TeXmacs>.

      <\itemize>
        <item>If you like, you can enable the \PDebug: Protocol trace to
        stdout?\Q item on the Zotero menu, and then start <TeXmacs> from a
        shell command prompt. It will print program trace information from
        \Pscaffolding\Q print statements embedded in the program. When you
        see it process \PDocument_complete\Q and remain quiescent, you know
        it's done and ready for the next thing.\ 

        <item>Note that enabling these printouts slows it down. The data
        printed out will vary depending on where I am with development. I
        comment off the printout statements when I no longer require them.
        Similarly, you can use them to inspect the value of variables during
        various stages of the running of this program, to help figure out
        what's wrong when it's not working right.
      </itemize>

      <item>It's a lot faster to work with when you do not put a
      <markup|zbibliography> into the document until very close to the end of
      production, since when there's a large bibliography to format, it takes
      a while each time you enter a new <markup|zcite> or edit one that's
      already in the document.

      <item>For improved performance while editting a long document, you can
      use the menu item \ <menu|Document \<gtr\> Part \<gtr\> Show one part>
      \ or \ <menu|Document \<gtr\> Part \<gtr\> Show several parts> \ to
      narrow the display to only the sections you are working in, thus giving
      the typesetter less work to do, making interactive performance better.
      <tt|tm-zotero> also respects the document part-mode, and so it only
      tells Juris-M / Zotero the list of fields for those within the visible
      parts of the document, thus reducing the size of the data-set that
      Juris-M / Zotero must process before providing the result, as well as
      the number of fields that any new citation or edited citation might
      affect and thus also require updating.
    </itemize>

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

    <\itemize>
      <item>Whether or not citations appear as footnotes (or endnotes) is a
      function of the CSL style chosen. A \Pnote\Q style makes a footnote or
      endnote with the text of the citation within it, and only the normal
      superscript note number at the point of citation within the text. There
      are \Pnumbered\Q styles that do not make a footnote; they instead
      create an in-text label like \P[1]\Q that is repeated in the
      bibliography in front of the item being cited. Later citations to the
      same source will produce the same label in the text, linking to the
      same bibliography item. There are also \Pauthor-date\Q styles that make
      a short textual label in-text, which is then used as the item label in
      the bibliography entry. The <tt|jm-indigobook> style creates an in-text
      full citation. The <tt|jm-indigobook-law-review> style puts virtually
      the same citation into a footnote instead.
    </itemize>

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

    <item>I used the add citation dialog to enter this:
    <zcite|+WoBHoEj0Jm92ki|<tuple|3|<#4954454D2043534C5F4349544154494F4E207B226369746174696F6E4944223A22743142506677595A222C2270726F70657274696573223A7B22666F726D61747465644369746174696F6E223A227B5C5C727466205C5C7A747465787469747B5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944333138377D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D31333636393236313236373333353834333036307D7D7B4164616D7D7320762E2053746174657D2C2032303035205554205C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944333138377D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D31333636393236313236373333353834333036307D7D7B36327D2C20313233205C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944333536347D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D31333636393236313236373333353834333036307D7D7B502E33647D203430302028303423405574616855746168582D582D5820303123405375702E2043742E5375702E2043742E582D582D582032303035292E7D222C22706C61696E4369746174696F6E223A2228636F6E63617420287A737562436974652028636F6E63617420287A747465787469742028636F6E63617420287A744872656646726F6D43697465546F426962205C22237A6269625379734944333138375C22205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D31333636393236313236373333353834333036305C22205C224164616D5C2229205C2273205C22202861626272205C22762E5C2229205C222053746174655C222929205C222C2032303035205554205C2220287A744872656646726F6D43697465546F426962205C22237A6269625379734944333138375C22205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D31333636393236313236373333353834333036305C22205C2236325C2229205C222C20313233205C2220287A744872656646726F6D43697465546F426962205C22237A6269625379734944333536345C22205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D31333636393236313236373333353834333036305C22205C22502E33645C2229205C2220343030202832303035295C22292920287A636974654C61796F7574537566666978205C222E5C222929222C2273757070726573732D747261696C696E672D70756E6374756174696F6E223A66616C73657D2C226369746174696F6E4974656D73223A5B7B226964223A333138372C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224164616D7320762E205374617465222C22636F6E7461696E65722D7469746C65223A225554222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A223632222C22766F6C756D65223A2232303035222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D3133363639323631323637333335383433303630222C226E6F7465223A226D6C7A73796E63313A303035347B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A75745C227D7D222C22697373756564223A7B22726177223A2253657074656D6265722032332C2032303035227D7D2C226C6162656C223A2270616765227D2C7B226964223A333536342C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224164616D7320762E205374617465222C22636F6E7461696E65722D7469746C65223A22502E203364222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A22343030222C22766F6C756D65223A22313233222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D3133363639323631323637333335383433303630222C226E6F7465223A226D6C7A73796E63313A303035347B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A75745C227D7D7B3A6A7572697364696374696F6E3A20557461687D222C22697373756564223A7B22726177223A2253657074656D6265722032332C2032303035227D7D2C226C6162656C223A2270616765227D5D2C22736368656D61223A2268747470733A2F2F6769746875622E636F6D2F6369746174696F6E2D7374796C652D6C616E67756167652F736368656D612F7261772F6D61737465722F63736C2D6369746174696F6E2E6A736F6E227D>|false|<#>>|<zsubCite|<zttextit|<ztHrefFromCiteToBib|#zbibSysID3187|https://scholar.google.com/scholar_case?case=13669261267335843060|Adam>s
    <abbr|v.> State>, 2005 UT <ztHrefFromCiteToBib|#zbibSysID3187|https://scholar.google.com/scholar_case?case=13669261267335843060|62>,
    123 <ztHrefFromCiteToBib|#zbibSysID3564|https://scholar.google.com/scholar_case?case=13669261267335843060|P.3d>
    400 (2005)><zciteLayoutSuffix|.>> Notice that this is a <em|parallel
    citation>. There are two entries in the Juris-M reference database for
    the same case. I cite one then the other immediately after it in the same
    reference cluster, and it outputs as a parallel citation. Citing more
    than one reference in a citation cluster is always possible. It's only
    when consecutive items within the citation cluster are of the same
    <em|legal case> that they will be collapsed into a parallel citation.

    <item><zcite|+ZKhKfHGkvinH8g|<tuple|3|<#4954454D2043534C5F4349544154494F4E207B226369746174696F6E4944223A22327870385561456B222C2270726F70657274696573223A7B22666F726D61747465644369746174696F6E223A227B5C5C727466205C5C7A747465787469747B5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944323237357D7B5C5C706174687B687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D393535303433333132363236393637343531397D7D7B427261647D7920762E204D6172796C616E647D2C2033373320555320383320283031234055535553582D582D5820303123405375702E2043742E5375702E2043742E582D582D582031393633293B205C5C7A747465787469747B5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944313432307D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F713D556E697465642B5374617465732B762B476F6C75625C5C26686C3D656E5C5C2661735C5C5F7364743D362C34355C5C26636173653D383133313237353636393038393834373439315C5C267363696C683D307D7D7B556E69747D65642053746174657320762E20476F6C75627D2C2036393420462E326420323037202830322E3130234031307468204369722E203034234043742E206F66204170702E2031393832293B205C5C7A747465787469747B5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944313334327D7B5C5C706174687B687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F713D4D6F6F6E65792B762B486F6C6F68616E5C5C26686C3D656E5C5C2661735C5C5F7364743D362C34355C5C26636173653D31303535333236373632313939343434323331305C5C267363696C683D307D7D7B4D6F6F6E7D657920762E20486F6C6F68616E7D2C203239342055532031303320283031234055535553582D582D5820303123405375702E2043742E5375702E2043742E582D582D582031393335292E7D222C22706C61696E4369746174696F6E223A2228636F6E63617420287A737562436974652028636F6E63617420287A747465787469742028636F6E63617420287A744872656646726F6D43697465546F426962205C22237A6269625379734944323237355C22205C22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D393535303433333132363236393637343531395C22205C22427261645C2229205C2279205C22202861626272205C22762E5C2229205C22204D6172796C616E645C222929205C222C20333733205553203833202831393633295C22292920287A636974654C61796F757444656C696D69746572205C223B205C222920287A737562436974652028636F6E63617420287A747465787469742028636F6E63617420287A744872656646726F6D43697465546F426962205C22237A6269625379734944313432305C22205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D556E697465642B5374617465732B762B476F6C756226686C3D656E2661735F7364743D362C343526636173653D38313331323735363639303839383437343931267363696C683D305C22205C22556E69745C2229205C22656420537461746573205C22202861626272205C22762E5C2229205C2220476F6C75625C222929205C222C2036393420462E326420323037202831307468205C22202861626272205C224369722E5C2229205C22205C22202861626272205C2243742E5C2229205C22206F66205C22202861626272205C224170702E5C2229205C222031393832295C22292920287A636974654C61796F757444656C696D69746572205C223B205C222920287A737562436974652028636F6E63617420287A747465787469742028636F6E63617420287A744872656646726F6D43697465546F426962205C22237A6269625379734944313334325C22205C22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D4D6F6F6E65792B762B486F6C6F68616E26686C3D656E2661735F7364743D362C343526636173653D3130353533323637363231393934343432333130267363696C683D305C22205C224D6F6F6E5C2229205C226579205C22202861626272205C22762E5C2229205C2220486F6C6F68616E5C222929205C222C2032393420555320313033202831393335295C22292920287A636974654C61796F7574537566666978205C222E5C222929227D2C226369746174696F6E4974656D73223A5B7B226964223A323237352C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5556543651445455225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5556543651445455225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A22427261647920762E204D6172796C616E64222C22636F6E7461696E65722D7469746C65223A225553222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A223833222C22766F6C756D65223A22333733222C2255524C223A22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D39353530343333313236323639363734353139222C226E6F7465223A226D6C7A73796E63313A303035317B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275735C227D7D3030303030222C22697373756564223A7B22726177223A2231393633227D2C226163636573736564223A7B22726177223A22323031342D31322D31325430363A34323A30385A227D7D7D2C7B226964223A313432302C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5544333237343552225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5544333237343552225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A22556E697465642053746174657320762E20476F6C7562222C22636F6E7461696E65722D7469746C65223A22462E203264222C22617574686F72697479223A22636F7572742E61707065616C73222C2270616765223A22323037222C22766F6C756D65223A22363934222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D556E697465642B5374617465732B762B476F6C756226686C3D656E2661735F7364743D362C343526636173653D38313331323735363639303839383437343931267363696C683D30222C226E6F7465223A226D6C7A73796E63313A303035357B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A6331305C227D7D3030303030222C22697373756564223A7B22726177223A22446563656D62657220312C2031393832227D2C226163636573736564223A7B22726177223A22323031352D30342D30315432303A35303A34365A227D7D7D2C7B226964223A313334322C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3239524A47495143225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3239524A47495143225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224D6F6F6E657920762E20486F6C6F68616E222C22636F6E7461696E65722D7469746C65223A225553222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A22313033222C22766F6C756D65223A22323934222C2255524C223A22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D4D6F6F6E65792B762B486F6C6F68616E26686C3D656E2661735F7364743D362C343526636173653D3130353533323637363231393934343432333130267363696C683D30222C226E6F7465223A226D6C7A73796E63313A303035317B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275735C227D7D3030303030222C22697373756564223A7B22726177223A224A616E756172792032312C2031393335227D2C226163636573736564223A7B22726177223A22323031352D30332D30345432333A31393A33335A227D7D7D5D2C22736368656D61223A2268747470733A2F2F6769746875622E636F6D2F6369746174696F6E2D7374796C652D6C616E67756167652F736368656D612F7261772F6D61737465722F63736C2D6369746174696F6E2E6A736F6E227D>|false|<#>>|<zsubCite|<zttextit|<ztHrefFromCiteToBib|#zbibSysID2275|http://scholar.google.com/scholar_case?case=9550433126269674519|Brad>y
    <abbr|v.> Maryland>, 373 US 83 (1963)><zciteLayoutDelimiter|;
    ><zsubCite|<zttextit|<ztHrefFromCiteToBib|#zbibSysID1420|https://scholar.google.com/scholar_case?q=United+States+v+Golub&hl=en&as_sdt=6,45&case=8131275669089847491&scilh=0|Unit>ed
    States <abbr|v.> Golub>, 694 F.2d 207 (10th <abbr|Cir.> <abbr|Ct.> of
    <abbr|App.> 1982)><zciteLayoutDelimiter|;
    ><zsubCite|<zttextit|<ztHrefFromCiteToBib|#zbibSysID1342|http://scholar.google.com/scholar_case?q=Mooney+v+Holohan&hl=en&as_sdt=6,45&case=10553267621994442310&scilh=0|Moon>ey
    <abbr|v.> Holohan>, 294 US 103 (1935)><zciteLayoutSuffix|.>> Notice that
    in this citation cluster, the first four characters of each citation are
    a hyperlink to the bibliography entry that corresponds with that
    citation. That link appears only when the document has a
    <markup|zbibliography>, since otherwise, the link would not have a valid
    target.

    <item>Inside the <markup|zbibliography> (at the end of this document)
    you'll find that some of the entries have the first four characters as a
    hyperlink to the on-line URL associated with that Juris-M/Zotero
    reference database entry. Whatever URL you place in that entry's \PURL\Q
    field will appear here as the target of the hyperlinked first four
    characters of the bibliography item. The same hyperlink will appear in
    the resulting PDF document. It is possible to create relative links to
    other PDF documents stored near the final PDF of the document being
    created. <todo|Someday there ought to be a way to fix up links to
    attachments or exhibits<math|\<ldots\>>>

    <item>You may insert a bibliography using either the menu, or the
    <key|\\zbibliography> or <key|\\zb> keyboard shortcuts.

    <item>When the cursor is just to the right of a citation (or just inside
    of the \Pzbibliography\Q), you can:

    <\itemize>
      <item>press the <key|Tab> key to call <markup|assertCitation>, or to
      open the bibliography edit dialogue; press <key|Ctrl-Enter> to call
      <markup|editCitation> or <markup|editBibliography>.

      <item>press <key|Shift-space> to toggle the suppress trailing
      punctuation flag for that zcite.

      <item>press <key|Backspace> to disactivate the tag, which will allow
      you to move the cursor inside of it to:

      <\itemize>
        <item>Make arbitrary edits, which will change the flag from green to
        red, and next time you invoke Juris-M / Zotero integration for that
        zcite, it will prompt you with the question of whether to overwrite
        your by-hand modification, or to keep it.

        <item>Cut, Copy, or Paste sub-cite's. When you highlight one or more
        <key|\\zsubCite>'s within a disactivated <key|\\zcite>, you can
        clipboard-cut, clipboard-copy, or clipboard-paste them. You can
        delete them from the citation cluster, cut them out of this zcite,
        and then paste them as zcite's into the primary text, or paste them
        into another disactivated zcite, adding them to that citation
        cluster. This is faster and more convenient than using copy + paste
        to make a copy of a zcite, then using <menu|editCitation> to remove
        some from the first one, and the others from the second one, in order
        to split a zcite cluster into multiple zcite's. Instead, just
        disactivate the zcite, cursor-arrow in, use <key|C-SPC> to set the
        \Pmark\Q, cursor-arrows to widen the selection to encompass one or
        more <key|\\zsubCite>'s, <key|C-w> to \Pwipe\Q, <key|Enter> to
        reactivate that tag, cursor-arrow over to where you want to paste,
        then <key|C-y> to \Pyank\Q the primary clipboard contents, inserting
        a zcite with the subcite's you just cut from the other one, or,
        alternatively, cursor-arrow over to another zcite, use
        <key|Backspace> to disactivate it, cursor-arrow inside of it, then
        <key|C-y> to paste those subcite's in to become part of this zcite.
      </itemize>
    </itemize>

    <item>You will also see that when a <markup|zcite> or
    <markup|zbibliography> is in focus, there are settings available for each
    citation when you click the \Pwrench\Q icon. You can make your
    bibliography 1 or 2 columns, you can set it's relative text size, and you
    can control whether or not to render hyperlinks or ref-lists.

    <item>The appearance of the ref-list can be modified by changing the
    <markup|zbibItemRef> macro. Look in the preamble to this document for an
    example, and also find it in the <tmpackage|tm-zotero.ts> file. The
    default assumes that you have sections in the document.

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

      <item><very-small|<tt|000000000@#\\ztbibSubHeading{!Title of Subheading
      Here}>> or \ \ <very-small|<tt|000000000@#\<less\>ztbibSubHeading\<gtr\>Title
      of Subheading Here\<less\>/ztbibSubHeading\<gtr\>>> is for creating a
      category subheading in your bibliography. Notice that there are exactly
      9 zeros, followed by <verbatim|@#>. It is used in the <tt|title> field
      in the Juris-M / Zotero database. The zeros make it sort to the top of
      the list of titles in it's sort category. The item type should be set
      to an appropriate type to match one of the item types for the
      particular category. For legal cases, you must also set the
      jurisdiction and court in order to make the subheading appear in the
      right part of the bibliography<math|\<ldots\>> it all depends on the
      (possibly categorizing) sort macro inside the CSL style file you are
      using. You <em|may> prefix that with a sorting-prefix,
      <with|font-shape|italic|e.g.,> \P<verbatim|01#@>\Q, as described above,
      to fine-tune the sort order to ensure that the heading appears where it
      belongs in your completed bibliography.

      <\itemize>
        <item>Notice the <tt|!> which preserves the preceding <tt|{> which
        would otherwise be \Pescaped\Q before being sent to <TeXmacs> in
        order to <em|prevent> it from being parsed as a <LaTeX> syntactic
        element, for the braces that surround the argument to a macro.
        Normally we want certain characters to be escaped, so we can write
        them as we please into bibliography annotations or titles, but
        sometimes we might want to pass <LaTeX> through to <TeXmacs> in this
        fashion to achieve special effects.

        <item>The <tt|\\ztbibSubHeading> macro is defined in the
        <tt|legal-brief.ts> style. You may visit that file to see what it
        looks like, and you may redefine it in your document preamble if you
        like. (There <em|are> other \Psettings\Q sort of macros to be found
        in there that can also be overrided in your document preamble when
        you need that.)

        <\itemize>
          <item>A <tt|\\ztbibSubHeading> type item <em|may> have a URL or DOI
          associated with it. If it does, then the first 4 characters will be
          made into a hyperlink to that URL or DOI, just as with other items
          in the zbibliography.

          <item>The parsing code inside of <tt|zotero.scm> looks for the
          regexp <tt|/.ztbib[A-Za-z]+/> and keeps whatever macro that is. It
          must begin with \P<tt|\\ztbib>\Q for it to work, but you may define
          whatever macro you like to use for that.

          <item>If you see an error in the bibliography where there's
          something that looks like <with|color|red|<verbatim|\<less\>ztbibSnowFooting\<gtr\>>>
          then that means that macro is not defined. Look for a typo in the
          Juris-M / Zotero database entry, or define the macro in your
          preamble or style package.
        </itemize>
      </itemize>

      <item>Those special entries are designed to work with the included
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

  <\zbibliography|+DvR2qZelkLrxq7|<tuple|3|<#4249424C207B22756E6369746564223A5B5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F455A514637494350225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F43324345334A3736225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E32363251564339225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4733525535423755225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3242373434343643225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A545A355A465648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F384A455038355335225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F47374E55434B5655225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4851353645463637225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4252433852454753225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F41474246384B5A47225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F39414E4235464135225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4A5256514E48544B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F47584D5643445451225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D3639534D55334B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4542464A33545834225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5747535635584A53225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D52465653325347225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4741585038325A38225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4947475042494A42225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B57584D53374341225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3333334A34543357225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3638414B505A4B44225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3541555045504D53225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B48444849524E45225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5037453952525237225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F374341354B374332225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56553452494D4238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E385339554D4453225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F543542485633354B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F52483444484E3436225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3849425341534B46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3753453537465033225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4447384945325046225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F584B465547474E52225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F333856454A335850225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3535554A42334E58225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5443534D344E3648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F46373333394B4544225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3752434B52384947225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3532325356345353225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5258374444554B51225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3350343842354D43225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5056415452443748225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4546374E5036484A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F414856354E455057225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524133374B455657225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F363452564B324D46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5856584638414B39225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4733565042575836225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F44535A345A374B56225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5751363645443534225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3757495639374949225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3536524146385356225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3242395852325243225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F36344A3544483738225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F573541343555514D225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5244363944383254225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F363252464B4B3738225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E545A33564D5248225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33364A5446534951225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56435A3357484851225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5452493857364A39225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5745585034353739225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3542555A49553332225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4541555357354253225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F494B423435503933225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F43524A4A41455452225D5D2C22637573746F6D223A5B5D7D2043534C5F4249424C494F475241504859>|false|<#>>>
    <ztbibSubHeading|United States Supreme Court Cases>\ 

    <ztbibItemText|1252||sysID1252|<zttextit|<ztHrefFromBibToURL|#zbibSysID1252|http://scholar.google.com/scholar_case?q=Blakely+v+Washington&hl=en&as_sdt=6,45&case=16163203473167624369&scilh=0|Blak>ely
    <abbr|v.> Washington>, 542 US 296 (2004).>

    <ztbibItemText|2275||sysID2275|<zttextit|<ztHrefFromBibToURL|#zbibSysID2275|http://scholar.google.com/scholar_case?case=9550433126269674519|Brad>y
    <abbr|v.> Maryland>, 373 US 83 (1963).>

    <ztbibItemText|211||sysID211|<zttextit|<ztHrefFromBibToURL|#zbibSysID211|https://scholar.google.com/scholar_case?q=489+us+189&hl=en&as_sdt=6,45&case=5543768239799414902&scilh=0|DeSh>aney
    <abbr|v.> Winnebago County Dept. of Social Servs.>, 489 US 189 (1989).>

    <ztbibItemText|2239||sysID2239|<zttextit|<ztHrefFromBibToURL|#zbibSysID2239|http://scholar.google.com/scholar_case?case=2659052629576231238&q=407+S.+514,+532+(1972)&hl=en&as_sdt=6,45&scilh=0|Dogg>ett
    <abbr|v.> United States>, 505 US 647 (1992).>

    <ztbibItemText|1620||sysID1620|<zttextit|<ztHrefFromBibToURL|#zbibSysID1620|https://scholar.google.com/scholar_case?case=3973384553826466817|Doug>las
    <abbr|v.> California>, 372 US 353 (1963).>

    <ztbibItemText|3216||sysID3216|<zttextit|<ztHrefFromBibToURL|#zbibSysID3216|https://scholar.google.com/scholar_case?q=ex+parte+Young&hl=en&as_sdt=6,45&case=15822732193533819720&scilh=0|ex
    p>arte Young>, 209 US 123 (1908).>

    <ztbibItemText|1170||sysID1170|<zttextit|<ztHrefFromBibToURL|#zbibSysID1170|http://scholar.google.com/scholar_case?q=528+us+167&hl=en&as_sdt=3,45&case=5440560917097220943&scilh=0|Frie>nds
    of Earth, <abbr|Inc.> <abbr|v.> Laidlaw Environmental Services (TOC),
    <abbr|Inc.>>, 528 US 167 (2000).>

    <ztbibItemText|2309||sysID2309|<zttextit|<ztHrefFromBibToURL|#zbibSysID2309|http://scholar.google.com/scholar_case?case=12450678889272734206&q=related:b7CW8KJyhJsJ:scholar.google.com/&hl=en&as_sdt=6,45|Gigl>io
    <abbr|v.> United States>, 405 US 150 (1972).>

    <ztbibItemText|325||sysID325|<zttextit|<ztHrefFromBibToURL|#zbibSysID325|https://scholar.google.com/scholar_case?case=3245341013213844183&q=attainder|Join>t
    Anti-Fascist Refugee Comm. <abbr|v.> McGrath>, 341 US 123 (1951).>

    <ztbibItemText|1342||sysID1342|<zttextit|<ztHrefFromBibToURL|#zbibSysID1342|http://scholar.google.com/scholar_case?q=Mooney+v+Holohan&hl=en&as_sdt=6,45&case=10553267621994442310&scilh=0|Moon>ey
    <abbr|v.> Holohan>, 294 US 103 (1935).>

    <ztbibItemText|3333||sysID3333|<zttextit|<ztHrefFromBibToURL|#zbibSysID3333|https://scholar.google.com/scholar_case?q=Wall+v+Kholi&hl=en&as_sdt=6,45&case=13423175293489651929&scilh=0|Wall>
    <abbr|v.> Kholi>, 131 <abbr|S.> <abbr|Ct.> 1278 (2011).>

    <ztbibItemText|93||sysID93|<zttextit|<ztHrefFromBibToURL|#zbibSysID93|https://scholar.google.com/scholar_case?q=Gonzales+307+f.3d&hl=en&as_sdt=6,45&case=1586392809091968224&scilh=0|Gonz>ales
    <abbr|v.> City of Castle Rock>, 307 F.3d 1258 (10th <abbr|Cir.>
    <abbr|Ct.> of <abbr|App.> 2002).>

    <ztbibItemText|1391||sysID1391|<zttextit|<ztHrefFromBibToURL|#zbibSysID1391|http://scholar.google.com/scholar_case?case=3104253368109472968&q=United+States+v.+Cronic&hl=en&as_sdt=6,45&scilh=0|Unit>ed
    States <abbr|v.> Golub>, 638 F.2d 185 (10th <abbr|Cir.> <abbr|Ct.> of
    <abbr|App.> 1980).>

    <ztbibItemText|1420||sysID1420|<zttextit|<ztHrefFromBibToURL|#zbibSysID1420|https://scholar.google.com/scholar_case?q=United+States+v+Golub&hl=en&as_sdt=6,45&case=8131275669089847491&scilh=0|Unit>ed
    States <abbr|v.> Golub>, 694 F.2d 207 (10th <abbr|Cir.> <abbr|Ct.> of
    <abbr|App.> 1982).>

    <ztbibSubHeading|Other Jurisdictions' Court Cases>\ 

    <ztbibItemText|230||sysID230|<zttextit|<ztHrefFromBibToURL|#zbibSysID230|https://scholar.google.com/scholar_case?about=13788143762591333722|Wrig>ht
    <abbr|v.> Wright>, 54 NY 437 (<abbr|N.Y.> <abbr|Ct.> of <abbr|App.>
    1873).>

    <ztbibItemText|3468||sysID3468|<zttextit|<abbr|Dr.> Bonham's Case 1861>,
    (1861) 77 ER 646 (Masters).>

    <ztbibSubHeading|Utah Supreme Court Cases>\ 

    <ztbibItemText|3187||sysID3187|<zttextit|<ztHrefFromBibToURL|#zbibSysID3187|https://scholar.google.com/scholar_case?case=13669261267335843060|Adam>s
    <abbr|v.> State>, 2005 UT 62., <ztHrefFromBibToURL|#zbibSysID3564|https://scholar.google.com/scholar_case?case=13669261267335843060|123>
    P.3d 400 (2005).>

    <ztbibItemText|101||sysID101|<zttextit|<ztHrefFromBibToURL|#zbibSysID101|http://scholar.google.com/scholar_case?case=14333925206835369542&q=state+v++rees&hl=en&as_sdt=4,45|Stat>e
    <abbr|v.> Rees>, 125 P.3d 874 (2005).>

    <ztbibSubHeading|Utah Court of Appeals Cases>\ 

    <ztbibItemText|1356||sysID1356|<zttextit|<ztHrefFromBibToURL|#zbibSysID1356|http://scholar.google.com/scholar_case?case=2513784522765968505|Stat>e
    <abbr|v.> Hegbloom>, 2014 UT App 213 (Utah <abbr|Ct.> of <abbr|App.>
    2014).>

    <ztbibItemText|131||sysID131|<zttextit|<ztHrefFromBibToURL|#zbibSysID131|http://scholar.google.com/scholar_case?case=17234797921577956966&q=state+v++rees+20010490-CA+coram+nobis&hl=en&as_sdt=4,45|Stat>e
    <abbr|v.> Rees>, 63 P.3d 120 (Utah <abbr|Ct.> of <abbr|App.> 2003).>

    <ztbibItemText|1523||sysID1523|<zttextit|<ztHrefFromBibToURL|#zbibSysID1523|https://scholar.google.com/scholar_case?case=11195558326880357574|Stat>e
    <abbr|v.> Sery>, 758 P.2d 935 (Utah <abbr|Ct.> of <abbr|App.> 1988).>

    <ztbibItemText|920||sysID920|<zttextit|Wiscombe <abbr|v.> Wiscombe>, 744
    P.2d 1024 (Utah <abbr|Ct.> of <abbr|App.> 1987).>

    <ztbibSubHeading|Other Court Cases>\ 

    <ztbibItemText|212||sysID212|<zttextit|<ztHrefFromBibToURL|#zbibSysID212|https://scholar.google.com/scholar_case?case=13190905168500779696|Comm>issioner
    of Probation <abbr|v.> Adams>, 65 <abbr|<abbr|Mass.>> <abbr|<abbr|App.>>
    <abbr|<abbr|Ct.>> 725 (2006).>

    <ztbibItemText|1166||sysID1166|<zttextit|<ztHrefFromBibToURL|#zbibSysID1166|http://scholar.google.com/scholar_case?case=17563102041873563785&q=v+van+pelt&hl=en&as_sdt=6,45|Wall>ace
    <abbr|v.> Van Pelt>, 969 S.W.2d 380 (Mo. <abbr|Ct.> of <abbr|App.>
    1998).>

    <ztbibSubHeading|United States Code>\ 

    <ztbibItemText|3434||sysID3434|<zttextit|<ztHrefFromBibToURL|#zbibSysID3434|https://www.law.cornell.edu/uscode/text/18/1346|Defi>nition
    of \Pscheme or artifice to defraud,\Q> 18 US Code
    <SectionSignGlyph|><space|0.5spc>1346.>

    <ztbibItemText|3656||sysID3656|<zttextit|<ztHrefFromBibToURL|#zbibSysID3656|https://www.law.cornell.edu/uscode/text/42/402|Old->age
    and survivors insurance benefit payments>, 42 US Code
    <SectionSignGlyph|><space|0.5spc>402.>

    <ztbibSubHeading|<ztHrefFromBibToURL|#zbibSysID3571|http://le.utah.gov/xcode/Constitution.html|Cons>titution
    of Utah>\ 

    <ztbibItemText|3474||sysID3474|<zttextit|<ztHrefFromBibToURL|#zbibSysID3474|http://le.utah.gov/xcode/ArticleI/Article_I,_Section_11.html|Cour>ts
    open<emdash>Redress of injuries>, Utah <abbr|Const.> <abbr|Art.> I,
    <SectionSignGlyph|><space|0.5spc>11.>

    <ztbibItemText|374||sysID374|<zttextit|<ztHrefFromBibToURL|#zbibSysID374|http://le.utah.gov/code/CONST/htm/00I01_001200.htm|Righ>ts
    of accused persons>, Utah <abbr|Const.> <abbr|Art.> I,
    <SectionSignGlyph|><space|0.5spc>12.>

    <ztbibItemText|3471||sysID3471|<zttextit|<ztHrefFromBibToURL|#zbibSysID3471|http://le.utah.gov/xcode/ArticleI/Article_I,_Section_24.html|Unif>orm
    operation of law>, Utah <abbr|Const.> <abbr|Art.> I,
    <SectionSignGlyph|><space|0.5spc>24.>

    <ztbibItemText|3469||sysID3469|<zttextit|<ztHrefFromBibToURL|#zbibSysID3469|http://le.utah.gov/xcode/ArticleI/Article_I,_Section_27.html|Fund>amental
    rights>, Utah <abbr|Const.> <abbr|Art.> I,
    <SectionSignGlyph|><space|0.5spc>27.>

    <ztbibItemText|3966||sysID3966|<zttextit|<ztHrefFromBibToURL|#zbibSysID3966|http://le.utah.gov/xcode/ArticleVI/Article_VI,_Section_26.html|Priv>ate
    laws forbidden>, Utah <abbr|Const.> <abbr|Art.> VI,
    <SectionSignGlyph|><space|0.5spc>26.>

    <ztbibSubHeading|Utah Code>\ 

    <ztbibItemText|3427||sysID3427|<zttextit|<ztHrefFromBibToURL|#zbibSysID3427|http://le.utah.gov/xcode/Title68/Chapter3/68-3.html?v=C68-3_1800010118000101|Cons>truction>,
    Utah Code <SectionSignGlyph|><space|0.5spc>68-3 (1953).>

    <ztbibItemText|3429||sysID3429|<zttextit|<ztHrefFromBibToURL|#zbibSysID3429|http://le.utah.gov/xcode/Title68/Chapter3/68-3-S2.html?v=C68-3-S2_1800010118000101|Stat>utes
    in derogation of common law not strictly construed<emdash>Rules of equity
    prevail>, Utah Code <SectionSignGlyph|><space|0.5spc>68-3-2 (2010).>

    <ztbibItemText|3990||sysID3990|<zttextit|<ztHrefFromBibToURL|#zbibSysID3990|http://le.utah.gov/xcode/Title76/Chapter1/76-1-S104.html?v=C76-1-S104_1800010118000101|Purp>oses
    and principles of construction>, Utah Code
    <SectionSignGlyph|><space|0.5spc>76-1-104 (1973).>

    <ztbibItemText|3457||sysID3457|<zttextit|<ztHrefFromBibToURL|#zbibSysID3457|http://le.utah.gov/xcode/Title77/Chapter7/77-7-S3.html?v=C77-7-S3_1800010118000101|Arre>st
    by private persons>, Utah Code <SectionSignGlyph|><space|0.5spc>77-7-3
    (1980).>

    <ztbibItemText|3613||sysID3613|<zttextit|<ztHrefFromBibToURL|#zbibSysID3613|http://le.utah.gov/xcode/Title78B/Chapter7/78B-7-S115.html?v=C78B-7-S115_2016051020160510|Dism>issal
    of Protective Order>, Utah Code <SectionSignGlyph|><space|0.5spc>78B-7-115
    (2016).>

    <ztbibItemText|4002||sysID4002|<zttextit|<ztHrefFromBibToURL|#zbibSysID4002|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9.html?v=C78B-9_1800010118000101|Post>-conviction
    remedies act>, Utah Code <SectionSignGlyph|><space|0.5spc>78B-9 (2008).>

    <ztbibItemText|4008||sysID4008|<zttextit|<ztHrefFromBibToURL|#zbibSysID4008|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9-S106.html|Prec>lusion
    of relief<emdash>Exception>, Utah Code
    <SectionSignGlyph|><space|0.5spc>78B-9-106 (2008).>

    <ztbibItemText|4009||sysID4009|<zttextit|<ztHrefFromBibToURL|#zbibSysID4009|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9-S107.html|Stat>ute
    of limitations for postconviction relief>, Utah Code
    <SectionSignGlyph|><space|0.5spc>78B-9-107 (2008).>

    <ztbibItemText|4006||sysID4006|<zttextit|<ztHrefFromBibToURL|#zbibSysID4006|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9-S401.5.html|Defi>nitions>,
    Utah Code <SectionSignGlyph|><space|0.5spc>78B-9-401.5 (2008).>

    <ztbibSubHeading|<ztHrefFromBibToURL|#zbibSysID3576|https://www.utcourts.gov/resources/rules/urcp/index.htm|Utah>
    Rules of Civil Procedure>\ 

    <ztbibItemText|3625||sysID3625|<zttextit|<ztHrefFromBibToURL|#zbibSysID3625|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp011.html|Sign>ing
    of pleadings, motions, and other papers; representations to court;
    sanctions>, URCP 11.>

    <ztbibItemText|3733||sysID3733|<zttextit|<ztHrefFromBibToURL|#zbibSysID3733|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp052.html|Find>ings
    by the court; correction of the record>, URCP 52.>

    <ztbibItemText|3735||sysID3735|<zttextit|<ztHrefFromBibToURL|#zbibSysID3735|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp059.html|New
    >trial; altering or amending a judgment>, URCP 59.>

    <ztbibItemText|3756||sysID3756|<zttextit|<ztHrefFromBibToURL|#zbibSysID3756|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp060.html|Reli>ef
    from judgment or order>, URCP 60.>

    <ztbibItemText|3959||sysID3959|<zttextit|<ztHrefFromBibToURL|#zbibSysID3959|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp065C.html|Post>-conviction
    Relief>, URCP 65C.>

    <ztbibSubHeading|<ztHrefFromBibToURL|#zbibSysID3627|https://www.utcourts.gov/resources/rules/urcrp/|Utah>
    Rules of Criminal Procedure>\ 

    <ztbibItemText|3461||sysID3461|<zttextit|<ztHrefFromBibToURL|#zbibSysID3461|http://www.utcourts.gov/resources/rules/urcrp/view.html?rule=URCRP07.html|Proc>eedings
    before a magistrate>, URCrP 7.>

    <ztbibSubHeading|<ztHrefFromBibToURL|#zbibSysID3577|https://www.utcourts.gov/resources/rules/urap/index.htm|Utah>
    Rules of Appellate Procedure>\ 

    <ztbibItemText|3421||sysID3421|<zttextit|<ztHrefFromBibToURL|#zbibSysID3421|http://www.utcourts.gov/resources/rules/urap/view.html?rule=03.htm|Appe>al
    as of right: how taken>, URAP 3 (2015).>

    <ztbibItemText|3440||sysID3440|<zttextit|<ztHrefFromBibToURL|#zbibSysID3440|http://www.utcourts.gov/resources/rules/urap/view.html?rule=04.htm|Appe>al
    as of right: when taken>, URAP 4 (2015).>

    <ztbibItemText|3484||sysID3484|<zttextit|<ztHrefFromBibToURL|#zbibSysID3484|http://www.utcourts.gov/resources/rules/urap/view.html?rule=05.htm|Disc>retionary
    appeals of interlocutory orders>, URAP 5 (2014).>

    <ztbibItemText|3955||sysID3955|<zttextit|<ztHrefFromBibToURL|#zbibSysID3955|http://www.utcourts.gov/resources/rules/urap/view.html?rule=10.htm|Moti>on
    for Summary Disposition>, URAP 10 (2012).>

    <ztbibItemText|3482||sysID3482|<zttextit|<ztHrefFromBibToURL|#zbibSysID3482|http://www.utcourts.gov/resources/rules/urap/view.html?rule=11.htm|The
    >record on appeal>, URAP 11 (2015).>

    <ztbibItemText|3486||sysID3486|<zttextit|<ztHrefFromBibToURL|#zbibSysID3486|http://www.utcourts.gov/resources/rules/urap/view.html?rule=19.htm|Extr>aordinary
    writs>, URAP 19 (2015).>

    <ztbibItemText|3603||sysID3603|<zttextit|<ztHrefFromBibToURL|#zbibSysID3603|http://www.utcourts.gov/resources/rules/urap/view.html?rule=20.htm|Habe>as
    corpus proceedings>, URAP 20 (2012).>

    <ztbibItemText|3748||sysID3748|<zttextit|<ztHrefFromBibToURL|#zbibSysID3748|http://www.utcourts.gov/resources/rules/urap/view.html?rule=24.htm|Brie>fs>,
    URAP 24 (2015).>

    <ztbibSubHeading|<ztHrefFromBibToURL|#zbibSysID4019|https://www.utcourts.gov/resources/rules/ucja/index.html#Chapter_12|Utah>
    Code of Judicial Conduct>\ 

    <ztbibItemText|4020||sysID4020|<zttextit|<ztHrefFromBibToURL|#zbibSysID4020|https://www.utcourts.gov/resources/rules/ucja/#Chapter_13|Rule>s
    of Professional Conduct>, UCJA <abbr|Ch.> 13.>

    <ztbibItemText|4022||sysID4022|<zttextit|<ztHrefFromBibToURL|#zbibSysID4022|http://www.utcourts.gov/resources/rules/ucja/view.html?rule=ch13/1_0.htm|Term>inology>,
    UCJA <abbr|ch.> 13, Rule 1.0 (2015).>

    <ztbibSubHeading|Utah Legislature Bills>\ 

    <ztbibItemText|3285||sysID3285|<ztHrefFromBibToURL|#zbibSysID3285|http://le.utah.gov/~2013/bills/static/SB0263.html|SB02>63
    SB0263, Utah State Senate (2013).>

    <ztbibSubHeading|Books, Journal Articles, and Other Documents>\ 

    <ztbibItemText|1451||sysID1451|<ztHrefFromBibToURL|#zbibSysID1451|http://www.saveservices.org/wp-content/uploads/SAVE-Assault-Civil-Rights.pdf|SAVE>,
    An Assault Upon Our Civil Rights (Stop Abusive and Violent Environments
    2013).>
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
    <associate|page-bot|1in>
    <associate|page-even|1in>
    <associate|page-height|auto>
    <associate|page-medium|paper>
    <associate|page-odd|1in>
    <associate|page-right|1in>
    <associate|page-screen-margin|false>
    <associate|page-top|1in>
    <associate|page-type|letter>
    <associate|page-width|auto>
    <associate|preamble|false>
    <associate|section-display-numbers|false>
    <associate|zbibPageBefore|1>
    <associate|zotero-BibliographyStyle_arrayList|<tuple>>
    <associate|zotero-BibliographyStyle_bodyIndent|0.0000tab>
    <associate|zotero-BibliographyStyle_entrySpacing|1.0000>
    <associate|zotero-BibliographyStyle_firstLineIndent|0.0000tab>
    <associate|zotero-BibliographyStyle_lineSpacing|1.0000>
    <associate|zotero-BibliographyStyle_tabStopCount|0>
    <associate|zotero-data-data-version|3>
    <associate|zotero-data-zotero-version|4.0.29.12m98>
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
    <associate|zotero-pref-projectName|tm-zotero-tutorial-cited>
    <associate|zotero-pref-storeReferences|true>
    <associate|zotero-pref-suppressTrailingPunctuation|false>
    <associate|zotero-session-id|uCcnCMzc>
    <associate|zotero-style-bibliographyStyleHasBeenSet|1>
    <associate|zotero-style-citation-layout-delimiter|; >
    <associate|zotero-style-citation-layout-prefix|>
    <associate|zotero-style-citation-layout-suffix|.>
    <associate|zotero-style-hasBibliography|1>
    <associate|zotero-style-id|http://juris-m.github.io/styles/jm-indigobook-catsort-bib>
    <associate|zotero-style-locale|en-US>
    <associate|zoteroDocumentData|\<data data-version="3"
    zotero-version="4.0.29.12m98"\>\<session id="uCcnCMzc"/\>\<style
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
    name="projectName" value="tm-zotero-tutorial-cited"/\>\<pref
    name="extractingLibraryID" value="0"/\>\<pref
    name="extractingLibraryName" value="No group selected"/\>\<pref
    name="fieldType" value="ReferenceMark"/\>\<pref name="storeReferences"
    value="true"/\>\<pref name="automaticJournalAbbreviations"
    value="true"/\>\<pref name="noteType" value="0"/\>\<pref
    name="suppressTrailingPunctuation" value="false"/\>\</prefs\>\</data\>>
    <associate|zt-option-zbib-zt-wrap-with-page-break-before|true>
    <associate|zt-render-bibItemRefsLists|false>
  </collection>
</initial>

<\references>
  <\collection>
    <associate|auto-1|<tuple|1|1>>
    <associate|auto-10|<tuple|<with|mode|<quote|math>|<rigid|\<circ\>>>|5>>
    <associate|auto-11|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|5>>
    <associate|auto-12|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|6>>
    <associate|auto-13|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|6>>
    <associate|auto-14|<tuple|<with|mode|<quote|math>|\<bullet\>>|8>>
    <associate|auto-15|<tuple|<with|mode|<quote|math>|\<bullet\>>|8>>
    <associate|auto-16|<tuple|1|9>>
    <associate|auto-2|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|2>>
    <associate|auto-3|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|2>>
    <associate|auto-4|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|3>>
    <associate|auto-5|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|3>>
    <associate|auto-6|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-7|<tuple|<with|mode|<quote|math>|\<bullet\>>|3>>
    <associate|auto-8|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|auto-9|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|footnote-1|<tuple|1|8>>
    <associate|footnr-1|<tuple|1|8>>
    <associate|zbibSysID101|<tuple|1|9>>
    <associate|zbibSysID1166|<tuple|1|9>>
    <associate|zbibSysID1170|<tuple|1|9>>
    <associate|zbibSysID1252|<tuple|1|9>>
    <associate|zbibSysID131|<tuple|1|9>>
    <associate|zbibSysID1342|<tuple|1|9>>
    <associate|zbibSysID1356|<tuple|1|9>>
    <associate|zbibSysID1391|<tuple|1|9>>
    <associate|zbibSysID1420|<tuple|1|9>>
    <associate|zbibSysID1451|<tuple|1|11>>
    <associate|zbibSysID1523|<tuple|1|9>>
    <associate|zbibSysID1620|<tuple|1|9>>
    <associate|zbibSysID211|<tuple|1|9>>
    <associate|zbibSysID212|<tuple|1|9>>
    <associate|zbibSysID2239|<tuple|1|9>>
    <associate|zbibSysID2275|<tuple|1|9>>
    <associate|zbibSysID230|<tuple|1|9>>
    <associate|zbibSysID2309|<tuple|1|9>>
    <associate|zbibSysID3187|<tuple|1|9>>
    <associate|zbibSysID3216|<tuple|1|9>>
    <associate|zbibSysID325|<tuple|1|9>>
    <associate|zbibSysID3285|<tuple|1|11>>
    <associate|zbibSysID3333|<tuple|1|9>>
    <associate|zbibSysID3421|<tuple|1|10>>
    <associate|zbibSysID3427|<tuple|1|10>>
    <associate|zbibSysID3429|<tuple|1|10>>
    <associate|zbibSysID3434|<tuple|1|9>>
    <associate|zbibSysID3440|<tuple|1|10>>
    <associate|zbibSysID3457|<tuple|1|10>>
    <associate|zbibSysID3461|<tuple|1|10>>
    <associate|zbibSysID3468|<tuple|1|9>>
    <associate|zbibSysID3469|<tuple|1|10>>
    <associate|zbibSysID3471|<tuple|1|10>>
    <associate|zbibSysID3474|<tuple|1|10>>
    <associate|zbibSysID3482|<tuple|1|11>>
    <associate|zbibSysID3484|<tuple|1|10>>
    <associate|zbibSysID3486|<tuple|1|11>>
    <associate|zbibSysID3603|<tuple|1|11>>
    <associate|zbibSysID3613|<tuple|1|10>>
    <associate|zbibSysID3625|<tuple|1|10>>
    <associate|zbibSysID3656|<tuple|1|10>>
    <associate|zbibSysID3733|<tuple|1|10>>
    <associate|zbibSysID3735|<tuple|1|10>>
    <associate|zbibSysID374|<tuple|1|10>>
    <associate|zbibSysID3748|<tuple|1|11>>
    <associate|zbibSysID3756|<tuple|1|10>>
    <associate|zbibSysID3955|<tuple|1|10>>
    <associate|zbibSysID3959|<tuple|1|10>>
    <associate|zbibSysID3966|<tuple|1|10>>
    <associate|zbibSysID3990|<tuple|1|10>>
    <associate|zbibSysID4002|<tuple|1|10>>
    <associate|zbibSysID4006|<tuple|1|10>>
    <associate|zbibSysID4008|<tuple|1|10>>
    <associate|zbibSysID4009|<tuple|1|10>>
    <associate|zbibSysID4020|<tuple|1|11>>
    <associate|zbibSysID4022|<tuple|1|11>>
    <associate|zbibSysID920|<tuple|1|9>>
    <associate|zbibSysID93|<tuple|1|9>>
    <associate|zciteID+WoBHoEj0Jm92ki#zbibSysID3187|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|zciteID+WoBHoEj0Jm92ki#zbibSysID3564|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|zciteID+ZKhKfHGkvinH8g#zbibSysID1342|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|zciteID+ZKhKfHGkvinH8g#zbibSysID1420|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|zciteID+ZKhKfHGkvinH8g#zbibSysID2275|<tuple|<with|mode|<quote|math>|<rigid|\<rightarrow\>>>|4>>
    <associate|zotero+DvR2qZelkLrxq7-noteIndex|<tuple|0|9>>
    <associate|zotero+WoBHoEj0Jm92ki-noteIndex|<tuple|0|4>>
    <associate|zotero+ZKhKfHGkvinH8g-noteIndex|<tuple|0|4>>
  </collection>
</references>

<\auxiliary>
  <\collection>
    <\associate|idx>
      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|brown>|tm-zotero>>|<pageref|auto-2>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|brown>|legal-brief>>|<pageref|auto-3>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Zotero>>|<pageref|auto-4>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|brown>|tm-zotero>>|<pageref|auto-5>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document \<gtr\> Part
      \<gtr\> Show one part>>|<pageref|auto-6>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Document \<gtr\> Part
      \<gtr\> Show several parts>>|<pageref|auto-7>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Zotero>>|<pageref|auto-8>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Zotero>|<with|font-family|<quote|ss>|Set
      Document Prefs>>|<pageref|auto-9>>

      <tuple|<tuple|<with|font-family|<quote|ss>|editCitation>>|<pageref|auto-10>>

      <tuple|<tuple|<with|font-family|<quote|tt>|color|<quote|brown>|tm-zotero.ts>>|<pageref|auto-11>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Refresh>>|<pageref|auto-12>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Abbrevs>>|<pageref|auto-13>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Zotero>>|<pageref|auto-14>>

      <tuple|<tuple|<with|font-family|<quote|ss>|Zotero>|<with|font-family|<quote|ss>|Edit
      Bibliography>>|<pageref|auto-15>>
    </associate>
    <\associate|toc>
      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Initial
      Setup> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-1><vspace|0.5fn>

      <vspace*|1fn><with|font-series|<quote|bold>|math-font-series|<quote|bold>|Sample
      Bibliography> <datoms|<macro|x|<repeat|<arg|x>|<with|font-series|medium|<with|font-size|1|<space|0.2fn>.<space|0.2fn>>>>>|<htab|5mm>>
      <no-break><pageref|auto-16><vspace|0.5fn>
    </associate>
  </collection>
</auxiliary>