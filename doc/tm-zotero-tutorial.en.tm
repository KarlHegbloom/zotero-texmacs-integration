<TeXmacs|1.99.9>

<style|<tuple|tmdoc|english|tm-zotero>>

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

    <item>Be aware that sometimes operations between <TeXmacs> and
    Juris-M/Zotero can take a while. Be patient, and watch the CPU meter.
    It's a lot faster to work with when you do not put a
    <markup|zbibliography> into the document until very close to the end of
    production, since when there's a large bibliography to format, it takes a
    while each time you enter a new <markup|zcite> or edit one that's already
    in the document.

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
    <zcite|+WoBHoEj0Jm92ki|<#4954454D2043534C5F4349544154494F4E207B226369746174696F6E4944223A22743142506677595A222C2270726F70657274696573223A7B22666F726D61747465644369746174696F6E223A227B5C5C727466207B5C5C697473686170657B7D5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944333138377D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D31333636393236313236373333353834333036307D7D7B4164616D7D7320762E2053746174657D2C2032303035205554205C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944333138377D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D31333636393236313236373333353834333036307D7D7B36327D2C20313233205C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944333536347D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D31333636393236313236373333353834333036307D7D7B502E33647D203430302028303223405574616855746168582D582D5820303123405375702E2043742E5375702E2043742E582D582D582032303035292E7D222C22706C61696E4369746174696F6E223A222877697468205C22666F6E742D73686170655C22205C226974616C69635C222028636F6E63617420287A744872656646726F6D43697465546F426962205C22237A6269625379734944333138375C22205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D31333636393236313236373333353834333036305C22205C224164616D5C2229205C2273205C22202861626272205C22762E5C2229205C222053746174652C2032303035205554205C2220287A744872656646726F6D43697465546F426962205C22237A6269625379734944333138375C222028736C696E6B205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D31333636393236313236373333353834333036305C2229205C2236325C2229205C222C20313233205C2220287A744872656646726F6D43697465546F426962205C22237A6269625379734944333536345C222028736C696E6B205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D31333636393236313236373333353834333036305C2229205C22502E33645C2229205C2220343030202832303035292E5C222929222C2273757070726573732D747261696C696E672D70756E6374756174696F6E223A66616C73657D2C226369746174696F6E4974656D73223A5B7B226964223A333138372C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33584535375A4A39225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224164616D7320762E205374617465222C22636F6E7461696E65722D7469746C65223A225554222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A223632222C22766F6C756D65223A2232303035222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D3133363639323631323637333335383433303630222C226E6F7465223A226D6C7A73796E63313A303035347B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A75745C227D7D222C22697373756564223A7B22726177223A2253657074656D6265722032332C2032303035227D7D2C226C6162656C223A2270616765227D2C7B226964223A333536342C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4551464654454D58225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224164616D7320762E205374617465222C22636F6E7461696E65722D7469746C65223A22502E203364222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A22343030222C22766F6C756D65223A22313233222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D3133363639323631323637333335383433303630222C226E6F7465223A226D6C7A73796E63313A303035347B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A75745C227D7D7B3A6A7572697364696374696F6E3A20557461687D222C22697373756564223A7B22726177223A2253657074656D6265722032332C2032303035227D7D2C226C6162656C223A2270616765227D5D2C22736368656D61223A2268747470733A2F2F6769746875622E636F6D2F6369746174696F6E2D7374796C652D6C616E67756167652F736368656D612F7261772F6D61737465722F63736C2D6369746174696F6E2E6A736F6E227D>|<with|font-shape|italic|<ztHrefFromCiteToBib|#zbibSysID3187|https://scholar.google.com/scholar_case?case=13669261267335843060|Adam>s
    <abbr|v.> State, 2005 UT <ztHrefFromCiteToBib|#zbibSysID3187|<slink|https://scholar.google.com/scholar_case?case=13669261267335843060>|62>,
    123 <ztHrefFromCiteToBib|#zbibSysID3564|<slink|https://scholar.google.com/scholar_case?case=13669261267335843060>|P.3d>
    400 (2005).>> Notice that this is a <em|parallel citation>. There are two
    entries in the Juris-M reference database for the same case. I cite one
    then the other immediately after it in the same reference cluster, and it
    outputs as a parallel citation. Citing more than one reference in a
    citation cluster is always possible. It's only when consecutive items
    within the citation cluster are of the same case that they will be
    collapsed into a parallel citation.

    <item><zcite|+ZKhKfHGkvinH8g|<#4954454D2043534C5F4349544154494F4E207B226369746174696F6E4944223A22327870385561456B222C2270726F70657274696573223A7B22666F726D61747465644369746174696F6E223A227B5C5C727466207B5C5C697473686170657B7D5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944323237357D7B5C5C706174687B687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F636173653D393535303433333132363236393637343531397D7D7B427261647D7920762E204D6172796C616E647D2C2033373320555320383320283031234055535553582D582D5820303123405375702E2043742E5375702E2043742E582D582D582031393633293B207B5C5C697473686170657B7D5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944313432307D7B5C5C706174687B68747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F713D556E697465642B5374617465732B762B476F6C75625C5C26686C3D656E5C5C2661735C5C5F7364743D362C34355C5C26636173653D383133313237353636393038393834373439315C5C267363696C683D307D7D7B556E69747D65642053746174657320762E20476F6C75627D2C2036393420462E326420323037202831307468204369722E203034234043742E206F66204170702E2031393832293B207B5C5C697473686170657B7D5C5C7A744872656646726F6D43697465546F4269627B237A6269625379734944313334327D7B5C5C706174687B687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725C5C5F636173653F713D4D6F6F6E65792B762B486F6C6F68616E5C5C26686C3D656E5C5C2661735C5C5F7364743D362C34355C5C26636173653D31303535333236373632313939343434323331305C5C267363696C683D307D7D7B4D6F6F6E7D657920762E20486F6C6F68616E7D2C203239342055532031303320283031234055535553582D582D5820303123405375702E2043742E5375702E2043742E582D582D582031393335292E7D222C22706C61696E4369746174696F6E223A222877697468205C22666F6E742D73686170655C22205C226974616C69635C222028636F6E63617420287A744872656646726F6D43697465546F426962205C22237A6269625379734944323237355C22205C22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D393535303433333132363236393637343531395C22205C22427261645C2229205C2279205C22202861626272205C22762E5C2229205C22204D6172796C616E642C20333733205553203833202831393633293B205C2220287A744872656646726F6D43697465546F426962205C22237A6269625379734944313432305C222028736C696E6B205C2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D556E697465642B5374617465732B762B476F6C756226686C3D656E2661735F7364743D362C343526636173653D38313331323735363639303839383437343931267363696C683D305C2229205C22556E69745C2229205C22656420537461746573205C22202861626272205C22762E5C2229205C2220476F6C75622C2036393420462E326420323037202831307468204369722E205C22202861626272205C2243742E5C2229205C22206F66205C22202861626272205C224170702E5C2229205C222031393832293B205C2220287A744872656646726F6D43697465546F426962205C22237A6269625379734944313334325C222028736C696E6B205C22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D4D6F6F6E65792B762B486F6C6F68616E26686C3D656E2661735F7364743D362C343526636173653D3130353533323637363231393934343432333130267363696C683D305C2229205C224D6F6F6E5C2229205C226579205C22202861626272205C22762E5C2229205C2220486F6C6F68616E2C2032393420555320313033202831393335292E5C222929227D2C226369746174696F6E4974656D73223A5B7B226964223A323237352C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5556543651445455225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5556543651445455225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A22427261647920762E204D6172796C616E64222C22636F6E7461696E65722D7469746C65223A225553222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A223833222C22766F6C756D65223A22333733222C2255524C223A22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F636173653D39353530343333313236323639363734353139222C226E6F7465223A226D6C7A73796E63313A303035317B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275735C227D7D3030303030222C22697373756564223A7B22726177223A2231393633227D2C226163636573736564223A7B22726177223A22323031342D31322D31325430363A34323A30385A227D7D7D2C7B226964223A313432302C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5544333237343552225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5544333237343552225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A22556E697465642053746174657320762E20476F6C7562222C22636F6E7461696E65722D7469746C65223A22462E203264222C22617574686F72697479223A22636F7572742E61707065616C73222C2270616765223A22323037222C22766F6C756D65223A22363934222C2255524C223A2268747470733A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D556E697465642B5374617465732B762B476F6C756226686C3D656E2661735F7364743D362C343526636173653D38313331323735363639303839383437343931267363696C683D30222C226E6F7465223A226D6C7A73796E63313A303035357B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275733A6331305C227D7D3030303030222C22697373756564223A7B22726177223A22446563656D62657220312C2031393832227D2C226163636573736564223A7B22726177223A22323031352D30342D30315432303A35303A34365A227D7D7D2C7B226964223A313334322C2275726973223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3239524A47495143225D2C22757269223A5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3239524A47495143225D2C226974656D44617461223A7B2274797065223A226C6567616C5F63617365222C227469746C65223A224D6F6F6E657920762E20486F6C6F68616E222C22636F6E7461696E65722D7469746C65223A225553222C22617574686F72697479223A2253757072656D6520436F757274222C2270616765223A22313033222C22766F6C756D65223A22323934222C2255524C223A22687474703A2F2F7363686F6C61722E676F6F676C652E636F6D2F7363686F6C61725F636173653F713D4D6F6F6E65792B762B486F6C6F68616E26686C3D656E2661735F7364743D362C343526636173653D3130353533323637363231393934343432333130267363696C683D30222C226E6F7465223A226D6C7A73796E63313A303035317B5C22747970655C223A5C22636173655C222C5C2265787472616669656C64735C223A7B5C226A7572697364696374696F6E5C223A5C2275735C227D7D3030303030222C22697373756564223A7B22726177223A224A616E756172792032312C2031393335227D2C226163636573736564223A7B22726177223A22323031352D30332D30345432333A31393A33335A227D7D7D5D2C22736368656D61223A2268747470733A2F2F6769746875622E636F6D2F6369746174696F6E2D7374796C652D6C616E67756167652F736368656D612F7261772F6D61737465722F63736C2D6369746174696F6E2E6A736F6E227D>|<with|font-shape|italic|<ztHrefFromCiteToBib|#zbibSysID2275|http://scholar.google.com/scholar_case?case=9550433126269674519|Brad>y
    <abbr|v.> Maryland, 373 US 83 (1963);
    <ztHrefFromCiteToBib|#zbibSysID1420|<slink|https://scholar.google.com/scholar_case?q=United+States+v+Golub&hl=en&as_sdt=6,45&case=8131275669089847491&scilh=0>|Unit>ed
    States <abbr|v.> Golub, 694 F.2d 207 (10th Cir. <abbr|Ct.> of <abbr|App.>
    1982); <ztHrefFromCiteToBib|#zbibSysID1342|<slink|http://scholar.google.com/scholar_case?q=Mooney+v+Holohan&hl=en&as_sdt=6,45&case=10553267621994442310&scilh=0>|Moon>ey
    <abbr|v.> Holohan, 294 US 103 (1935).>> Notice that in this citation
    cluster, the first four characters of each citation are a hyperlink to
    the bibliography entry that corresponds with that citation. That link
    appears only when the document has a <markup|zbibliography>, since
    otherwise, the link would not have a valid target.

    <item>Inside the <markup|zbibliography> (at the end of this document)
    you'll find that some of the entries have the first four characters as a
    hyperlink to the on-line URL associated with that Juris-M/Zotero
    reference database entry.

    <item>You may insert a bibliography using either the menu, or the
    <key|\\zbibliography> or <key|\\zb> keyboard shortcuts.

    <item>When the cursor is just to the right of a citation or just inside
    of the \Pzbibliography\Q, you can press the <key|Tab> key to open the
    Zotero citation or bibliography edit dialogues.

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

      <item><tt|000000000@#\\ztbibSubHeading{!Title of Subheading
      Here}><space|1tab>is for creating a category subheading in your
      bibliography. Notice that there are exactly 9 zeros, followed by
      <verbatim|@#>. It is used in the <tt|title> field in the Juris-M /
      Zotero database. The item type should be set to an appropriate type to
      match one of the item types for the particular category. For legal
      cases, you must also set the jurisdiction and court in order to make
      the subheading appear in the right part of the bibliography. You
      <em|may> prefix that with a sorting-prefix,
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

  <\zbibliography|+DvR2qZelkLrxq7|<#4249424C207B22756E6369746564223A5B5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F455A514637494350225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F43324345334A3736225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E32363251564339225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4733525535423755225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3242373434343643225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5A545A355A465648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F384A455038355335225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F47374E55434B5655225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4851353645463637225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4252433852454753225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F41474246384B5A47225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F39414E4235464135225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4A5256514E48544B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F47584D5643445451225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D3639534D55334B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4542464A33545834225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5747535635584A53225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4D52465653325347225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4741585038325A38225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4947475042494A42225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B57584D53374341225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3333334A34543357225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3638414B505A4B44225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3541555045504D53225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4B48444849524E45225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5037453952525237225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F374341354B374332225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56553452494D4238225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E385339554D4453225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F543542485633354B225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F52483444484E3436225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3849425341534B46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3753453537465033225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4447384945325046225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F584B465547474E52225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F333856454A335850225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3535554A42334E58225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5443534D344E3648225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F46373333394B4544225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3752434B52384947225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3532325356345353225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5258374444554B51225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3350343842354D43225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5056415452443748225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4546374E5036484A225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F414856354E455057225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F524133374B455657225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F363452564B324D46225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5856584638414B39225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4733565042575836225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F44535A345A374B56225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5751363645443534225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3757495639374949225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3536524146385356225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3242395852325243225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F36344A3544483738225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F573541343555514D225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5244363944383254225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F363252464B4B3738225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4E545A33564D5248225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F33364A5446534951225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F56435A3357484851225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5452493857364A39225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F5745585034353739225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F3542555A49553332225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F4541555357354253225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F494B423435503933225D2C5B22687474703A2F2F7A6F7465726F2E6F72672F75736572732F3232363037342F6974656D732F43524A4A41455452225D5D2C22637573746F6D223A5B5D7D2043534C5F4249424C494F475241504859>>
    <ztbibSubHeading|United States Supreme Court Cases>\ 

    <ztbibItemText|1252||sysID1252|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1252|http://scholar.google.com/scholar_case?q=Blakely+v+Washington&hl=en&as_sdt=6,45&case=16163203473167624369&scilh=0|Blak>ely
    <abbr|v.> Washington>, 542 US 296 (2004).>

    <ztbibItemText|2275||sysID2275|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID2275|<slink|http://scholar.google.com/scholar_case?case=9550433126269674519>|Brad>y
    <abbr|v.> Maryland>, 373 US 83 (1963).>

    <ztbibItemText|211||sysID211|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID211|<slink|https://scholar.google.com/scholar_case?q=489+us+189&hl=en&as_sdt=6,45&case=5543768239799414902&scilh=0>|DeSh>aney
    <abbr|v.> Winnebago County Dept. of Social Servs.>, 489 US 189 (1989).>

    <ztbibItemText|2239||sysID2239|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID2239|<slink|http://scholar.google.com/scholar_case?case=2659052629576231238&q=407+S.+514,+532+(1972)&hl=en&as_sdt=6,45&scilh=0>|Dogg>ett
    <abbr|v.> United States>, 505 US 647 (1992).>

    <ztbibItemText|1620||sysID1620|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1620|<slink|https://scholar.google.com/scholar_case?case=3973384553826466817>|Doug>las
    <abbr|v.> California>, 372 US 353 (1963).>

    <ztbibItemText|3216||sysID3216|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3216|<slink|https://scholar.google.com/scholar_case?q=ex+parte+Young&hl=en&as_sdt=6,45&case=15822732193533819720&scilh=0>|ex
    p>arte Young>, 209 US 123 (1908).>

    <ztbibItemText|1170||sysID1170|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1170|<slink|http://scholar.google.com/scholar_case?q=528+us+167&hl=en&as_sdt=3,45&case=5440560917097220943&scilh=0>|Frie>nds
    of Earth, <abbr|Inc.> <abbr|v.> Laidlaw Environmental Services (TOC),
    <abbr|Inc.>>, 528 US 167 (2000).>

    <ztbibItemText|2309||sysID2309|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID2309|<slink|http://scholar.google.com/scholar_case?case=12450678889272734206&q=related:b7CW8KJyhJsJ:scholar.google.com/&hl=en&as_sdt=6,45>|Gigl>io
    <abbr|v.> United States>, 405 US 150 (1972).>

    <ztbibItemText|325||sysID325|<with|font-shape|italic|Joint Anti-Fascist
    Refugee Comm. <abbr|v.> McGrath>, 341 US 123 (1951).>

    <ztbibItemText|1342||sysID1342|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1342|<slink|http://scholar.google.com/scholar_case?q=Mooney+v+Holohan&hl=en&as_sdt=6,45&case=10553267621994442310&scilh=0>|Moon>ey
    <abbr|v.> Holohan>, 294 US 103 (1935).>

    <ztbibItemText|3333||sysID3333|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3333|<slink|https://scholar.google.com/scholar_case?q=Wall+v+Kholi&hl=en&as_sdt=6,45&case=13423175293489651929&scilh=0>|Wall>
    <abbr|v.> Kholi>, 131 <abbr|S.> <abbr|Ct.> 1278 (2011).>

    <ztbibSubHeading|Utah Supreme Court Cases>\ 

    <ztbibItemText|3187||sysID3187|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3187|<slink|https://scholar.google.com/scholar_case?case=13669261267335843060>|Adam>s
    <abbr|v.> State>, 2005 UT 62, <ztHrefFromBibToURL|#zbibSysID3564|<slink|https://scholar.google.com/scholar_case?case=13669261267335843060>|123>
    P.3d 400 (2005).>

    <ztbibItemText|101||sysID101|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID101|<slink|http://scholar.google.com/scholar_case?case=14333925206835369542&q=state+v++rees&hl=en&as_sdt=4,45>|Stat>e
    <abbr|v.> Rees>, 125 P.3d 874 (2005).>

    <ztbibSubHeading|Utah Court of Appeals Cases>\ 

    <ztbibItemText|1356||sysID1356|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1356|<slink|http://scholar.google.com/scholar_case?case=2513784522765968505>|Stat>e
    <abbr|v.> Hegbloom>, 2014 UT App 213 (2014).>

    <ztbibItemText|131||sysID131|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID131|<slink|http://scholar.google.com/scholar_case?case=17234797921577956966&q=state+v++rees+20010490-CA+coram+nobis&hl=en&as_sdt=4,45>|Stat>e
    <abbr|v.> Rees>, 63 P.3d 120 (2003).>

    <ztbibItemText|1523||sysID1523|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1523|<slink|https://scholar.google.com/scholar_case?case=11195558326880357574>|Stat>e
    <abbr|v.> Sery>, 758 P.2d 935 (1988).>

    <ztbibItemText|920||sysID920|<with|font-shape|italic|Wiscombe <abbr|v.>
    Wiscombe>, 744 P.2d 1024 (1987).>

    <ztbibSubHeading|Federal Tenth Circuit Court Cases>\ 

    <ztbibItemText|93||sysID93|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID93|<slink|https://scholar.google.com/scholar_case?q=Gonzales+307+f.3d&hl=en&as_sdt=6,45&case=1586392809091968224&scilh=0>|Gonz>ales
    <abbr|v.> City of Castle Rock>, 307 F.3d 1258 (10th Cir. <abbr|Ct.> of
    <abbr|App.> 2002).>

    <ztbibItemText|1391||sysID1391|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1391|<slink|http://scholar.google.com/scholar_case?case=3104253368109472968&q=United+States+v.+Cronic&hl=en&as_sdt=6,45&scilh=0>|Unit>ed
    States <abbr|v.> Golub>, 638 F.2d 185 (10th Cir. <abbr|Ct.> of
    <abbr|App.> 1980).>

    <ztbibItemText|1420||sysID1420|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1420|<slink|https://scholar.google.com/scholar_case?q=United+States+v+Golub&hl=en&as_sdt=6,45&case=8131275669089847491&scilh=0>|Unit>ed
    States <abbr|v.> Golub>, 694 F.2d 207 (10th Cir. <abbr|Ct.> of
    <abbr|App.> 1982).>

    <ztbibSubHeading|Other Jurisdictions' Court Cases>\ 

    <ztbibItemText|3468||sysID3468|<with|font-shape|italic|<abbr|Dr.>
    Bonham's Case 1861>, (1861) 77 English Reports 646 (Masters).>

    <ztbibItemText|212||sysID212|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID212|<slink|https://scholar.google.com/scholar_case?case=13190905168500779696>|Comm>issioner
    of Probation <abbr|v.> Adams>, 65 <abbr|<abbr|Mass.>> <abbr|<abbr|App.>>
    <abbr|<abbr|Ct.>> 725 (2006).>

    <ztbibItemText|1166||sysID1166|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID1166|<slink|http://scholar.google.com/scholar_case?case=17563102041873563785&q=v+van+pelt&hl=en&as_sdt=6,45>|Wall>ace
    <abbr|v.> Van Pelt>, 969 S.W.2d 380 (Mo. <abbr|Ct.> of <abbr|App.>
    1998).>

    <ztbibItemText|230||sysID230|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID230|<slink|https://scholar.google.com/scholar_case?about=13788143762591333722>|Wrig>ht
    <abbr|v.> Wright>, 54 NY 437 (N.<abbr|Y.> <abbr|Ct.> of <abbr|App.>
    1873).>

    <ztbibSubHeading|United States Code>\ 

    <ztbibItemText|3434||sysID3434|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3434|<slink|https://www.law.cornell.edu/uscode/text/18/1346>|Defi>nition
    of \Pscheme or artifice to defraud,\Q> 18 <abbr|<abbr|U.>S.C.>>

    <ztbibItemText|3656||sysID3656|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3656|<slink|https://www.law.cornell.edu/uscode/text/42/405>|Evid>ence,
    procedure, and certification for payments>, 42 <abbr|<abbr|U.>S.C.>>

    <ztbibItemText|4020||sysID4020|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID4020|<slink|https://www.utcourts.gov/resources/rules/ucja/#Chapter_13>|Rule>s
    of Professional Conduct>, UCJA>

    <ztbibSubHeading|Constitution of Utah>\ 

    <ztbibItemText|3474||sysID3474|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3474|<slink|http://le.utah.gov/xcode/ArticleI/Article_I,_Section_11.html>|Utah>
    Const, <abbr|Art.> I, <SectionSignGlyph|>11>,
    <with|font-shape|small-caps|Open courts provision>>

    <ztbibItemText|374||sysID374|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID374|<slink|http://le.utah.gov/code/CONST/htm/00I01_001200.htm>|Utah>
    Const, <abbr|Art.> I, <SectionSignGlyph|>12>,
    <with|font-shape|small-caps|Rights of accused persons>>

    <ztbibItemText|3471||sysID3471|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3471|<slink|http://le.utah.gov/xcode/ArticleI/Article_I,_Section_24.html>|Utah>
    Const, <abbr|Art.> I, <SectionSignGlyph|>24>,
    <with|font-shape|small-caps|Uniform operation of law>>

    <ztbibItemText|3469||sysID3469|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3469|<slink|http://le.utah.gov/xcode/ArticleI/Article_I,_Section_27.html>|Utah>
    Const, <abbr|Art.> I, <SectionSignGlyph|>27>,
    <with|font-shape|small-caps|Frequent recurrence to fundamental
    principles>>

    <ztbibItemText|3966||sysID3966|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3966|<slink|http://le.utah.gov/xcode/ArticleVI/Article_VI,_Section_26.html>|Utah>
    Const, <abbr|Art.> VI, <SectionSignGlyph|>26>,
    <with|font-shape|small-caps|No private or special law shall be enacted
    where a general law can be applicable>>

    <ztbibSubHeading|Utah Code>\ 

    <ztbibItemText|3427||sysID3427|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3427|<slink|http://le.utah.gov/xcode/Title68/Chapter3/68-3.html?v=C68-3_1800010118000101>|Utah>
    Code <SectionSignGlyph|>68-3: Construction Statutes> (1953).>

    <ztbibItemText|3429||sysID3429|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3429|<slink|http://le.utah.gov/xcode/Title68/Chapter3/68-3-S2.html?v=C68-3-S2_1800010118000101>|Utah>
    Code <SectionSignGlyph|>68-3-2: Statutes in derogation of common law not
    strictly construed <emdash> Rules of equity prevail> (2010).>

    <ztbibItemText|3990||sysID3990|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3990|<slink|http://le.utah.gov/xcode/Title76/Chapter1/76-1-S104.html?v=C76-1-S104_1800010118000101>|Utah>
    Code <SectionSignGlyph|>76-1-104: Purposes and principles of
    construction> (1973).>

    <ztbibItemText|3457||sysID3457|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3457|<slink|http://le.utah.gov/xcode/Title77/Chapter7/77-7-S3.html?v=C77-7-S3_1800010118000101>|Utah>
    Code <SectionSignGlyph|>77-7-3: Arrest by private persons> (1980).>

    <ztbibItemText|3613||sysID3613|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3613|<slink|http://le.utah.gov/xcode/Title78B/Chapter7/78B-7-S115.html?v=C78B-7-S115_2016051020160510>|Utah>
    Code <SectionSignGlyph|>78B-7-115: Dismissal of Protective Order>, Utah
    Code (2016).>

    <ztbibItemText|4002||sysID4002|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID4002|<slink|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9.html?v=C78B-9_1800010118000101>|Utah>
    Code <SectionSignGlyph|>78B-9: Post-conviction remedies act>, Utah Code
    (2008).>

    <ztbibItemText|4008||sysID4008|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID4008|<slink|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9-S106.html>|Utah>
    Code <SectionSignGlyph|>78B-9-106: Preclusion of relief <emdash>
    Exception>, Utah Code (2008).>

    <ztbibItemText|4009||sysID4009|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID4009|<slink|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9-S107.html>|Utah>
    Code <SectionSignGlyph|>78B-9-107: Statute of limitations for
    postconviction relief>, Utah Code (2008).>

    <ztbibItemText|4006||sysID4006|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID4006|<slink|http://le.utah.gov/xcode/Title78B/Chapter9/78B-9-S401.5.html>|Utah>
    Code <SectionSignGlyph|>78B-9-401.5: Definitions>, Utah Code (2008).>

    <ztbibSubHeading|Utah Rules of Civil Procedure>\ 

    <ztbibItemText|3625||sysID3625|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3625|<slink|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp011.html>|Sign>ing
    of pleadings, motions, and other papers; representations to court;
    sanctions>, URCP 11>

    <ztbibItemText|3733||sysID3733|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3733|<slink|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp052.html>|Find>ings
    by the court; correction of the record>, URCP 52>

    <ztbibItemText|3735||sysID3735|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3735|<slink|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp059.html>|New
    >trial; altering or amending a judgment>, URCP 59>

    <ztbibItemText|3756||sysID3756|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3756|<slink|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp060.html>|Reli>ef
    from judgment or order>, URCP 60>

    <ztbibItemText|3959||sysID3959|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3959|<slink|http://www.utcourts.gov/resources/rules/urcp/view.html?rule=urcp065C.html>|Post>-conviction
    Relief>, URCP 65C>

    <ztbibSubHeading|Utah Rules of Criminal Procedure>\ 

    <ztbibItemText|3461||sysID3461|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3461|<slink|http://www.utcourts.gov/resources/rules/urcrp/view.html?rule=URCRP07.html>|Proc>eedings
    before a magistrate>, URCrP 7 (2014).>

    <ztbibSubHeading|Utah Rules of Appellate Procedure>\ 

    <ztbibItemText|3421||sysID3421|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3421|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=03.htm>|Appe>al
    as of right: how taken>, URAP 3 (2015).>

    <ztbibItemText|3440||sysID3440|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3440|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=04.htm>|Appe>al
    as of right: when taken>, URAP 4 (2015).>

    <ztbibItemText|3484||sysID3484|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3484|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=05.htm>|Disc>retionary
    appeals of interlocutory orders>, URAP 5 (2014).>

    <ztbibItemText|3955||sysID3955|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3955|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=10.htm>|Moti>on
    for Summary Disposition>, URAP 10 (2012).>

    <ztbibItemText|3482||sysID3482|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3482|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=11.htm>|The
    >record on appeal>, URAP 11 (2015).>

    <ztbibItemText|3486||sysID3486|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3486|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=19.htm>|Extr>aordinary
    writs>, URAP 19 (2015).>

    <ztbibItemText|3603||sysID3603|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3603|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=20.htm>|Habe>as
    corpus proceedings>, URAP 20 (2012).>

    <ztbibItemText|3748||sysID3748|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID3748|<slink|http://www.utcourts.gov/resources/rules/urap/view.html?rule=24.htm>|Brie>fs>,
    URAP 24 (2015).>

    <ztbibSubHeading|<ztHrefFromBibToURL|#zbibSysID4019|<slink|https://www.utcourts.gov/resources/rules/ucja/#Chapter_13>|Utah>
    Supreme Court Rules of Professional Conduct>\ 

    <ztbibItemText|4022||sysID4022|<with|font-shape|italic|<ztHrefFromBibToURL|#zbibSysID4022|<slink|http://www.utcourts.gov/resources/rules/ucja/view.html?rule=ch13/1_0.htm>|Term>inology>,
    UCJA ch13 Rule 1.0>

    <ztbibSubHeading|Utah Legislature Bills>\ 

    <ztbibItemText|3285||sysID3285|<ztHrefFromBibToURL|#zbibSysID3285|<slink|http://le.utah.gov/~2013/bills/static/SB0263.html>|SB02>63
    SB0263, Utah State Senate (2013).>

    <ztbibSubHeading|Books, Journal Articles, and Other Documents>\ 

    <ztbibItemText|1451||sysID1451|<ztHrefFromBibToURL|#zbibSysID1451|<slink|http://www.saveservices.org/wp-content/uploads/SAVE-Assault-Civil-Rights.pdf>|SAVE>,
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
    <associate|zotero-data-zotero-version|4.0.29.12m98alpha>
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
    <associate|zotero-session-id|rE8qJpwR>
    <associate|zotero-style-bibliographyStyleHasBeenSet|1>
    <associate|zotero-style-hasBibliography|1>
    <associate|zotero-style-id|http://juris-m.github.io/styles/jm-indigobook-catsort-bib>
    <associate|zotero-style-locale|en-US>
    <associate|zoteroDocumentData|\<data data-version="3"
    zotero-version="4.0.29.12m98alpha"\>\<session id="rE8qJpwR"/\>\<style
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
  </collection>
</initial>