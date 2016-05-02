Notes
=====

This is where I'll take notes as I explore the TeXmacs,
Juris-M/Zotero, and zotero-openoffice-integration source code in order
to understand how to implement zotero integration for TeXmacs.

Parts of TeXmacs documents
--------------------------

The top of the document is a `TeXmacs` tag. It has no end tag, so it's
not like an environment that wraps the whole document. It declares the
version of TeXmacs that was used to save the file. When the editor is
upgraded and you open an old file, it upgrades the document.

Next is the `style` tag, which contains a `tuple` with the flat list
of style and style packages.

After that is the `body` of the document. It starts with one that
looks like `<\body>` and ends with one that looks like
`</body>`. (Notice the different direction slash in start and end
tags.)

Then there's the `initial` containing a `collection` of environment
definitions, as `associate` tags. I think it's effectively an alist at
the scheme level.

That is followed by `attachments`, containing a `collection` of
`associate`. Here I notice that the `associate` value for
`bib-bibliography` is a tree of `db-entry` items, taken from parsing
the bibtex file.

After the `attachments` is `references`, also a `collection` of
`associate`, where there's a label associated with a `tuple`. When the
bibliography style is a numbered style, I see the tuple associated
with a bib-BIBTEX-KEY containing only an integer, the bibliographic
citation number. When it's an alpha style, I see the alpha citation
label. When it's the authoryear or natbib style, I see a string that
has the author and then the year in parentheses.

After `references` is `auxiliary`, containing a `collection`, where I
find `associate` for `bib`, which appears to be a list of bibtex
citation keys. There is also a `associate` for `toc`, which probably
contains one line for each item in the table of contents.

TeXmacs Scheme Functions that will be very useful
-------------------------------------------------

I think I'm going to need to be able to walk the document and find
every citation tag. I will need the visible citations in document
order, as well as their location `position` for quick jumping from one
to the next. I think that the definition of the `cite` tag can both
insert and maintain. So... What happens when I enter a `cite` and then
delete it? Does it get removed at the moment I delete it, or only
later when some process is run over the document? Do I need to create
that process myself or is there already a mechanism for that?

#### Experiment Results ####

With `supports\_db()` set to `true`, I created a document, article
style, with a tm-plain bibliography, using a `.bib` file with quite a
few entries in it... when I inserted the `cite` tags, I clicked on the
magnifying glass icon, and a dialog widget popped up, but inside of it
are only about a dozen items to choose from. I think it's not parsing
the `@misc` entries, or there's some other kind of syntax problem with
the BibTeX database exported by Juris-M, using the export translator
provided by Better BibTeX for Zotero. In fact, the error console shows
that there's an invalid syntax for the `jabref-meta` of a bunch of
them...

In this document-order, I entered a citation for the last item shown,
then one for the first item, then one for the third item. I then ran
the update bibliography from the document menu. I then entered another
cite, for the fourth item in the bibtex database, but did not run the
update bibliography routine. The numbered labels, in document order,
are 3,1,2,?. Looking inside of the texmacs source file with emacs, I
find the expected cite tags, which contain only the bibtex key for
each citation and no other information. Below is the `bibliography`
environment, which is a `bib-list` of `bibitem\*`, and each
`bibitem\*` is followed immediately by a `label` tag, where the label
is the string `bib-` prepended to the bibtex key. That is what gives
the `cite` locations their displayed label: It's the label getting
it's display string from the tag nearest to the left of it that sets
the label string that it grabs.

This is why with this style, inserting a citation for a certain source
more than once gives the same label number as for the previous
one. Running the bibliography update also made all of those cite tags
render the reference label inside of them, by some automatic mechanism
that I've not explored yet.

Down in the `attachments` section is the `bib-bibliography` assoc with
it's value being a tree of `db-entry`, put there by `bib-attach`,
which is overloadable since it was defined using `tm-define`. Perhaps
for Juris-M/Zotero/CSL, it can store the json or something?

Next, in the `references` section, I see a `collection` of
`associate`, one for each `label` tag in the `bibliography` `bib-list`
`bibitem\*`. The value field for each one contains a `tuple`, where
the first item is the number for that label's display. I recall a
previous experiment, not documented this way, where I used a standard
BibTeX style that uses alpha-date strings formed by the authors name
and the publication year. In these entries I found those strings, that
are also then displayed where the cite tags are rendered. They also
come from the labels in the `bibitem\*`. The second item in these
tuples is a ?, and the third item is a relative file path to the
no-name file that this document was before I saved it with it's new
name. I do not know what that is for yet.

There are references entries for each of the three items in the
bibliography, the first three cites entered, but not for the fourth
one, which was entered, and it's tag activated, immediately prior to
saving the document to look at it.

In the `auxiliary` section is a `collection` with `associate` `bib`
containing a list of bibtex citation keys. The first three cite keys
are there, in document order, but not the new fourth one. Document
order also happens to be the order that I entered the citations into
the document, so a further experiment is going to be necessary to find
out whether they are being maintained in document order, or only in
the chronological order that they are written into the document.

Since entering the cite tag and activating does not appear to be what
causes it to write an entry to the references or the auxiliary bib
list, I will now run the update bibliography item from the menu, then
save the document again, and have a look at what changed. The new
citation was entered chronologically last and physically last in the
document, so first I will kill it and yank it into the top of the
document, save that and look, and then run the update bibliography and
look again.

Viewing the source, I now see the new cite at the top as
expected. Also as expected, it does not appear in either the
references, nor the auxiliary bib list.

After running update bibliography, I see, in TeXmacs, that the newest
cite has number 3, and so they are now numbered, in document order,
3,4,1,2. Viewing the source, I see that the `bibliography` environment
has, of course, been updated, with the new item. Those are not
automatically numbered by the `bib-list`, which is a description style
list. The numeric labels were put there by the tm-plain style, which
sorts them by author, title, then year, IIRC.

In the `references` section, I see that the entire collection was
replaced with a rebuilt one, and that they appear in the same sorted
order that they appear in the bibliography in. The tuple values look
the same.

In the `auxiliary` `bib` list, I see that the new key is at the top of
the list! This is good, because that means they are being maintained
in document order. Now I need to know what piece of code did
that... See below for the analysis of how the update bibliography
happens...

In this experiment, the `supports\_db` is enabled, and I'm using a
`tm-*` style. That means that the code the did this is called from
inside of the `bib-compile` function. It calls on `bib-compile-sub`,
which had all of the `tm-*` styles hard-coded. (I just fixed that.) It
in turn calls on `bib-generate` when the style is a `tm-*` native
scheme style, and that does it's magic via `module-provide` to load
the right scheme code, and then calls the `bib-process` from
`bibtex/bib-utils.scm`, which is defined with `tm-define` and thus,
ta-da! is overloadable for use by our own style if need be.

I think that the document-order insertion of the bibtex keys in the
auxiliary is caused by the typesetter walking the document from start
to finish.

#### Questions: ####

 * Can document positions be stored persistently in the auxiliary
   data? Or do I need to walk the document and then keep them in a
   data structure only while the editor is running?
   
The reason I want to know that is that I know from reading the
citeproc documentation that it works by passing it the list of
citations in document order that precede the citationCluster being
operated on, the citationCluster here, and the list of citations that
follow it, in document order. It then computes the citations that
differ from it's internal state... and returns the id's for the fields
that need to be updated, including the one for the present
citationCluster. So that implies that it needs a way to index them for
update.

When the bibliography has the labels that set the rendered cite tag
`reference`, there can be a one to many relation between those
bibliography `bib-list` `bibitem\*` `label` and `reference` display
sites throughout the document. That's great for numbered citation
styles, but for *e.g.,* Indigobook or Bluebook, or for ones where it's
expected to make a footnote... the first item will have a different
display string than subsequent items will need to have, and when the
same source is cited more than once with no intervening citations, it
renderes an *ibid*.

Just to be sure then, each `cite` is going to need to have it's own
references label... and since each `cite` is a citationCluster, which
might change on editing, so it's displayed label will change... but
really it's invisible references label doesn't need to... so they can
be gensym'd with a counter. It should not maintain a separate one for
each bibtex key in a citation cluster since the selected CSL style is
what can determine the appearance of the entire cluster, and so
maintaining them separately implies duplicating what the CSL citeproc
does, and that is against the purpose of it, which is to outsource the
formatting of the entire citationCluster.

Another question now, requiring another experiment: What happens with
tm-plain when I enter more than one bibtex key inside a single
citation, making it a cluster of them? Predictably, the new cite shows
up, and the tag looks just like the others, only there's a pipe
character separating each bibtex key within it. Running the update
bibliography, I see in TeXmacs that inside that citation, there's
three numbers separated by commas, surrounded by square brackets. The
bibliography contains the new entries. Now in document order, the
numbers are 4,6,3,5,2,1,3. The `references` collection is, as
expected, in bibliography sorted order, and the `auxiliary` `bib` list
is in document order, one key per line, as a straight list, with no
tuple for the multi-item citation.

That means that for the tm-csl or tm-zotero style, those items will
need to be keys generated for each citationCluster, not for each
bibtex key. It will also be necessary to ensure that they are
maintained in document order, more or less just like the code that
does this now does it. That makes this a lot easier...

After looking at the code for `bib-compile` and thinking about this,
it seems to me that it must be the typesetter, where it calls on the
macro expansions, that makes this all happen, and so of course they
get added in document order, by that mechanism.

I think there is an "on activate" action that can happen with
tags... there's also an autocompletion mechanism... It is what I want,
also an icon. It's being used already by the bib db codes, and uses
`tm-define` so it can be overloaded when the mode is set right, to
call on the one that uses the cite-as-you-write or something...


### `kernel/texmacs/tm-convert.scm` ###

`tmfile-extract`: Sample uses can easily be found via grep-find for
"tmfile-extract". Ones I'm likely to need may include: `(aux
(tmfile-extract mas 'auxiliary))`, `(att (tmfile-extract doc
'attachments))`, `(ref (tmfile-extract mas 'references))`

### `kernel/library/content.scm` ###

`collection-ref` etc. are here.

### `database/bib-manage.scm` ###

Note: need to see what else used `tm-define` and thus can be
overloaded. What about the database lookup etc? Might be useful for
Juris-M/Zotero support, right? If it's not already, consider changing
it.

`bib-compile` is defined with `tm-define` and so it should be
overloadable so that it can be overridden for only a particular
tm-bibliography style or mode.[†] It is called from
`src/Edit/Process/edit\_process.cpp` by
`edit\_process\_rep::generate\_bibliography`.

  * When `supports\_db()` is true:

    * `bib-compile` is called when the `bib\_file` is not found, or
    
    * when the `bib\_file` is found.

  * When the `bib\_file` is not found and `supports\_db()` is `false`,
    it is an error.

  * When the `bib\_file` is found and `supports\_db()` is `false`,
    then:
    
    * if the BibTeX command is not present and the `style` is one of
      the standard bibliography styles that has a `tm-STYLE`
      counterpart defined, the `style` is set to that `tm-STYLE`
      counterpart. Todo: see Todo in code.
      
    * When the name of the style begins with `tm-` a built-in style is
      run by evaluating `(use-modules (bibtex STYLE))` followed by
      `(bib-process bib STYLE ot)` where `bib` is the TeXmacs
      bib-prefix, STYLE is the style without the `tm-` prefix, and
      `ot` is `stree` of the `bib\_entries` loaded via `parse\_bib`,
      which is written in C++.
      
    * if the BibTeX `style` does not start with `tm-`, then
      `bibtex\_run` is called. It returns the `tree` `t`.
      
`bib-attach` will be called when `supports\_db()` is true, after
`bib-compile` is finished. At the end, it checks for `t` being atomic,
a single string, with the word Error: at the beginning, and if found,
prints that error in the status-bar. That means that BibTeX (or the
drop-in replacement for it) created a `.bbl` file with only that error
string in it, or that `bib-compile` returned that error string. As
long as `t` is compound, it is inserted, so that means that at the top
of `edit\_process\_rep::generate\_aux\_recursively()` the `go\_to` is
setting the edit insert cursor to the right location for insertion of
the bibliography tree, in a new `document`, which has just replaced
the subtree where the bibliography was prior to execution of this.

[†] See: `kernel/texmacs/tm-define.scm` (overloadable functions) and
`kernel/texmacs/tm-modes.scm` (mode properties for overloaded
functions to test and only run the right one for the mode).


TeXmacs Style Macros to study and explore
-----------------------------------------

I need to do experiments with entering citations, sequentially from
top of the document, to see what order it adds the reference tags, and
then move up and insert one above them, to see if it maintains the
references in document order or not. There has to be a way to get them
in document order, so that the citeproc can do the right thing for
citation styles that have an ibid form. Another consideration needing
to be made is: what happens when there's an invisible citation in the
document?

`std-automatic.ts` for `cite-arg`

`cite-author-year.ts` for `natbib` style citations: Note that this one
might be broken; I see that it adds the `auxiliary` `collection`
`associate` `bib` bibtex citation keys twice for each one, rather than
once for each one. I think that's probably a bug, though it might not
matter for the way things work now, but if the same bug happened to be
present when I'm working with citeproc, it could easily cause
trouble. Or... maybe it does it on purpose? Does it need to? It has to
access the data and parse it to produce the string that's displayed
upon rendering. For the tm-cls or tm-zotero, the citeproc will do all
the work.
