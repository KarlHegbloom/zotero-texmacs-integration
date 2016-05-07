;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : tm-bb-cayw.scm
;; DESCRIPTION : Support for CSL bibliography styles via BetterBibTeX for
;;               Zotero's jsonrpc and http GET interfaces.
;; COPYRIGHT   : (C) 2016  Karl Martin Hegbloom
;;
;;  Initially derived from (text natbib), (C) 2006  Joris van der Hoeven
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This must be named with the "tm-" prefix so that the logic within
;; the bibliography updating routines will call on code here so it can
;; update each point of citation within the document.
;;
(texmacs-module (tm-bb-cayw))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (natbib-author s)
;;   (let* ((i (string-index s #\()))
;;     (if i (substring s 0 i) s)))

;; (define (natbib-year s)
;;   (let* ((i (string-index s #\())
;;          (j (string-index s #\))))
;;     (if (and i j (< i j)) (substring s (+ i 1) j) "?")))

;; (define (natbib-author* s)
;;   (let* ((i (string-index s #\())
;;          (j (string-index s #\)))
;;          (n (string-length s)))
;;     (cond ((not (and i j (< i j))) s)
;;           ((< (+ j 1) n) (substring s (+ j 1) n))
;;           (else (substring s 0 i)))))

;; (tm-define (natbib-get t type-t)
;;   (:secure #t)
;;   (let* ((s (tree-as-string t))
;;          (type (tree-as-string type-t)))
;;     (cond ((== type "author") (natbib-author s))
;;           ((== type "year") (natbib-year s))
;;           ((== type "author*") (natbib-author* s))
;;           (else "?"))))
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; From zotero/chrome/content/zotero/tools/csledit.{xul,js}
;;
;; first                {position : 0}
;; subsequent           {position : 1}
;; ibid                 {position : 2}
;; ibid-with-locator    {position : 3}
;; near-note            {position : 1,  near-note : true}
;;


;;; Use cases:
;;
;; Requirement: During the initial writing of a document, I need to
;;              insert a citation.
;;
;; I select an item from a toolbar, or a menu that initiates the
;; insert citation from Zotero command, OR I use the "\" initiated
;; latex-like shortcut to initiate it, so activating the tag causes it
;; to call the same interactive command.
;;
;; That interactive command uses an http call to better bibtex's cayw
;; interface which pops up the citation entry dialog. When that
;; returns, it returns the needed information as a scheme "reader"
;; compatible string so that calling scheme "read" on it provides the
;; data back to this program that it can use to fill in the fields.
;;
;; It stores the returned data in a cache within the document. The
;; returned data, and data cached, contains the citekey, the prefix,
;; the locator label, the locator, the suffix, the pre-formatted text,
;; for display and printing, for a first citation, a subsequent
;; one(?), an ibid, an ibid with locator, and for a near-note. (The
;; formatted bibliography is returned via a different mechanism, via
;; the schomd jsonrpc interface. The reason we need to do it as a
;; separate step is that the CSL is what defines the bibliography's
;; sort order, taking it out of the scheme code. Maybe someday if
;; TeXmacs can run guile-2.2+, and the EcmaScript language is up to
;; the task, the citeproc.js can be brought inside and used sort of
;; the way the Ruby citeproc does, allowing runtime modifications to
;; the CSL processor for customizations, but that's too much for now.)
;;
;; It must store everything it needs for it to call on Zotero another
;; time for a batch-update (during bibliography update) in case:
;;
;;   * the citation style is changed
;;
;;   * the reference data was editted in Zotero
;;
;; The text that's actually displayed depends upon whether or not
;; there is an occurrance of a visible citation to the same citekey
;; prior to this point in the document. So:
;;
;;   * When a citation is created, it must look and see if it's the
;;     first one in the document or not.
;;
;;     * If it is, then it uses the form for the first occurrance.
;;
;;     * If it's not, then it must take the appropriate form depending
;;       on whether the immediately previous citation is for the same
;;       citekey (so this one shall be an ibid or ibid with locator),
;;       or if there's citations to other citekeys in between a past
;;       occurance of the citekey (so this one is a near-note).
;;
;;     * It must then continue to walk the document tree looking for
;;       citations to the same citekey, so that it can update any
;;       further references to it so they display the correct form.
;;
;; If this proves to be too slow, then all of this tree-walking will
;; need to be deferred to the bibliography update "batch" operation. I
;; prefer updating displayed forms from cached data at time of
;; entering the text, and will code that first try.
;;
;;
;; Requirement: After the document has citations within in, I need to
;;              initialize or update the bibliography and recheck the
;;              individual citations' display strings in case the
;;              reference manager data was updated in Zotero. I do
;;              that by selecting the update bibliography menu item.
;;
;;
;; During this process, it must walk the document's citations... as
;; above, build the temp.aux (? or will that happen via a different
;; mechanism?) and then call on the 'texmacs-zotero-bibtex' (? or will
;; that happen for me also?) to update the bibliography(ies) itself
;; (themselves).
;;
;;
;; Implementation: I think that the cayw HTTP GET interface is best
;;     used for the initial entry of citations, and for editting of
;;     individual cites, as in reinvoking it at a location where
;;     there's already a cite, and having the citekey or citekeys show
;;     up in the opened dialog for modification, to add or remove
;;     citekeys, prefix, suffix, or locator information. The schomd
;;     jsonrpc interface is better for the batch update of the
;;     bibliography(ies), and for the batch update of the data for
;;     each cite location already there in the document.
;;
;; Todo: Make a new cayw interface method that returns the appopriate
;;       data as scheme data. (Since guile-1.8 does not support json
;;       and it's just as easy to produce scheme reader syntax.) It
;;       must accept an optional short list of one or more citekeys
;;       along with their associated prefix, suffix, locator label,
;;       and locator data. It must also of course accept a style
;;       parameter. That parameter may as well be taken from the
;;       associated bibliography, since that's the obvious convention,
;;       and the way that the other tm-STYLE bibliographic styles
;;       work.
;;
;; Todo: Make a new schomd interface method that returns the
;;       appropriate data for updating each citation in the
;;       document. This can use jsonrpc. It can either return a json
;;       object, and be handled by a guile-2 script, or it can return
;;       a scheme string. Since it will already be talking jsonrpc, it
;;       may as well return json... and the guile script can send a
;;       scheme reader string to TeXmacs which runs guile-1.8, which
;;       does not support the (web client) or (json) modules. Someday
;;       the extra layer can be removed, when TeXmacs is running with
;;       guile-2 internally. Having it return a json object seems more
;;       universally useful, to other programs not yet written. On the
;;       other hand, a texmacs document object encoded as a scheme
;;       reader string would be the simplest thing to utilize, and a
;;       call to the jsonrpc method via (system "curl ...") might be
;;       the easiest way to make this work. The main requirement here
;;       is that it must accept a potentially large list of citekeys
;;       as input, and then return, for each citekey, an object that
;;       contains that citekey associated with the necessary data as
;;       described above. That's why I'm thinking of the schomd
;;       interface, because it's a POST method, and of course jsonrpc
;;       is the natural input format for the Better BibTeX for Zotero
;;       system.
;;

(use-modules (ice-9 hash-table))

(define (locator sh-loc)
  (let ((sh-loc-to-locator-h
         (alist->hash-table
          '(("art." . "article")
            ("ch." . "chapter")
            ("subch." . "subchapter")
            ("col." . "column")
            ("fig." . "figure")
            ("l." . "line")
            ("n." . "note")
            ("no." . "issue")
            ("op." . "opus")
            ("p." . "page")
            ("para." . "paragraph")
            ("subpara." . "subparagraph")
            ("pt." . "part")
            ("r." . "rule")
            ("sec." . "section")
            ("subsec." . "subsection")
            ("Sec." . "Section")
            ("sv." . "sub verso")
            ("sch." . "schedule")
            ("tit." . "title")
            ("vrs." . "verse")
            ("vol." . "volume")))))
    (hash-ref sh-loc-to-locator-h sh-loc "page")))


(define (cayw-get-latex-cite)
  (cadadr
   (tree->stree
    (tree-import
     (string-append 
      "http://localhost:23119/better-bibtex/cayw?format=latex&timestamp="
      (number->string (vector-ref (times) 0)))
     "verbatim"))))

(tm-define (cayw-insert-latex-cite)
  (:secure #t)
  (string->tree (cayw-get-latex-cite)))
