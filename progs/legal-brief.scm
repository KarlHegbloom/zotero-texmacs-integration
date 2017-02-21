;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺
;;;
;; MODULE      : legal-brief-style.scm
;; DESCRIPTION : Scheme-side support for the Legal Brief Style.
;; COPYRIGHT   : (C) 2016  Karl M. Hegbloom <karl.hegbloom@gmail.com>
;;
;; This software falls under the GNU general public license version 3 or
;; later. It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file
;; LICENSE in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>
;;
;;;
(texmacs-module (legal-brief)
  (:use (generic document-style))
  (:use (utils library cursor))
  (:use (tm-zotero))
  )

(texmacs-modes
  (in-legal-brief-style% (style-has? "legal-brief-style")))

(tm-define (lb-ext:is-in-legal-brief-style?)
  (:secure)
  (if (in-legal-brief-style?)
      "true"
      "false"))


(define-public (in-paragraph? . t)
  (let ((found #f)
        (t (or (and (pair? t)
                    (not (null? t))
                    (tree? (car t))
                    (car t))
               (cursor-tree))))
    (cursor-after
     (tree-go-to t :start)
     (go-start-paragraph)
     (set! found (tree-is? (cursor-tree) 'paragraph)))
    found))

(define-public (in-subparagraph? . t)
  (let ((found #f)
        (t (or (and (pair? t)
                    (not (null? t))
                    (tree? (car t))
                    (car t))
               (cursor-tree))))
    (cursor-after
     (tree-go-to t :start)
     (go-start-paragraph)
     (set! found (tree-is? (cursor-tree) 'subparagraph)))
    found))


(tm-define (kbd-enter t shift?)
  (:require (and (inside? 'paragraph)
                 (not (inside? 'hybrid))
                 (not (is-zfield? t))
                 (in-legal-brief-style?)))
  (go-end-paragraph)
  (insert-return)
  (insert '(paragraph "")))

(tm-define (kbd-enter t shift?)
  (:require (and (not (inside? 'paragraph))
                 (not (inside? 'hybrid))
                 (not (is-zfield? t))
                 (in-legal-brief-style?)
                 (in-subparagraph?)))
  (when shift?
    (go-end-paragraph))
  (insert-return)
  (insert '(subparagraph "")))

(tm-define (kbd-enter t shift?)
  (:require (and (not (inside? 'paragraph))
                 (not (inside? 'hybrid))
                 (not (is-zfield? t))
                 (in-legal-brief-style?)
                 (in-paragraph?)))
  (when shift?
    (go-end-paragraph))
  (insert-return)
  (insert '(paragraph "")))


(tm-define (kbd-enter t shift?)
  (:require (and (not (inside? 'hybrid))
                 (in-legal-brief-style?)
                 ;; (in-section?)
                 (or (inside? 'part)
                     (inside? 'chapter)
                     (inside? 'section)
                     (inside? 'subsection)
                     (inside? 'subsubsection))))
  (go-end-line)
  (insert-return)
  (insert '(paragraph "")))


;; TODO Not tested yet.
(tm-define (variant-circulate t forward?)
  (:require (or (in-paragraph? t)
                (in-subparagraph? t)))
  (go-start-paragraph)
  (tree-go-to (cursor-tree) 1)
  (former (cursor-tree) forward?))


;;; LaTeX / Hybrid kbd commands:
;;;
;;; \Legal-Heading
;;; \Cert-of-Service
;;;
(kbd-commands
  ("Legal-Heading" "Insert Legal Heading"
   (insert-legal-heading))
  ("Cert-of-Service" "Insert Legal Certificate of Service"
   (insert-legal-cert-of-service))
  ("Appellate-Issue" "Insert Appellate Issue Template"
   (insert-appellate-issue-template))
  )


;;; Todo:
;;;       Document how I created these templates, to help people with creating their own templates. Also, research whether there's
;;;       already some work begin in the TeXmacs system for supporting template documents/sub-documents, etc.
;;;
;;;       Fill out form created using TeXmacs widgets to obtain information to put into this legal heading and into the document
;;;       metadata. That information can come from the database that TeXmacs supports. Memoize responses and add them to saved items
;;;       for use later in autocompletion? Or put them into a database that can be editted like the BibTeX databases can; allow
;;;       narrowing of display by various elements, addition and deletion of them, etc.
;;;
;;;       So a style sheet can be made that has tags that expand to be the value of Title, Preparer, etc., and so when those are
;;;       changed, the document updates; But it needs to be possible to "freeze" the document, and so just like how a zcite can
;;;       update when the reference database is updated, values of these fields can update when the database for them is
;;;       updated... but the values should also be part of the document and once a document is "filed" or "published", they need to
;;;       be frozen; at least at a "revision", and care taken so that old documents don't "break"; self-contained is usually
;;;       good. What about external style-sheet, such as tm-zotero.ts or legal-brief.ts ? Perhaps there can be a version declaration
;;;       and the old copy pulled from revision control if needed, or the version suffix added between the end of the style sheet's
;;;       file-name and the .ts file-name extension? Or maybe there can be a function that puts all of the style sheet stuff that
;;;       the document uses bundled into the preamble, the way you can in LaTeX, to make it self-contained?
;;;
;;;       Think about how to automatically form a tag name from this information for the feature in Juris-M / Zotero that puts a tag
;;;       on everything cited by the document. Remember that in XChange, searching for a case number returns the same case number in
;;;       use in more than one state court, so it must also contain information about which court (SL3D). The assigned judge can
;;;       change, but that is constant; it's essentially the unique key.
;;;
;;;       Think about interface to in-office document management systems, and to court e-filing systems... perhaps to things like
;;;       github, blogs (wordpress), or sites that allow publication of academic research articles? Automatic upload, updating,
;;;       revision/version control, collaborative editting?, keyword / tag selection, etc.
;;;
;;;       Other information that should be part of the document, stored inside of it: Date begun, Date filed, (Revision number? git
;;;       or svn?). I want to know whether or not it was filed, and what date it was filed. I name the files with the ISO-8601 date
;;;       (%Y-%m-%d_CASENUMBER_Title.tm), and that ought to be the filing date. But when I begin the document, I name it with the
;;;       date that I begin writing it, or sometimes it's due date... need to standardize that, and make it easy to automatically
;;;       rename the file based on the filing date, so it can be renamed to the filing date the day that is done.
;;;
;;;       Look at LaTeX pdfpages, xwatermark ("Bates" numbering of exhibits), and at http://www.sejda.com and sejda-console and
;;;       think about how to include exhibits into TeXmacs documents, with the ability to create locus and references into those
;;;       exhibit attachments, referencing them by their symbolic PDF outline's table of contents, document name, section, relative
;;;       page, and absolute page within this document that includes that one as an exhibit... And... Is it possible for an attached
;;;       document like that to be both displayed in-line, included the way sejda-console can, reparenting it's PDF outlines under
;;;       the Exhibit N outline heading of this document, and have the same PDF stream available as a PDF attachment? With
;;;       sejda-console, you can split them off easily enough, but a normal PDF reader usually lets you write out the attachments as
;;;       files. Also, attached video, audio, etc.? Someday when Guile-2.x is part of TeXmacs and it's ECMAscript (JavaScript)
;;;       implementation is up to it, perhaps creation of PDF with embedded JavaScript can be handled by TeXmacs, for dealing with
;;;       things like embedded audio/video with subtitles, chapters, etc.?
;;;
;;;
;;; (dropdown Jurisdiction Chooser)
;;;   (dropdown Utah,Federal,...)
;;;   (dropdown Civil,Criminal,Appellate,Supreme)
;;;
;;;     Affects document margins, line-spacing, single or double-sided, etc. formatting.
;;;
;;; Preparer:name,address,email,phone (dropdown (atty, pro se), when atty, dropdown (for petitioner, for respondent) + bar #
;;;
;;; Court heading (what court, it's address)
;;;
;;; Petitioner:name (If is appellate, then (dropdown Appellee,Appellant))
;;;           :address,email,phone
;;; Respondent:name (If is appellate, then (dropdown Appellee,Appellant))
;;;
;;; Document title
;;; Case number (or blank for initial petition without assigned case number)
;;; (? Discovery tier)
;;; (? Commissioner:name,bar#)
;;; Judge:name,bar#
;;;
;;; So the document metadata will have the author set to the name of the preparer, the title to the document title. The metadata
;;; keywords ought to have the the Juris-M/TeXmacs tag, the jurisdiction, the case number, the type of case...
;;;
;;; Salt Lake Third District court case numbers are encoded. The first two digits is the last two digits of the year the case was
;;; opened. The next 3 digits are a case-type code, and the last 4 digits are a serial number for cases... probably for that type,
;;; or they could not have more than 9999 cases total, of all types, in a year, and they likely have more than that, so it's
;;; probably a serial number for cases of that particular type. Wow, I filed in February, and the serial numbers for my PCRA's are
;;; 1178,1179,1180? Get in line to fill out a form! (Convict them all and let the appellate court sort them out?)
;;;
;;;
;;; Todo: Farther in the future: e-filing of PDF, and production of RTF for some documents, since they require RTF for things like
;;; proposed orders to be signed by the judge, since they are e-signed and for some reason it uses RTF not PDF for that. I think
;;; that PDF would work better and be more portable... So another Todo: research e-filing etc.
;;;

(tm-define (insert-legal-heading)
  (insert
   '(document
      (with "par-sep" "0.2fn"
        (document
          (concat
            (blanc-page)
            (tabular
             (tformat
              (cwith "5" "5" "1" "1" "cell-bsep" "0.2fn")
              (cwith "6" "6" "1" "1" "cell-rsep" "0")
              (cwith "6" "6" "1" "1" "cell-bsep" "0")
              (cwith "6" "6" "1" "1" "cell-tsep" "0")
              (twith "table-halign" "l")
              (twith "table-valign" "f")
              (cwith "6" "6" "1" "1" "cell-lsep" "0.25tab")
              (twith "table-lsep" "0")
              (twith "table-rsep" "0")
              (twith "table-bsep" "0")
              (twith "table-tsep" "0")
              (table (row (cell (concat "Name of Document Preparer")))
                     (row (cell (concat "Address")))
                     (row (cell "City, State, Zip"))
                     (row (cell (hlink "Preparer@Email.Com" "mailto:Preparer@Email.Com")))
                     (row (cell "+tel.number"))
                     (row (cell (concat "Proceeding " (with "font-shape" "italic" "pro se") ".")))))))
          (with "par-mode" "center"
            (document
              (tabular
               (tformat
                (cwith "1" "1" "1" "-1" "cell-lsep" "0")
                (cwith "1" "1" "1" "-1" "cell-rsep" "0")
                (cwith "1" "1" "1" "-1" "cell-lborder" "0")
                (cwith "1" "1" "1" "-1" "cell-rborder" "0")
                (twith "table-lborder" "0")
                (twith "table-rborder" "0")
                (twith "table-bborder" "0")
                (twith "table-tborder" "0")
                (cwith "1" "1" "1" "1" "cell-block" "yes")
                (cwith "1" "1" "1" "1" "cell-row-span" "1")
                (cwith "1" "1" "1" "1" "cell-col-span" "2")
                (cwith "1" "1" "1" "-1" "cell-tborder" "1pt")
                (cwith "1" "1" "1" "-1" "cell-bborder" "1pt")
                (cwith "1" "1" "1" "-1" "cell-tsep" "1ex")
                (cwith "1" "1" "1" "-1" "cell-bsep" "0.75ex")
                (cwith "2" "2" "1" "-1" "cell-rborder" "0")
                (cwith "2" "2" "1" "1" "cell-rborder" "1pt")
                (cwith "2" "2" "1" "-1" "cell-bborder" "1pt")
                (cwith "2" "2" "1" "1" "cell-block" "yes")
                (cwith "2" "2" "1" "-1" "cell-hpart" "")
                (twith "table-width" "1par")
                (twith "table-hmode" "exact")
                (cwith "2" "2" "1" "-1" "cell-width" "0.5par")
                (cwith "2" "2" "1" "-1" "cell-hmode" "exact")
                (cwith "1" "1" "1" "1" "cell-halign" "c")
                (cwith "1" "1" "1" "1" "cell-valign" "C")
                (cwith "2" "2" "1" "1" "cell-valign" "c")
                (cwith "2" "2" "2" "2" "cell-valign" "c")
                (cwith "2" "2" "1" "1" "cell-lsep" "1.5em")
                (cwith "2" "2" "2" "2" "cell-halign" "c")
                (cwith "2" "2" "2" "2" "cell-hyphen" "t")
                (cwith "2" "2" "2" "2" "cell-block" "auto")
                (cwith "2" "2" "1" "1" "cell-tsep" "1fn")
                (cwith "2" "2" "1" "1" "cell-bsep" "1fn")
                (cwith "2" "2" "2" "2" "cell-bsep" "1fn")
                (cwith "2" "2" "2" "2" "cell-tsep" "1fn")
                (cwith "2" "2" "1" "1" "cell-halign" "l")
                (table (row (cell (document
                                    (concat
                                      (toc-main-2 "Heading")
                                      (name "In the Nth Judicial District Court, Some State City, State of Chaos"))
                                    "       The Nth District Court, 420 Stately State Street, Some State City, State ZipCode"))
                            (cell ""))
                       (row (cell (document
                                    (concat (name "PetitionerName") ",")
                                    (concat (space "1em") "Petitioner,")
                                    ""
                                    (concat (space "2em") "vs.")
                                    ""
                                    (concat (name "RespondentName") ",")
                                    (concat (space "1em") "Respondent.")))
                            (cell (document
                                    (tabular*
                                     (tformat
                                      (twith "table-width" "3in")
                                      (twith "table-hmode" "min")
                                      (cwith "1" "1" "1" "1" "cell-valign" "b")
                                      (cwith "1" "1" "1" "1" "cell-hyphen" "t")
                                      (cwith "1" "1" "1" "1" "cell-halign" "c")
                                      (table (row (cell (document
                                                          (with "par-mode" "center" "font-shape" "small-caps"
                                                            (document
                                                              (concat
                                                                "The" (next-line)
                                                                "Document" (next-line)
                                                                "Title")))))))))
                                    (tabular
                                     (tformat
                                      (cwith "1" "-1" "1" "1" "cell-halign" "r")
                                      (cwith "3" "3" "1" "-1" "cell-bsep" "0.25fn")
                                      (twith "table-tsep" "1fn")
                                      (cwith "1" "1" "1" "1" "cell-hyphen" "n")
                                      (cwith "1" "1" "2" "2" "cell-bsep" "0")
                                      (table (row (cell (concat "Civil Cases" (space "0.2spc") ":"))
                                                  (cell (concat
                                                          "00"
                                                          (space "0.2spc")
                                                          "000"
                                                          (space "0.2spc")
                                                          "0000")))
                                             (row (cell (concat "Commissioner" (space "0.2spc") ":"))
                                                  (cell (name "CommissionerName")))
                                             (row (cell (concat "Judge" (space "0.2spc") ":"))
                                                  (cell (name "JudgeName"))))))))))))))))
      (concat
        (vspace* "0.5fn")
        (with "font-shape" "italic" "Pax Domine,") " ")))
  (open-document-metadata))


(tm-define (insert-legal-cert-of-service)
  (insert
   '(document
      (center
       (document
         (concat
           (blanc-page "")
           (large (name "Certificate of Mailing or Service")))))
      (concat (vspace* "1.0fn") "I certify that a true and correct copy of the foregoing:")
      (with "par-mode" "center"
        (document
          (concat
            (vspace* "1fn")
            (name (concat
                    "Title"
                    (next-line)
                    "of"
                    (next-line)
                    "Document"))
            (vspace "1fn"))))
      "was mailed, emailed, or hand-delivered to:"
      (letter-header
       (document
         (destination
          (document
            "RecipientAddress"
            (em "Attorney for PetitionerOrRespondent, OR pro se.")))))
      (concat
        "This document was mailed, emailed, or hand delivered on " (strong "DATE") ".")
      (signature
       (document
         (concat "Document Preparer"))))))


(tm-define (insert-appellate-issue-template)
  (insert
   `(document
      (subsection "[Name the issue]")
      "[Succinctly state the issue.]"
      (subsubsection "Determinative Law")
      "[Cite any statutes, rules, or cases determinative of the issue.]"
      (subsubsection "Standard of Review")
      "[State applicable standard of appellate review for the issue, with supporting authority.]")))

;;; Local Variables:
;;; mode: scheme
;;; fill-column: 132
;;; End:
