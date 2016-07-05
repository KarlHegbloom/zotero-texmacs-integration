(texmacs-module (insert-legal-templates)
  (:use (generic document-style)))

;; LaTeX / Hybrid kbd commands:
;;
;; \Legal-Heading
;; \Cert-of-Service
;;
(kbd-commands
  ("Legal-Heading" "Insert Legal Heading"
   (insert-legal-heading))
  ("Cert-of-Service" "Insert Legal Certificate of Service"
   (insert-legal-cert-of-service))
  )


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




;;; Local Variables:
;;; mode: scheme
;;; fill-column: 132
;;; End:
