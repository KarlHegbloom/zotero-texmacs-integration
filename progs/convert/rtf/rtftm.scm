
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : rtftm.scm
;; DESCRIPTION : conversion of RTF trees to TeXmacs trees
;; COPYRIGHT   : (C) 2016  Karl M Hegbloom
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;
;;; This is not a full RTF document converter. For now, all it knows how to do
;;; is convert the citeproc.js RTF output into TeXmacs.
;;;

(texmacs-module (convert rtf rtftm)
  (:use (kernel texmacs tm-convert)))

;; (:use
;;     (convert tools tmlength)
;;     (convert tools tmcolor)
;;     (convert tools old-tmtable)
;;     (convert tools stm)
;;     (convert tools environment)))

(define (url-temp-ext ext)
  (url-glue (url-temp) (string-append "." ext)))

(tm-define (rtf-snippet->latex-snippet rtf_str)
  (let ((temp_rtf (url-temp-ext "rtf"))
        (temp_tex (url-temp-ext "tex")))
    
    (string-save rtf_str temp_rtf)
    (system (string-append
             "unrtf -P " (dirname (%search-load-path "convert/rtf/latex.conf"))
             " -t latex " (url->unix temp_rtf) 
             " | sed -re 's,(\\w\\.}?) ,\\1\\\\hspace{1spc},g'"
             " > " (url->unix temp_tex)))
    (system-remove temp_rtf)
    (let ((tex (string-load temp_tex)))
      (system-remove temp_tex)
      tex)))

(tm-define (rtf-snippet->texmacs s)
  (latex->texmacs
   (parse-latex
    (rtf-snippet->latex-snippet s))))
    

;; (converter rtf-document rtf-stree
;;   (:function parse-rtf-document))

;; (converter rtf-stree texmacs-stree
;;   (:function rtf->texmacs))
