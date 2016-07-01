;;; coding: utf-8

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; MODULE      : init-rtf.scm
;; DESCRIPTION : setup rtf converters
;; COPYRIGHT   : (C) 2016  Karl M Hegbloom
;;
;; This software falls under the GNU general public license version 3 or later.
;; It comes WITHOUT ANY WARRANTY WHATSOEVER. For details, see the file LICENSE
;; in the root directory or <http://www.gnu.org/licenses/gpl-3.0.html>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(texmacs-module (convert rtf init-rtf))


(define (rtf-recognizes-at? s pos)
  (set! pos (format-skip-spaces s pos))
  (cond ((format-test? s pos "{\\rtf ") #t)
        (else #f)))

(define (rtf-recognizes? s)
  (and (string? s) (rtf-recognizes-at? s 0)))


(define-format rtf
  (:name "RTF")
  (:suffix "rtf")
  (:recognize rtf-recognizes?))


;; (lazy-define (convert rtf rtftm) parse-rtf-snippet)
;; (lazy-define (convert rtf rtftm) parse-rtf-document)
;; (lazy-define (convert rtf rtftm) rtf->texmacs)

;; (converter rtf-snippet latex-snippet
;;   `(:shell "unrtf -P " ,(dirname (%search-load-path "convert/rtf/latex.conf"))
;;            " -t latex " from " > " to)

