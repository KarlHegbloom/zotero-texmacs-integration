;; (compat guile-2) -- guile 2.0 interface for older guiles
;; Copyright (C) 2009, 2010 Andy Wingo <wingo at pobox dot com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Compatibility library, so that users may write code for the Guile 2.0
;; interface and have it run on older guiles.
;;
;;; Code:

(define-module (compat guile-2))

(cond-expand
 (guile-2
  ;; have to define something, so we don't get "no code for module"
  (define hello #t))
 (else
  (use-modules (ice-9 syncase))
  (module-use! (module-public-interface (current-module))
               (resolve-interface '(ice-9 syncase)))
  (define-public guile-2 syncase)

  (export eval-when)
  (define-macro (eval-when conditions . body)
    (if (or (memq 'eval conditions)
            (memq 'load conditions)
            (memq 'expand conditions))
        `(begin . ,body)
        '(begin)))

  (read-hash-extend #\' (lambda (chr port)
                          (list 'syntax (read port))))))
