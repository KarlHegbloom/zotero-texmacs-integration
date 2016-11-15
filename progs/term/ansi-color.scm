;; (term ansi-color) -- color output using ANSI escape sequences
;; Copyright (C) 2003  Richard Todd

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

#!
;;; Commentary:
@cindex terminals, ANSI color codes for
@cindex ANSI color codes
@cindex color codes, ANSI
The @samp{(term ansi-color)} module generates ANSI escape
sequences for colors.  Here is an example of the module's use:

@lisp
;; method one: safer, since you know the colors
;; will get reset
(display (colorize-string "Hello!\n" 'RED 'BOLD 'ON-BLUE))

;; method two: insert the colors by hand
(for-each display
          (list (color 'RED 'BOLD 'ON-BLUE)
                "Hello!"
                 (color 'RESET)))
@end lisp
;;; Code:
!#

(define-module (term ansi-color)
     #:export  (color
                colorize-string)
     #:use-module (srfi srfi-1)   ; for 'remove'
     #:use-module (srfi srfi-13)) ; for 'string-join' 

(define ansi-color-tables
  (let ((table (make-hash-table 23)))
    (hashq-set! table 'CLEAR "0")
    (hashq-set! table 'RESET "0")
    (hashq-set! table 'BOLD  "1")
    (hashq-set! table 'DARK  "2")
    (hashq-set! table 'UNDERLINE "4")
    (hashq-set! table 'UNDERSCORE "4")
    (hashq-set! table 'BLINK "5")
    (hashq-set! table 'REVERSE "6")
    (hashq-set! table 'CONCEALED "8")
    (hashq-set! table 'BLACK "30")
    (hashq-set! table 'RED "31")
    (hashq-set! table 'GREEN "32")
    (hashq-set! table 'YELLOW "33")
    (hashq-set! table 'BLUE "34")
    (hashq-set! table 'MAGENTA "35")
    (hashq-set! table 'CYAN "36")
    (hashq-set! table 'WHITE "37")
    (hashq-set! table 'ON-BLACK "40")
    (hashq-set! table 'ON-RED "41")
    (hashq-set! table 'ON-GREEN "42")
    (hashq-set! table 'ON-YELLOW "43")
    (hashq-set! table 'ON-BLUE "44")
    (hashq-set! table 'ON-MAGENTA "45")
    (hashq-set! table 'ON-CYAN "46")
    (hashq-set! table 'ON-WHITE "47")
    table))

(define (color . lst)
"Returns a string containing the ANSI escape sequence for
producing the requested set of attributes. 

The allowed values for the attributes are listed below.  Unknown
attributes are ignored.

@table @asis
@item Reset Attributes
@samp{CLEAR} and @samp{RESET} are allowed and equivalent.

@item Non-Color Attributes
@samp{BOLD} makes text bold, and @samp{DARK} reverses this.
@samp{UNDERLINE} and @samp{UNDERSCORE} are equivalent.  @samp{BLINK}
makes the text blink.  @samp{REVERSE} invokes reverse video.
@samp{CONCEALED} hides output (as for getting passwords, etc.).

@item Foregrond Color Attributes
@samp{BLACK}, @samp{RED}, @samp{GREEN}, @samp{YELLOW}, @samp{BLUE},
@samp{MAGENTA}, @samp{CYAN}, @samp{WHITE}

@item Background Color Attributes
@samp{ON-BLACK}, @samp{ON-RED}, @samp{ON-GREEN}, @samp{ON-YELLOW},
@samp{ON-BLUE}, @samp{ON-MAGENTA}, @samp{ON-CYAN}, @samp{ON-WHITE}
@end table"
  (let ((color-list 
         (remove not 
                 (map (lambda (color) (hashq-ref ansi-color-tables color))
                      lst))))
    (if (null? color-list)
        ""
        (string-append 
         (string #\esc #\[)
         (string-join color-list ";" 'infix)
         "m"))))
  
(define (colorize-string str . color-list)
"Returns a copy of @var{str} colorized using ANSI
escape sequences according to the attributes specified in
@var{color-list}.  At the end of the returned string, the color
attributes will be reset such that subsequent output will not
have any colors in effect.

The allowed values for the attributes are listed in the
documentation for the @code{color} function."
  (string-append
   (apply color color-list)
   str
   (color 'RESET)))

;;; arch-tag: e8dd6a14-490c-417e-a7fe-983939293db1
