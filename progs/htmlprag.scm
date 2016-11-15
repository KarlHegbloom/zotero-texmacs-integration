;; (htmlprag) -- pragmatic parsing of real-world HTML
;; Copyright (C) 2003-2004 Neil W. Van Dyke <neil at neilvandyke.org>
;; Modified 2004 by Andy Wingo to fit in with guile-lib.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.

;; The license of the code that this is based on, when it came from Neil
;; W. Van Dyke, was the LGPL version 2.1. Neil notes that other
;; licensing options for his code are available; interested parties
;; should contact him directly.

;;; Commentary:
;;
;;; HtmlPrag provides permissive HTML parsing capability to Scheme programs,
;;; which is useful for software agent extraction of information from Web
;;; pages, for programmatically transforming HTML files, and for implementing
;;; interactive Web browsers.  HtmlPrag emits ``SHTML,'' which is an encoding
;;; of HTML in [SXML], so that conventional HTML may be processed with XML
;;; tools such as [SXPath] and [SXML-Tools].  Like [SSAX-HTML], HtmlPrag
;;; provides a permissive tokenizer, but also attempts to recover structure.
;;; HtmlPrag also includes procedures for encoding SHTML in HTML syntax.
;;;
;;; The HtmlPrag parsing behavior is permissive in that it accepts erroneous
;;; HTML, handling several classes of HTML syntax errors gracefully, without
;;; yielding a parse error.  This is crucial for parsing arbitrary real-world
;;; Web pages, since many pages actually contain syntax errors that would
;;; defeat a strict or validating parser.  HtmlPrag's handling of errors is
;;; intended to generally emulate popular Web browsers' interpretation of the
;;; structure of erroneous HTML.  We euphemistically term this kind of parse
;;; ``pragmatic.''
;;;
;;; HtmlPrag also has some support for [XHTML], although XML namespace
;;; qualifiers [XML-Names] are currently accepted but stripped from the
;;; resulting SHTML.  Note that valid XHTML input is of course better handled
;;; by a validating XML parser like [SSAX].
;;;
;;; To receive notification of new versions of HtmlPrag, and to be polled for
;;; input on changes to HtmlPrag being considered, ask the author to add you to
;;; the moderated, announce-only email list, @code{htmlprag-announce}.
;;;
;;; Thanks to Oleg Kiselyov and Kirill Lisovsky for their help with SXML.
;;
;;; Code:

(define-module (htmlprag))

;; Exports defined at the end of the file

;; THIS FILE GENERATED Thu May 13 21:41:40 EDT 2004 -- DO NOT EDIT MANUALLY
;; ############# BEGIN CANONICAL htmlprag.scm #############
;;; @Package     HtmlPrag
;;; @Subtitle    Pragmatic Parsing of HTML to SHTML and SXML
;;; @HomePage    http://www.neilvandyke.org/htmlprag/
;;; @Author      Neil W. Van Dyke
;;; @AuthorEmail neil@@neilvandyke.org
;;; @Version     0.11
;;; @Date        13 May 2004

;; $Id: htmlprag.scm,v 1.304 2004/05/14 01:28:51 neil Exp $

;;; @legal
;;; Copyright @copyright{} 2003-2004 Neil W. Van Dyke.  This program is Free
;;; Software; you can redistribute it and/or modify it under the terms of the
;;; GNU Lesser General Public License as published by the Free Software
;;; Foundation; either version 2.1 of the License, or (at your option) any
;;; later version.  This program is distributed in the hope that it will be
;;; useful, but without any warranty; without even the implied warranty of
;;; merchantability or fitness for a particular purpose.  See the GNU Lesser
;;; General Public License [LGPL] for more details.  For other license options
;;; and commercial consulting, contact the author.
;;; @end legal

;;; @section Introduction

;;; HtmlPrag provides permissive HTML parsing capability to Scheme programs,
;;; which is useful for software agent extraction of information from Web
;;; pages, for programmatically transforming HTML files, and for implementing
;;; interactive Web browsers.  HtmlPrag emits ``SHTML,'' which is an encoding
;;; of HTML in [SXML], so that conventional HTML may be processed with XML
;;; tools such as [SXPath] and [SXML-Tools].  Like [SSAX-HTML], HtmlPrag
;;; provides a permissive tokenizer, but also attempts to recover structure.
;;; HtmlPrag also includes procedures for encoding SHTML in HTML syntax.
;;;
;;; The HtmlPrag parsing behavior is permissive in that it accepts erroneous
;;; HTML, handling several classes of HTML syntax errors gracefully, without
;;; yielding a parse error.  This is crucial for parsing arbitrary real-world
;;; Web pages, since many pages actually contain syntax errors that would
;;; defeat a strict or validating parser.  HtmlPrag's handling of errors is
;;; intended to generally emulate popular Web browsers' interpretation of the
;;; structure of erroneous HTML.  We euphemistically term this kind of parse
;;; ``pragmatic.''
;;;
;;; HtmlPrag also has some support for [XHTML], although XML namespace
;;; qualifiers [XML-Names] are currently accepted but stripped from the
;;; resulting SHTML.  Note that valid XHTML input is of course better handled
;;; by a validating XML parser like [SSAX].
;;;
;;; To receive notification of new versions of HtmlPrag, and to be polled for
;;; input on changes to HtmlPrag being considered, ask the author to add you to
;;; the moderated, announce-only email list, @code{htmlprag-announce}.
;;;
;;; Thanks to Oleg Kiselyov and Kirill Lisovsky for their help with SXML.

;;; @section Portability

;;; HtmlPrag officially requires R5RS, [SRFI-6], and [SRFI-23], but is known to
;;; also work on some non-R5RS implementations.  The current version tests
;;; successfully under Bigloo 2.6d, Chicken 1.22, Gauche 0.7.4.2, Guile 1.6.4,
;;; MIT Scheme 7.7.90, PLT MzScheme 206p1, RScheme 0.7.3.3-b20, SISC 1.8.7
;;; (using Kaffe 1.1.4), and STklos 0.55.  With a minor change to the source
;;; code, HtmlPrag also tests successfully under Scheme 48 0.57 and Scsh 0.6.3.
;;;
;;; Kawa has been removed temporarily from the test list, but should run if
;;; Sun's Java implementation can be used.  SXM has removed temporarily from
;;; the test list, until the test suite code can be adjusted to not exceed
;;; SXM's limit on literals.

;; TODO: Note about packagings.
;;
;; Some packagings of HtmlPrag for particular Scheme implementations are
;; available from the HtmlPrag Web page and elsewhere.

;; TODO: Note conventional prefix option with module systems that support it.
;;
;; @lisp
;; (require (prefix htmlprag: (lib "htmlprag.ss" "htmlprag")))
;; @end lisp

;;; In addition to the documented public bindings, the HtmlPrag source code
;;; includes some internal-use-only toplevel bindings.  The names of these
;;; begin with the ``@code{htmlprag-internal:}'' prefix.  Packagings of
;;; HtmlPrag for particular Scheme implementations should suppress these
;;; bindings from export when possible.

;; The following bindings are used internally by HtmlPrag for portability,
;; with the intention that packagings of HtmlPrag use faster or more
;; appropriate bindings for the particular Scheme implementation.

;; @defproc htmlprag-internal:a2c num
;;
;; Returns the character with ASCII value @var{num}.  In most Scheme
;; implementations, this is the same as @code{integer->char}.  Two exceptions
;; are Scheme 48 0.57 and Scsh 0.6.3, for which the user must manually edit
;; file @code{htmlprag.scm} to bind this variable to @code{ascii->char}.  A
;; future version of HtmlPrag will automatically use @code{ascii->char} where
;; available.

(define htmlprag-internal:a2c integer->char)

;; @defproc htmlprag-internal:append! a b
;;
;; Returns a concatenation of lists @var{a} and @var{b}, modifying the tail of
;; @var{a} to point to the head of @var{b} if both lists are non-null.  A
;; future version should use the more general @code{append!} where available.

(define (htmlprag-internal:append! a b)
  (cond ((null? a) b)
        ((null? b) a)
        (else      (let loop  ((sub a))
                     (if (null? (cdr sub))
                         (begin (set-cdr! sub b)
                                a)
                         (loop (cdr sub)))))))

;; @defproc htmlprag-internal:reverse!ok lst
;;
;; Returns a reversed list @var{lst}, possibly destructive.  A future version
;; will use @code{reverse!} where available, and @code{reverse} elsewhere.

(define htmlprag-internal:reverse!ok reverse)

;; @defproc htmlprag-internal:down str
;;
;; Returns a string that is equivalent to @var{str} with all characters mapped
;; to lowercase, as if by @code{char-downcase}, without mutating @var{str}.  A
;; future version should use the Scheme implementation's native nondestructive
;; procedure where available.

(define (htmlprag-internal:down s)
  (list->string (map char-downcase (string->list s))))

;; @defproc htmlprag-internal:error proc-str msg obj
;;
;; For Bigloo, this is changed to:
;;
;; @lisp
;; (define htmlprag-internal:error error)
;; @end lisp

(define (htmlprag-internal:error p m o) (error (string-append p " - " m) o))

;; TODO: Make htmlprag-internal:error be syntax.

;; @defproc htmlprag-internal:down!ok str
;;
;; Returns a string that is equivalent to @var{str} with all characters mapped
;; to lowercase, as if by @code{char-downcase}, possibly mutating @var{str}.
;; A future version should use the Scheme implementation's native destructive
;; or nondestructive procedure where available.

(define htmlprag-internal:down!ok htmlprag-internal:down)

;; @defproc htmlprag-internal:gosc os
;;
;; One-shot version of the conventional @code{get-output-string}.  The result
;; of any subsequent attempt to write to the port or get the output string is
;; undefined.  This may or may not free up resources.

(define (htmlprag-internal:gosc os)
  (let ((str (get-output-string os)))
    ;; Note: By default, we don't call close-output-port, since at least one
    ;; tested Scheme implementation barfs on that.
    ;;
    ;; (close-output-port os)
    str))

;; @defvar htmlprag-internal:at
;;
;; Constant bound to the symbol @code{@@}.  This is to make code portable to
;; Scheme implementations with readers that cannot read @code{@@} as a symbol.
;; (Actually, RScheme can now read @code{@@}, which leaves Stalin as the only
;; one the author knows of, so we'll probably go back to just using literal
;; @code{@@} symbols.

(define htmlprag-internal:at (string->symbol "@"))

;;; @section SHTML and SXML

;; TODO: Introduce SHTML.

;;; Some constants and a procedure are defined for convenience and portability
;;; when examining the SHTML produced by the tokenizer and parser.

;;; @defvar  shtml-comment-symbol
;;; @defvarx shtml-decl-symbol
;;; @defvarx shtml-empty-symbol
;;; @defvarx shtml-end-symbol
;;; @defvarx shtml-entity-symbol
;;; @defvarx shtml-pi-symbol
;;; @defvarx shtml-start-symbol
;;; @defvarx shtml-text-symbol
;;; @defvarx shtml-top-symbol
;;;
;;; These variables are bound to the following case-sensitive symbols used in
;;; SHTML, respectively: @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*},
;;; @code{*END*}, @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*},
;;; and @code{*TOP*}.  These can be used in lieu of the literal symbols in
;;; programs read by a case-insensitive Scheme reader.

(define shtml-comment-symbol (string->symbol "*COMMENT*"))
(define shtml-decl-symbol    (string->symbol "*DECL*"))
(define shtml-empty-symbol   (string->symbol "*EMPTY*"))
(define shtml-end-symbol     (string->symbol "*END*"))
(define shtml-entity-symbol  (string->symbol "*ENTITY*"))
(define shtml-pi-symbol      (string->symbol "*PI*"))
(define shtml-start-symbol   (string->symbol "*START*"))
(define shtml-text-symbol    (string->symbol "*TEXT*"))
(define shtml-top-symbol     (string->symbol "*TOP*"))

;;; @defvar  shtml-named-char-id
;;; @defvarx shtml-numeric-char-id
;;;
;;; These variables are bound to the SHTML entity public identifier strings
;;; for symbolic and numeric character entities.  These strings are currently
;;; @code{"additional"} and @code{"additional-char"}, respectively, but are
;;; likely to change in a future version of HtmlPrag, so programs should use
;;; the bindings rather than the literal strings directly.

(define shtml-named-char-id   "additional")
(define shtml-numeric-char-id "additional-char")

;; TODO: Make public procedures for creating character entities, since the
;;       current SHTML syntax for them is pretty nasty.

;;; @defproc shtml-entity-value entity
;;;
;;; Yields the value for the SHTML entity.  Values of named entities are
;;; symbols, and values of numeric entities are numbers.  For example:
;;;
;;; @lisp
;;; (define (f s) (shtml-entity-value (car (cdr (html->shtml s)))))
;;; (f "&nbsp;")  @result{} nbsp
;;; (f "&#2000;") @result{} 2000
;;; @end lisp

(define (shtml-entity-value entity)
  (if (and (list? entity)
           (= (length entity) 3)
           (eqv? (car entity) shtml-entity-symbol))
      (let ((public-id (list-ref entity 1))
            (system-id (list-ref entity 2)))
        (cond ((equal? public-id shtml-named-char-id)
               (string->symbol system-id))
              ((equal? public-id shtml-numeric-char-id)
               (string->number system-id))
              (else (htmlprag-internal:error "shtml-entity-value"
                                             "invalid entity public id"
                                             public-id))))
      (htmlprag-internal:error "shtml-entity-value"
                               "not an entity"
                               entity)))

;;; @section Tokenizing

;;; The tokenizer is used by the higher-level structural parser, but can also
;;; be called directly for debugging purposes or unusual applications.  Some of
;;; the list structure of tokens, such as for start tag tokens, is mutated and
;;; incorporated into the SHTML list structure emitted by the parser.

;; TODO: Document the token format.

;;; @defproc make-html-tokenizer in normalized?
;;;
;;; Constructs an HTML tokenizer procedure on input port @var{in}.  If boolean
;;; @var{normalized?} is true, then tokens will be in a format conducive to use
;;; with a parser emitting normalized SXML.  Each call to the resulting
;;; procedure yields a successive token from the input.  When the tokens have
;;; been exhausted, the procedure returns the null list.  For example:
;;;
;;; @lisp
;;; (define input (open-input-string "<a href=\"foo\">bar</a>"))
;;; (define next  (make-html-tokenizer input #f))
;;; (next) @result{} (a (@@ (href "foo")))
;;; (next) @result{} "bar"
;;; (next) @result{} (*END* a)
;;; (next) @result{} ()
;;; (next) @result{} ()
;;; @end lisp

(define make-html-tokenizer
  ;; TODO: Have the tokenizer replace contiguous whitespace within individual
  ;;       text tokens with single space characters (except for when in `pre'
  ;;       and verbatim elements).  The parser will introduce new contiguous
  ;;       whitespace (e.g., when text tokens are concatenated, invalid end
  ;;       tags are removed, whitespace is irrelevant between certain
  ;;       elements), but then the parser only has to worry about the first and
  ;;       last character of each string.  Perhaps the text tokens should have
  ;;       both leading and trailing whitespace stripped, and contain flags for
  ;;       whether or not leading and trailing whitespace occurred.
  (letrec ((no-token '())

           ;; TODO: Maybe make this an option.
           (verbatim-to-eof-elems '(plaintext))

           ;; TODO: Implement proper parsing of `verbatim-pair-elems' elements.
           ;;       Note that we must support invalid termination like this:
           (verbatim-pair-elems '(script server style xmp))

           (ws-chars (list #\space
                           (htmlprag-internal:a2c 9)
                           (htmlprag-internal:a2c 10)
                           (htmlprag-internal:a2c 11)
                           (htmlprag-internal:a2c 12)
                           (htmlprag-internal:a2c 13)))

           (output-string->string-or-false
            (lambda (os)
              (let ((s (htmlprag-internal:gosc os)))
                (if (string=? s "") #f s))))

           (output-string->symbol-or-false
            (lambda (os)
              (let ((s (output-string->string-or-false os)))
                (if s (string->symbol s) #f))))
           )
    (lambda (in normalized?)
      ;; TODO: Make a tokenizer option that causes XML namespace qualifiers to
      ;;       be ignored.
      (letrec
          (
           ;; Port buffer with inexpensive unread of one character and slightly
           ;; more expensive pushback of second character to unread.  The
           ;; procedures themselves do no consing.  The tokenizer currently
           ;; needs two-symbol lookahead, due to ambiguous "/" while parsing
           ;; element and attribute names, which could be either empty-tag
           ;; syntax or XML qualified names.
           (c           #f)
           (next-c      #f)
           (c-consumed? #t)
           (read-c      (lambda ()
                          (if c-consumed?
                              (if next-c
                                  (begin (set! c      next-c)
                                         (set! next-c #f))
                                  (set! c (read-char in)))
                              (set! c-consumed? #t))))
           (unread-c    (lambda ()
                          (if c-consumed?
                              (set! c-consumed? #f)
                              ;; TODO: Procedure name in error message really
                              ;;       isn't "make-html-tokenizer"...
                              (htmlprag-internal:error "make-html-tokenizer"
                                                       "already unread"
                                                       c))))
           (push-c      (lambda (new-c)
                          (if c-consumed?
                              (begin (set! c           new-c)
                                     (set! c-consumed? #f))
                              (if next-c
                                  (htmlprag-internal:error
                                   "make-html-tokenizer"
                                   "pushback full"
                                   c)
                                  (begin (set! next-c      c)
                                         (set! c           new-c)
                                         (set! c-consumed? #f))))))

           ;; TODO: These procedures are a temporary convenience for
           ;;       enumerating the pertinent character classes, with an eye
           ;;       towards removing redundant tests of character class.  These
           ;;       procedures should be eliminated in a future version.
           (c-eof?      (lambda () (eof-object? c)))
           (c-amp?      (lambda () (eqv? c #\&)))
           (c-apos?     (lambda () (eqv? c #\')))
           (c-bang?     (lambda () (eqv? c #\!)))
           (c-colon?    (lambda () (eqv? c #\:)))
           (c-quot?     (lambda () (eqv? c #\")))
           (c-equals?   (lambda () (eqv? c #\=)))
           (c-gt?       (lambda () (eqv? c #\>)))
           (c-lt?       (lambda () (eqv? c #\<)))
           (c-minus?    (lambda () (eqv? c #\-)))
           (c-pound?    (lambda () (eqv? c #\#)))
           (c-ques?     (lambda () (eqv? c #\?)))
           (c-semi?     (lambda () (eqv? c #\;)))
           (c-slash?    (lambda () (eqv? c #\/)))
           (c-splat?    (lambda () (eqv? c #\*)))
           (c-lf?       (lambda () (eqv? c #\newline)))
           (c-angle?    (lambda () (memv c '(#\< #\>))))
           (c-ws?       (lambda () (memv c ws-chars)))
           (c-alpha?    (lambda () (char-alphabetic? c)))
           (c-digit?    (lambda () (char-numeric? c)))
           (c-alphanum? (lambda () (or (c-alpha?) (c-digit?))))
           (c-hexlet?   (lambda () (memv c '(#\a #\b #\c #\d #\e #\f
                                             #\A #\B #\C #\D #\E #\F))))

           (skip-ws     (lambda () (read-c) (if (c-ws?) (skip-ws) (unread-c))))

           (make-start-token
            (if normalized?
                (lambda (name ns attrs)
                  (list name (cons htmlprag-internal:at attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list name)
                      (list name (cons htmlprag-internal:at attrs))))))

           (make-empty-token
            (lambda (name ns attrs)
              (cons shtml-empty-symbol
                    (make-start-token name ns attrs))))

           (make-end-token
            (if normalized?
                (lambda (name ns attrs)
                  (list shtml-end-symbol
                        name
                        (cons htmlprag-internal:at attrs)))
                (lambda (name ns attrs)
                  (if (null? attrs)
                      (list shtml-end-symbol name)
                      (list shtml-end-symbol
                            name
                            (cons htmlprag-internal:at attrs))))))

           (make-named-char-token
            (lambda (name-str)
              (list shtml-entity-symbol
                    shtml-named-char-id
                    name-str)))

           (make-numeric-char-token
            (lambda (number)
              (list shtml-entity-symbol
                    shtml-numeric-char-id
                    (number->string number))))

           (make-comment-token
            (lambda (str) (list shtml-comment-symbol str)))

           (make-decl-token
            (lambda (parts) (cons shtml-decl-symbol parts)))

           (scan-qname
            ;; TODO: Make sure we don't accept local names that have "*", since
            ;;       this can break SXML tools.  Have to validate this
            ;;       afterwards if "verbatim-safe?".  Also check for "@" and
            ;;       maybe "@@".  Check qname parsing code, especially for
            ;;       verbatim mode.  This is important!
            (lambda (verbatim-safe?)
              ;; Note: If we accept some invalid local names, we only need two
              ;; symbols of lookahead to determine the end of a qname.
              (letrec ((os      #f)
                       (ns      '())
                       (vcolons 0)
                       (good-os (lambda ()
                                  (or os
                                      (begin (set! os (open-output-string))
                                             os)))))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((or (c-ws?) (c-splat?))
                         (if verbatim-safe?
                             (unread-c)))
                        ((or (c-angle?) (c-equals?) (c-quot?) (c-apos?))
                         (unread-c))
                        ((c-colon?)
                         (or (null? ns)
                             (set! ns (cons ":" ns)))
                         (if os
                             (begin
                               (set! ns (cons (htmlprag-internal:gosc os)
                                              ns))
                               (set! os #f)))
                         (loop))
                        ((c-slash?)
                         (read-c)
                         (cond ((or (c-eof?)
                                    (c-ws?)
                                    (c-equals?)
                                    (c-apos?)
                                    (c-quot?)
                                    (c-angle?)
                                    (c-splat?))
                                (unread-c)
                                (push-c #\/))
                               (else (write-char #\/ (good-os))
                                     (write-char c   os)
                                     (loop))))
                        (else (write-char c (good-os))
                              (loop))))
                (let ((ns    (if (null? ns)
                                 #f
                                 (apply string-append
                                        (htmlprag-internal:reverse!ok ns))))
                      (local (if os (htmlprag-internal:gosc os) #f)))
                  (if verbatim-safe?
                      ;; TODO: Make sure we don't have ambiguous ":" or drop
                      ;;       any characters!
                      (cons ns local)
                      ;; Note: We represent "xmlns:" syntax as normal qnames,
                      ;; for lack of something better to do with them when we
                      ;; don't support XML namespaces.
                      ;;
                      ;; TODO: Local names are currently forced to lowercase,
                      ;;       since HTML is usually case-insensitive.  If XML
                      ;;       namespaces are used, we might wish to keep local
                      ;;       names case-sensitive.
                      (if local
                          (if ns
                              (if (string=? ns "xmlns")
                                  (string->symbol (string-append ns ":" local))
                                  (cons ns
                                        (string->symbol
                                         (htmlprag-internal:down!ok
                                          local))))
                              (string->symbol
                               (htmlprag-internal:down!ok local)))
                          (if ns
                              (string->symbol
                               (htmlprag-internal:down!ok ns))
                              ;; TODO: Ensure that it's OK to return #f as a
                              ;;       name.
                              #f)))))))

           (scan-tag
            (lambda (start?)
              (skip-ws)
              (let ((tag-name   (scan-qname #f))
                    (tag-ns     #f)
                    (tag-attrs  #f)
                    (tag-empty? #f))
                ;; Scan element name.
                (if (pair? tag-name)
                    (begin (set! tag-ns   (car tag-name))
                           (set! tag-name (cdr tag-name))))
                ;; TODO: Ensure there's no case in which a #f tag-name isn't
                ;;       compensated for later.
                ;;
                ;; Scan element attributes.
                (set! tag-attrs
                      (let scan-attr-list ()
                        (read-c)
                        (cond ((c-eof?)   '())
                              ((c-angle?) (unread-c) '())
                              ((c-slash?)
                               (set! tag-empty? #t)
                               (scan-attr-list))
                              ((c-alpha?)
                               (unread-c)
                               (let ((attr (scan-attr)))
                                 (cons attr (scan-attr-list))))
                              (else (scan-attr-list)))))
                ;; Find ">" or unnatural end.
                (let loop ()
                  (read-c)
                  (cond ((c-eof?)   no-token)
                        ((c-slash?) (set! tag-empty? #t) (loop))
                        ((c-gt?)    #f)
                        ((c-ws?)    (loop))
                        (else       (unread-c))))
                ;; Change the tokenizer mode if necessary.
                (cond ((not start?) #f)
                      (tag-empty?   #f)
                      ;; TODO: Maybe make one alist lookup here, instead of
                      ;;       two.
                      ((memq tag-name verbatim-to-eof-elems)
                       (set! nexttok verbeof-nexttok))
                      ((memq tag-name verbatim-pair-elems)
                       (set! nexttok (make-verbpair-nexttok tag-name))))
                ;; Return a token object.
                (if start?
                    (if tag-empty?
                        (make-empty-token tag-name tag-ns tag-attrs)
                        (make-start-token tag-name tag-ns tag-attrs))
                    (make-end-token tag-name tag-ns tag-attrs)))))

           (scan-attr
            (lambda ()
              (let ((name (scan-qname #f))
                    (val  #f))
                (if (pair? name)
                    (set! name (cdr name)))
                (let loop-equals-or-end ()
                  (read-c)
                  (cond ((c-eof?) no-token)
                        ((c-ws?)  (loop-equals-or-end))
                        ((c-equals?)
                         (let loop-quote-or-unquoted ()
                           (read-c)
                           (cond ((c-eof?) no-token)
                                 ((c-ws?) (loop-quote-or-unquoted))
                                 ((or (c-apos?) (c-quot?))
                                  (let ((term c))
                                    (set! val (open-output-string))
                                    (let loop-quoted-val ()
                                      (read-c)
                                      (cond ((c-eof?)      #f)
                                            ((eqv? c term) #f)
                                            (else (write-char c val)
                                                  (loop-quoted-val))))))
                                 ((c-angle?) (unread-c))
                                 (else
                                  (set! val (open-output-string))
                                  (write-char c val)
                                  (let loop-unquoted-val ()
                                    (read-c)
                                    (cond ((c-eof?)  no-token)
                                          ((c-apos?) #f)
                                          ((c-quot?) #f)
                                          ((or (c-ws?) (c-angle?) (c-slash?))
                                           (unread-c))
                                          (else (write-char c val)
                                                (loop-unquoted-val))))))))
                        (else (unread-c))))
                (if normalized?
                    (list name (if val
                                   (htmlprag-internal:gosc val)
                                   (symbol->string name)))
                    (if val
                        (list name (htmlprag-internal:gosc val))
                        (list name))))))

           (scan-comment
            ;; TODO: Rewrite this to use tail recursion rather than a state
            ;;       variable.
            (lambda ()
              (let ((os    (open-output-string))
                    (state 'start-minus))
                (let loop ()
                  (read-c)
                  (cond ((c-eof?) #f)
                        ((c-minus?)
                         (set! state
                               (case state
                                 ((start-minus) 'start-minus-minus)
                                 ((start-minus-minus body) 'end-minus)
                                 ((end-minus) 'end-minus-minus)
                                 ((end-minus-minus)
                                  (write-char #\- os)
                                  state)
                                 (else (htmlprag-internal:error
                                        "make-html-tokenizer"
                                        "invalid state"
                                        state))))
                         (loop))
                        ((and (c-gt?) (eq? state 'end-minus-minus)) #f)
                        (else (case state
                                ((end-minus)       (write-char #\- os))
                                ((end-minus-minus) (display "--" os)))
                              (set! state 'body)
                              (write-char c os)
                              (loop))))
                (make-comment-token (htmlprag-internal:gosc os)))))

           (scan-pi
            (lambda ()
              (skip-ws)
              (let ((name (open-output-string))
                    (val  (open-output-string)))
                (let scan-name ()
                  (read-c)
                  (cond ((c-eof?)   #f)
                        ((c-ws?)    #f)
                        ((c-alpha?) (write-char c name) (scan-name))
                        (else       (unread-c))))
                ;; TODO: Do we really want to emit #f for PI name?
                (set! name (output-string->symbol-or-false name))
                (let scan-val ()
                  (read-c)
                  (cond ((c-eof?)  #f)
                        ;; ((c-amp?) (display (scan-entity) val)
                        ;;           (scan-val))
                        ((c-ques?)
                         (read-c)
                         (cond ((c-eof?) (write-char #\? val))
                               ((c-gt?)  #f)
                               (else     (write-char #\? val)
                                         (unread-c)
                                         (scan-val))))
                        (else (write-char c val) (scan-val))))
                (list shtml-pi-symbol
                      name
                      (htmlprag-internal:gosc val)))))

           (scan-decl
            ;; TODO: Find if SXML includes declaration forms, and if so,
            ;;       use whatever format SXML wants.
            ;;
            ;; TODO: Rewrite to eliminate state variables.
            (letrec
                ((scan-parts
                  (lambda ()
                    (let ((part       (open-output-string))
                          (nonsymbol? #f)
                          (state      'before)
                          (last?      #f))
                      (let loop ()
                        (read-c)
                        (cond ((c-eof?) #f)
                              ((c-ws?)
                               (case state
                                 ((before) (loop))
                                 ((quoted) (write-char c part) (loop))))
                              ((and (c-gt?) (not (eq? state 'quoted)))
                               (set! last? #t))
                              ((and (c-lt?) (not (eq? state 'quoted)))
                               (unread-c))
                              ((c-quot?)
                               (case state
                                 ((before)   (set! state 'quoted) (loop))
                                 ((unquoted) (unread-c))
                                 ((quoted)   #f)))
                              (else
                               (if (eq? state 'before)
                                   (set! state 'unquoted))
                               (set! nonsymbol? (or nonsymbol?
                                                    (not (c-alphanum?))))
                               (write-char c part)
                               (loop))))
                      (set! part (htmlprag-internal:gosc part))
                      (if (string=? part "")
                          '()
                          (cons (if (or (eq? state 'quoted) nonsymbol?)
                                    part
                                    ;; TODO: Normalize case of things we make
                                    ;;       into symbols here.
                                    (string->symbol part))
                                (if last?
                                    '()
                                    (scan-parts))))))))
              (lambda () (make-decl-token (scan-parts)))))

           (scan-entity
            (lambda ()
              (read-c)
              (cond ((c-eof?) "&")
                    ((c-alpha?)
                     ;; TODO: Do entity names have a maximum length?
                     (let ((name (open-output-string)))
                       (write-char c name)
                       (let loop ()
                         (read-c)
                         (cond ((c-eof?)   #f)
                               ((c-alpha?) (write-char c name) (loop))
                               ((c-semi?)  #f)
                               (else       (unread-c))))
                       (set! name (htmlprag-internal:gosc name))
                       ;; TODO: Make the entity map an option.
                       (let ((pair (assoc name '(("amp"  . "&")
                                                 ("apos" . "'")
                                                 ("gt"   . ">")
                                                 ("lt"   . "<")
                                                 ("quot" . "\"")))))
                         (if pair
                             (cdr pair)
                             (make-named-char-token name)))))
                    ((c-pound?)
                     (let ((num  (open-output-string))
                           (hex? #f))
                       (read-c)
                       (cond ((c-eof?)            #f)
                             ((memv c '(#\x #\X)) (set! hex? #t) (read-c)))
                       (let loop ()
                         (cond ((c-eof?)  #f)
                               ((c-semi?) #f)
                               ((or (c-digit?) (and hex? (c-hexlet?)))
                                (write-char c num)
                                (read-c)
                                (loop))
                               (else (unread-c))))
                       (set! num (htmlprag-internal:gosc num))
                       (if (string=? num "")
                           "&#;"
                           (let ((n (string->number num (if hex? 16 10))))
                             (if (and (<= 32 n 255) (not (= n 127)))
                                 (string (htmlprag-internal:a2c n))
                                 (make-numeric-char-token n))))))
                    (else (unread-c) "&"))))

           (normal-nexttok
            (lambda ()
              (read-c)
              (cond ((c-eof?) no-token)
                    ((c-lt?)
                     (let loop ()
                       (read-c)
                       (cond ((c-eof?)   "<")
                             ((c-ws?)    (loop))
                             ((c-slash?) (scan-tag #f))
                             ((c-ques?)  (scan-pi))
                             ((c-bang?)  (let loop ()
                                           (read-c)
                                           (cond ((c-eof?)   no-token)
                                                 ((c-ws?)    (loop))
                                                 ((c-minus?) (scan-comment))
                                                 (else       (unread-c)
                                                             (scan-decl)))))
                             ((c-alpha?) (unread-c) (scan-tag #t))
                             (else       (unread-c) "<"))))
                    ((c-gt?) ">")
                    (else (let ((os (open-output-string)))
                            (let loop ()
                              (cond ((c-eof?)   #f)
                                    ((c-angle?) (unread-c))
                                    ((c-amp?)
                                     (let ((entity (scan-entity)))
                                       (if (string? entity)
                                           (begin (display entity os)
                                                  (read-c)
                                                  (loop))
                                           (let ((saved-nexttok nexttok))
                                             (set! nexttok
                                                   (lambda ()
                                                     (set! nexttok
                                                           saved-nexttok)
                                                     entity))))))
                                    (else (write-char c os)
                                          (or (c-lf?)
                                              (begin (read-c) (loop))))))
                            (let ((text (htmlprag-internal:gosc os)))
                              (if (equal? text "")
                                  (nexttok)
                                  text)))))))

           (verbeof-nexttok
            (lambda ()
              (read-c)
              (if (c-eof?)
                  no-token
                  (let ((os (open-output-string)))
                    (let loop ()
                      (or (c-eof?)
                          (begin (write-char c os)
                                 (or (c-lf?)
                                     (begin (read-c) (loop))))))
                    (htmlprag-internal:gosc os)))))

           (make-verbpair-nexttok
            (lambda (elem-name)
              (lambda ()
                (let ((os (open-output-string)))
                  ;; Accumulate up to a newline-terminated line.
                  (let loop ()
                    (read-c)
                    (cond ((c-eof?)
                           ;; Got EOF in verbatim context, so set the normal
                           ;; nextok procedure, then fall out of loop.
                           (set! nexttok normal-nexttok))
                          ((c-lt?)
                           ;; Got "<" in verbatim context, so get next
                           ;; character.
                           (read-c)
                           (cond ((c-eof?)
                                  ;; Got "<" then EOF, so set to the normal
                                  ;; nexttok procedure, add the "<" to the
                                  ;; verbatim string, and fall out of loop.
                                  (set! nexttok normal-nexttok)
                                  (write-char #\< os))
                                 ((c-slash?)
                                  ;; Got "</", so...
                                  (read-c)
                                  (cond
                                   ((c-eof?)
                                    (display "</" os))
                                   ((c-alpha?)
                                    ;; Got "</" followed by alpha, so unread
                                    ;; the alpha, scan qname, compare...
                                    (unread-c)
                                    (let* ((vqname (scan-qname #t))
                                           (ns     (car vqname))
                                           (local  (cdr vqname)))
                                      ;; Note: We ignore XML namespace
                                      ;; qualifier for purposes of comparison.
                                      ;;
                                      ;; Note: We're interning strings here for
                                      ;; comparison when in theory there could
                                      ;; be many such unique interned strings
                                      ;; in a valid HTML document, although in
                                      ;; practice this should not be a problem.
                                      (if (and local
                                               (eqv? (string->symbol
                                                      (htmlprag-internal:down
                                                       local))
                                                     elem-name))
                                          ;; This is the terminator tag, so
                                          ;; scan to the end of it, set the
                                          ;; nexttok, and fall out of the loop.
                                          (begin
                                            (let scan-to-end ()
                                              (read-c)
                                              (cond ((c-eof?) #f)
                                                    ((c-gt?)  #f)
                                                    ((c-lt?)  (unread-c))
                                                    ((c-alpha?)
                                                     (unread-c)
                                                     ;; Note: This is an
                                                     ;; expensive way to skip
                                                     ;; over an attribute, but
                                                     ;; in practice more
                                                     ;; verbatim end tags will
                                                     ;; not have attributes.
                                                     (scan-attr)
                                                     (scan-to-end))
                                                    (else (scan-to-end))))
                                            (set! nexttok
                                                  (lambda ()
                                                    (set! nexttok
                                                          normal-nexttok)
                                                    (make-end-token
                                                     elem-name #f '()))))
                                          ;; This isn't the terminator tag, so
                                          ;; add to the verbatim string the
                                          ;; "</" and the characters of what we
                                          ;; were scanning as a qname, and
                                          ;; recurse in the loop.
                                          (begin
                                            (display "</" os)
                                            (if ns
                                                (begin (display ns os)
                                                       (display ":" os)))
                                            (if local
                                                (display local os))
                                            (loop)))))
                                   (else
                                    ;; Got "</" and non-alpha, so unread new
                                    ;; character, add the "</" to verbatim
                                    ;; string, then loop.
                                    (unread-c)
                                    (display "</" os)
                                    (loop))))
                                 (else
                                  ;; Got "<" and non-slash, so unread the new
                                  ;; character, write the "<" to the verbatim
                                  ;; string, then loop.
                                  (unread-c)
                                  (write-char #\< os)
                                  (loop))))
                          (else
                           ;; Got non-"<" in verbatim context, so just add it
                           ;; to the buffer, then, if it's not a linefeed, fall
                           ;; out of the loop so that the token can be
                           ;; returned.
                           (write-char c os)
                           (or (c-lf?) (loop)))))
                  ;; Return the accumulated line string, if non-null, or call
                  ;; nexttok.
                  (or (output-string->string-or-false os) (nexttok))))))

           (nexttok #f))

        (set! nexttok normal-nexttok)
        (lambda () (nexttok))))))

;;; @defproc tokenize-html in normalized?
;;;
;;; Returns a list of tokens from input port @var{in}, normalizing according to
;;; boolean @var{normalized?}.  This is probably most useful as a debugging
;;; convenience.  For example:
;;;
;;; @lisp
;;; (tokenize-html (open-input-string "<a href=\"foo\">bar</a>") #f)
;;; @result{} ((a (@@ (href "foo"))) "bar" (*END* a))
;;; @end lisp

(define (tokenize-html in normalized?)
  (let ((next-tok (make-html-tokenizer in normalized?)))
    (let loop ((tok (next-tok)))
      (if (null? tok)
          '()
          (cons tok (loop (next-tok)))))))

;;; @defproc shtml-token-kind token
;;;
;;; Returns a symbol indicating the kind of tokenizer @var{token}:
;;; @code{*COMMENT*}, @code{*DECL*}, @code{*EMPTY*}, @code{*END*},
;;; @code{*ENTITY*}, @code{*PI*}, @code{*START*}, @code{*TEXT*}.
;;; This is used by higher-level parsing code.  For example:
;;;
;;; @lisp
;;; (map shtml-token-kind
;;;      (tokenize-html (open-input-string "<a<b>><c</</c") #f))
;;; @result{} (*START* *START* *TEXT* *START* *END* *END*)
;;; @end lisp

(define (shtml-token-kind token)
  (cond ((string? token) shtml-text-symbol)
        ((list?   token)
         (let ((s (list-ref token 0)))
           (if (memq s `(,shtml-comment-symbol
                         ,shtml-decl-symbol
                         ,shtml-empty-symbol
                         ,shtml-end-symbol
                         ,shtml-entity-symbol
                         ,shtml-pi-symbol))
               s
               shtml-start-symbol)))
        (else (htmlprag-internal:error "shtml-token-kind"
                                       "unrecognized token kind"
                                       token))))

;;; @section Parsing

;;; Most applications will call a parser procedure such as
;;; @code{html->shtml} rather than calling the tokenizer directly.

;; @defvar htmlprag-internal:empty-elements
;;
;; List of names of HTML element types that have no content, represented as a
;; list of symbols.  This is used internally by the parser and encoder.  The
;; effect of mutating this list is undefined.

;; TODO: Document exactly which elements these are, after we make the new
;;       parameterized parser constructor.

(define htmlprag-internal:empty-elements
  '(area base br frame hr img input isindex keygen link meta object param
         spacer wbr))

;;; @defproc parse-html/tokenizer tokenizer normalized?
;;;
;;; Emits a parse tree like @code{html->shtml} and related procedures, except
;;; using @var{tokenizer} as a source of tokens, rather than tokenizing from an
;;; input port.  This procedure is used internally, and generally should not be
;;; called directly.

(define parse-html/tokenizer
  ;; TODO: Document the algorithm, then see if rewriting as idiomatic Scheme
  ;;       can make it more clear.
  (letrec ((empty-elements
            ;; TODO: Maybe make this an option.  This might also be an
            ;;       acceptable way to parse old HTML that uses the `p' element
            ;;       as a paragraph terminator.
            htmlprag-internal:empty-elements)
           (parent-constraints
            ;; TODO: Maybe make this an option.
            '((area     . (map))
              (body     . (html))
              (caption  . (table))
              (colgroup . (table))
              (dd       . (dl))
              (dt       . (dl))
              (frame    . (frameset))
              (head     . (html))
              (isindex  . (head))
              (li       . (dir menu ol ul))
              (meta     . (head))
              (noframes . (frameset))
              (option   . (select))
              (p        . (body td th))
              (param    . (applet))
              (tbody    . (table))
              (td       . (tr))
              (th       . (tr))
              (thead    . (table))
              (title    . (head))
              (tr       . (table tbody thead))))
           (start-tag-name (lambda (tag-token) (car tag-token)))
           (end-tag-name   (lambda (tag-token) (list-ref tag-token 1))))
    (lambda (tokenizer normalized?)
      ;; Example `begs' value:
      ;;
      ;; ( ((head ...) . ( (title ...)                         ))
      ;;   ((html ...) . ( (head  ...) (*COMMENT* ...)         ))
      ;;   (#f         . ( (html  ...) (*DECL*    doctype ...) )) )
      (let ((begs (list (cons #f '()))))
        (letrec ((add-to-current-beg
                  (lambda (tok)
                    (set-cdr! (car begs) (cons tok (cdr (car begs))))))
                 (finish-all-begs
                  (lambda ()
                    (let ((toplist #f))
                      (map (lambda (beg) (set! toplist (finish-beg beg)))
                           begs)
                      toplist)))
                 (finish-beg
                  (lambda (beg)
                    (let ((start-tok (car beg)))
                      (if start-tok
                          (htmlprag-internal:append!
                           (car beg)
                           (htmlprag-internal:reverse!ok (cdr beg)))
                          (htmlprag-internal:reverse!ok (cdr beg))))))
                 (finish-begs-to
                  (lambda (name lst)
                    (let* ((top      (car lst))
                           (starttag (car top)))
                      (cond ((not starttag) #f)
                            ((eqv? name (start-tag-name starttag))
                             (set! begs (cdr lst))
                             (finish-beg top)
                             #t)
                            (else (if (finish-begs-to name (cdr lst))
                                      (begin (finish-beg top) #t)
                                      #f))))))
                 (finish-begs-upto
                  (lambda (parents lst)
                    (let* ((top      (car lst))
                           (starttag (car top)))
                      (cond ((not starttag) #f)
                            ((memq (start-tag-name starttag) parents)
                             (set! begs lst)
                             #t)
                            (else (if (finish-begs-upto parents (cdr lst))
                                      (begin (finish-beg top) #t)
                                      #f)))))))
          (let loop ()
            (let ((tok (tokenizer)))
              (if (null? tok)
                  (finish-all-begs)
                  (let ((kind (shtml-token-kind tok)))
                    (cond ((memv kind `(,shtml-comment-symbol
                                        ,shtml-decl-symbol
                                        ,shtml-entity-symbol
                                        ,shtml-pi-symbol
                                        ,shtml-text-symbol))
                           (add-to-current-beg tok))
                          ((eqv? kind shtml-start-symbol)
                           (let* ((name (start-tag-name tok))
                                  (cell (assq name parent-constraints)))
                             (and cell (finish-begs-upto (cdr cell) begs))
                             (add-to-current-beg tok)
                             (or (memq name empty-elements)
                                 (set! begs (cons (cons tok '()) begs)))))
                          ((eqv? kind shtml-empty-symbol)
                           ;; Empty tag token, so just add it to current
                           ;; beginning while stripping off leading `*EMPTY*'
                           ;; symbol so that the token becomes normal SXML
                           ;; element syntax.
                           (add-to-current-beg (cdr tok)))
                          ((eqv? kind shtml-end-symbol)
                           (let ((name (end-tag-name tok)))
                             (if name
                                 ;; Try to finish to a start tag matching this
                                 ;; end tag.  If none, just drop the token,
                                 ;; though we used to add it to the current
                                 ;; beginning.
                                 (finish-begs-to name begs)
                                 ;; We have an anonymous end tag, so match it
                                 ;; with the most recent beginning.  If no
                                 ;; beginning to match, then just drop the
                                 ;; token, though we used to add it to the
                                 ;; current beginning.
                                 (and (car (car begs))
                                      (begin (finish-beg (car begs))
                                             (set! begs (cdr begs)))))))
                          (else (htmlprag-internal:error "parse-html/tokenizer"
                                                         "unknown tag kind"
                                                         kind)))
                    (loop))))))))))

;; @defproc htmlprag-internal:parse-html input normalized? top?
;;
;; This procedure is now used internally by @code{html->shtml} and its
;; variants, and should not be used directly by programs.  The interface is
;; likely to change in future versions of HtmlPrag.

(define (htmlprag-internal:parse-html input normalized? top?)
  (let ((parse
         (lambda ()
           (parse-html/tokenizer
            (make-html-tokenizer
             (cond ((input-port? input) input)
                   ((string?     input) (open-input-string input))
                   (else (htmlprag-internal:error
                          "htmlprag-internal:parse-html"
                          "invalid input type"
                          input)))
             normalized?)
            normalized?))))
    (if top?
        (cons shtml-top-symbol (parse))
        (parse))))

;;; @defproc  html->sxml-0nf input
;;; @defprocx html->sxml-1nf input
;;; @defprocx html->sxml-2nf input
;;; @defprocx html->sxml     input
;;;
;;; Permissively parse HTML from @var{input}, which is either an input port or
;;; a string, and emit an SHTML equivalent or approximation.  To borrow and
;;; slightly modify an example from [SSAX-HTML]:
;;;
;;; @lisp
;;; (html->shtml
;;;  "<html><head><title></title><title>whatever</title></head><body>
;;; <a href=\"url\">link</a><p align=center><ul compact style=\"aa\">
;;; <p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened</i>
;;; still &lt; bold </b></body><P> But not done yet...")
;;; @result{}
;;; (*TOP* (html (head (title) (title "whatever"))
;;;              (body "\n"
;;;                    (a (@@ (href "url")) "link")
;;;                    (p (@@ (align "center"))
;;;                       (ul (@@ (compact) (style "aa")) "\n"))
;;;                    (p "BLah"
;;;                       (*COMMENT* " comment <comment> ")
;;;                       " "
;;;                       (i " italic " (b " bold " (tt " ened")))
;;;                       "\n"
;;;                       "still < bold "))
;;;              (p " But not done yet...")))
;;; @end lisp
;;;
;;; Note that in the emitted SHTML the text token @code{"still < bold"} is
;;; @emph{not} inside the @code{b} element, which represents an unfortunate
;;; failure to emulate all the quirks-handling behavior of some popular Web
;;; browsers.
;;;
;;; The procedures @code{html->sxml-@var{n}nf} for @var{n} 0 through 2
;;; correspond to 0th through 2nd normal forms of SXML as specified in [SXML],
;;; and indicate the minimal requirements of the emitted SXML.
;;;
;;; @code{html->sxml} and @code{html->shtml} are currently aliases for
;;; @code{html->sxml-0nf}, and can be used in scripts and interactively, when
;;; terseness is important and any normal form of SXML would suffice.

(define (html->sxml-0nf input) (htmlprag-internal:parse-html input #f #t))
(define (html->sxml-1nf input) (htmlprag-internal:parse-html input #f #t))
(define (html->sxml-2nf input) (htmlprag-internal:parse-html input #t #t))

(define html->sxml  html->sxml-0nf)
(define html->shtml html->sxml-0nf)

;;; @section HTML Encoding

;;; Two procedures encode the SHTML representation as conventional HTML,
;;; @code{write-shtml-as-html} and @code{shtml->html}.  These are perhaps most
;;; useful for emitting the result of parsed and transformed input HTML.  They
;;; can also be used for emitting HTML from generated or handwritten SHTML.

;;; @defproc write-shtml-as-html shtml out
;;;
;;; Writes a conventional HTML transliteration of the SHTML @var{shtml} to
;;; output port @var{out}.  HTML elements of types that are always empty are
;;; written using HTML4-compatible XHTML tag syntax.  No inter-tag whitespace
;;; or line breaks not explicit in @var{shtml} is emitted.  The @var{shtml}
;;; should normally include a newline at the end of the document.  For example
;;; (which might not work verbatim in all Scheme implementations):
;;;
;;; @lisp
;;; (write-shtml-as-html
;;;  '((html (head (title "My Title"))
;;;          (body (@@ (bgcolor "white"))
;;;                (h1 "My Heading")
;;;                (p "This is a paragraph.")
;;;                (p "This is another paragraph."))))
;;;  (current-output-port))
;;; @print{} <html><head><title>My Title</title></head><body bgcolor="whi
;;; @print{} te"><h1>My Heading</h1><p>This is a paragraph.</p><p>This is
;;; @print{}  another paragraph.</p></body></html>
;;; @end lisp

(define (write-shtml-as-html shtml out)
  (letrec
      ((write-shtml-text
        (lambda (str out)
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (case c
                                      ;; ((#\") "&quot;")
                                      ((#\&) "&amp;")
                                      ((#\<) "&lt;")
                                      ((#\>) "&gt;")
                                      (else c)))
                                  out)
                         (loop (+ 1 i))))))))
       (write-dquote-ampified
        (lambda (str out)
          ;; TODO: If we emit "&quot;", we really should parse it, and HTML
          ;;       4.01 says we should, but anachronisms in HTML create the
          ;;       potential for nasty mutilation of URI in attribute values.
          (let ((len (string-length str)))
            (let loop ((i 0))
              (if (< i len)
                  (begin (display (let ((c (string-ref str i)))
                                    (if (eqv? c #\") "&quot;" c))
                                  out)
                         (loop (+ 1 i))))))))
       (do-thing
        (lambda (thing)
          (cond ((string? thing) (write-shtml-text thing out))
                ((list? thing)   (if (not (null? thing))
                                     (do-list-thing thing)))
                (else            (htmlprag-internal:error "write-shtml-as-html"
                                                          "invalid SHTML thing"
                                                          thing)))))
       (do-list-thing
        (lambda (thing)
          (let ((head (car thing)))
            (cond ((symbol? head)
                   ;; Head is a symbol, so...
                   (cond ((eq? head shtml-comment-symbol)
                          ;; TODO: Make sure the comment text doesn't contain a
                          ;;       comment end sequence.
                          (display "<!-- " out)
                          (let ((text (car (cdr thing))))
                            (if (string? text)
                                ;; TODO: Enforce whitespace safety without
                                ;;       padding unnecessarily.
                                ;;
                                ;; (let ((len (string-length text)))
                                ;; (if (= len 0)
                                ;; (display #\space out)
                                ;; (begin (if (not (eqv?
                                ;; (string-ref text 0)
                                ;; #\space))
                                (display text out)
                                (htmlprag-internal:error
                                 "write-shtml-as-html"
                                 "invalid SHTML comment text"
                                 thing)))
                          (or (null? (cdr (cdr thing)))
                              (htmlprag-internal:error
                               "write-shtml-as-html"
                               "invalid SHTML comment body"
                               thing))
                          (display " -->" out))
                         ((eq? head shtml-decl-symbol)
                          (let ((head (car (cdr thing))))
                            (display "<!" out)
                            (display (symbol->string head) out)
                            (for-each
                             (lambda (n)
                               (cond ((symbol? n)
                                      (display #\space out)
                                      (display (symbol->string n) out))
                                     ((string? n)
                                      (display " \"" out)
                                      (write-dquote-ampified n out)
                                      (display #\" out))
                                     (else (htmlprag-internal:error
                                            "write-shtml-as-html"
                                            "invalid SHTML decl"
                                            thing))))
                             (cdr (cdr thing)))
                            (display #\> out)))
                         ((eq? head shtml-entity-symbol)
                          (let ((val (shtml-entity-value thing)))
                            (display #\& out)
                            (if (integer? val)
                                (display #\# out))
                            (display val out))
                          (display #\; out))
                         ((eq? head shtml-pi-symbol)
                          (display "<?" out)
                          (display (symbol->string (car (cdr thing))) out)
                          (display #\space out)
                          (display (car (cdr (cdr thing))) out)
                          ;; TODO: Error-check that no more rest of PI.
                          (display "?>" out))
                         ((eq? head shtml-top-symbol)
                          (for-each do-thing (cdr thing)))
                         ((eq? head shtml-empty-symbol)
                          #f)
                         ((memq head `(,shtml-end-symbol
                                       ,shtml-start-symbol
                                       ,shtml-text-symbol))
                          (htmlprag-internal:error "write-shtml-as-html"
                                                   "invalid SHTML symbol"
                                                   head))
                         ((eq? head htmlprag-internal:at)
                          (htmlprag-internal:error
                           "write-shtml-as-html"
                           "illegal position of SHTML attributes"
                           thing))
                         (else
                          (display #\< out)
                          (display head out)
                          (let* ((rest   (cdr thing)))
                            (if (not (null? rest))
                                (let ((second (car rest)))
                                  (and (list? second)
                                       (not (null? second))
                                       (eq? (car second) htmlprag-internal:at)
                                       (begin (for-each do-attr (cdr second))
                                              (set! rest (cdr rest))))))
                            (if (memq head
                                      htmlprag-internal:empty-elements)
                                ;; TODO: Error-check to make sure the element
                                ;;       has no content other than attributes.
                                ;;       We have to test for cases like:
                                ;;       (br (@) () (()))
                                (display " />" out)
                                (begin (display #\> out)
                                       (for-each do-thing rest)
                                       (display "</" out)
                                       (display (symbol->string head) out)
                                       (display #\> out)))))))
                  ((or (list? head) (string? head))
                   ;; Head is a list or string, which might occur as the result
                   ;; of an SXML transform, so we'll cope.
                   (for-each do-thing thing))
                  (else
                   ;; Head is NOT a symbol, list, or string, so error.
                   (htmlprag-internal:error "write-shtml-as-html"
                                            "invalid SHTML list"
                                            thing))))))
       (write-attr-val-dquoted
        (lambda (str out)
          (display #\" out)
          (display str out)
          (display #\" out)))
       (write-attr-val-squoted
        (lambda (str out)
          (display #\' out)
          (display str out)
          (display #\' out)))
       (write-attr-val-dquoted-and-amped
        (lambda (str out)
          (display #\" out)
          (write-dquote-ampified str out)
          (display #\" out)))
       (write-attr-val
        (lambda (str out)
          (let ((len (string-length str)))
            (let find-dquote-and-squote ((i 0))
              (if (= i len)
                  (write-attr-val-dquoted str out)
                  (let ((c (string-ref str i)))
                    (cond ((eqv? c #\")
                           (let find-squote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-squoted str out)
                                 (if (eqv? (string-ref str i) #\')
                                     (write-attr-val-dquoted-and-amped str out)
                                     (find-squote (+ 1 i))))))
                          ((eqv? c #\')
                           (let find-dquote ((i (+ 1 i)))
                             (if (= i len)
                                 (write-attr-val-dquoted str out)
                                 (if (eqv? (string-ref str i) #\")
                                     (write-attr-val-dquoted-and-amped str out)
                                     (find-dquote (+ 1 i))))))
                          (else (find-dquote-and-squote (+ 1 i))))))))))
       (do-attr
        (lambda (attr)
          (or (list? attr)
              (htmlprag-internal:error "write-shtml-as-html"
                                       "invalid SHTML attribute"
                                       attr))
          (if (not (null? attr))
              (let ((name (car attr)))
                (or (symbol? name)
                    (htmlprag-internal:error "write-shtml-as-html"
                                             "invalid name in SHTML attribute"
                                             attr))
                (if (not (eq? name htmlprag-internal:at))
                    (begin
                      (display #\space out)
                      (display name    out)
                      (let ((rest (cdr attr)))
                        (or (list? rest)
                            (htmlprag-internal:error
                             "write-shtml-as-html"
                             "malformed SHTML attribute"
                             attr))
                        (if (not (null? rest))
                            (let ((value (car rest)))
                              (cond ((string? value)
                                     (display #\= out)
                                     (write-attr-val value out))
                                    ((eq? value #t)
                                     ;; Note: This is not valid SXML, but
                                     ;; perhaps should be.
                                     #f)
                                    (else
                                     (htmlprag-internal:error
                                      "write-shtml-as-html"
                                      "invalid value in SHTML attribute"
                                      attr)))))))))))))
    (do-thing shtml)
    (if #f #f)))

;;; @defproc shtml->html shtml
;;;
;;; Yields an HTML encoding of SHTML @var{shtml} as a string.  For example:
;;;
;;; @lisp
;;; (shtml->html
;;;  (html->shtml
;;;   "<P>This is<br<b<I>bold </foo>italic</ b > text.</p>"))
;;; @result{} "<p>This is<br /><b><i>bold italic</i></b> text.</p>"
;;; @end lisp
;;;
;;; Note that, since this procedure constructs a string, it should normally
;;; only be used when the HTML is relatively small.  When encoding HTML
;;; documents of conventional size and larger, @var{write-shtml-as-html} is
;;; much more efficient.

(define (shtml->html shtml)
  (let ((os (open-output-string)))
    (write-shtml-as-html shtml os)
    (htmlprag-internal:gosc os)))

;;; @section Deprecated

;;; As HtmlPrag evolves towards version 1.0,

;;; The equivalences below show the deprecated expressions below, the code on
;;; the left is deprecated and should be replaced with the code on the right.

;;; @lisp
;;; sxml->html       @equiv{}  shtml->html
;;; write-sxml-html  @equiv{}  write-shtml-as-html
;;; @end lisp

(define sxml->html      shtml->html)
(define write-sxml-html write-shtml-as-html)

;;; @section Tests

;;; A regression test suite is defined as procedure @code{test-htmlprag} in the
;;; source file.  The test suite can be run under various Scheme
;;; implementations with Unix shell commands like:
;;;
;;; @itemize @
;;;
;;; @item Bigloo
;;; @example
;;; bigloo -eval '(load "htmlprag.scm") (test-htmlprag) (exit)'
;;; @end example
;;;
;;; @item Chicken
;;; @example
;;; csi -batch -eval '(load "htmlprag.scm") (test-htmlprag)'
;;; @end example
;;;
;;; @item Gauche
;;; @example
;;; gosh -l./htmlprag.scm -e"(begin (test-htmlprag) (exit))"
;;; @end example
;;;
;;; @item Guile
;;; @example
;;; guile -l htmlprag.scm -c "(test-htmlprag)"
;;; @end example
;;;
;;; @c @item Kawa
;;; @c @example
;;; @c kawa -f htmlprag.scm -e "(test-htmlprag)"
;;; @c @end example
;;;
;;; @item MIT Scheme
;;; @example
;;; mit-scheme <<EOH
;;; (define open-input-string  string->input-port)
;;; (define open-output-string make-accumulator-output-port)
;;; (define get-output-string  get-output-from-accumulator)
;;; (load "htmlprag.scm") (test-htmlprag)
;;; EOH
;;; @end example
;;;
;;; @item PLT MzScheme
;;; @example
;;; mzscheme -qfe htmlprag.scm "(begin (test-htmlprag) (exit))"
;;; @end example
;;;
;;; @item RScheme
;;; @example
;;; rs -e '(load "htmlprag.scm") (test-htmlprag)' -exit
;;; @end example
;;;
;;; @item Scheme 48 @ (requires edit of @code{htmlprag-internal:a2c})
;;; @example
;;; scheme48 <<EOH
;;; ,open ascii
;;; ,open srfi-6
;;; ,open srfi-23
;;; (load "htmlprag.scm") (test-htmlprag)
;;; ,exit
;;; EOH
;;; @end example
;;;
;;; @item Scsh @ (requires edit of @code{htmlprag-internal:a2c})
;;; @example
;;; scsh -o srfi-6 -o srfi-23 -l htmlprag.scm -c "(test-htmlprag)"
;;; @end example
;;;
;;; @item SISC
;;; @example
;;; echo '(begin (load "htmlprag.scm") (test-htmlprag))' \
;;; | JAVAOPT="-ss32m" sisc
;;; @end example
;;;
;;; @item STklos
;;; @example
;;; echo '(load "htmlprag.scm") (test-htmlprag)' | stklos
;;; @end example
;;;
;;; @c @item SXM
;;; @c @example
;;; @c echo '(load "htmlprag.scm") (test-htmlprag)' | sxi
;;; @c @end example
;;;
;;; @end itemize
;;;
;;; A human-readable log of each test case execution will be written to the
;;; default output.  If all test cases pass, then the last line of the log will
;;; be:
;;;
;;; @example
;;; *** All Tests PASSED ***   Passed: 125   Failed: 0
;;; @end example

;;; @defproc test-htmlprag
;;;
;;; Run the test suite.  A log will be printed to the default output port.
;;; Returns true iff all tests pass.

(define (test-htmlprag)
  (letrec ((passed      0)
           (failed      0)
           (tests-begin (lambda ()
                          (newline)
                          (display "*** HtmlPrag Tests ***")
                          (newline)))
           (tests-end   (lambda ()
                          (newline)
                          (display "*** ")
                          (display (if (= 0 failed)
                                       "All Tests PASSED"
                                       "Some Tests FAILED"))
                          (display " ***   Passed: ")
                          (write passed)
                          (display "   Failed: ")
                          (write failed)
                          (display "   ")
                          (newline)
                          (= 0 failed)))
           (test (lambda (proc proc-sym args expected)
                   (newline)
                   (display #\()
                   (write proc-sym)
                   (for-each (lambda (arg)
                               (display #\space)
                               (if (or (symbol? arg) (list? arg) (vector? arg))
                                   (display #\'))
                               (write arg))
                             args)
                   (display #\))
                   (newline)
                   (let ((result (apply proc args)))
                     (display "==> ")
                     (write result)
                     (newline)
                     (if (equal? result expected)
                         (begin (set! passed (+ 1 passed))
                                (display ";; Passed.")
                                (newline))
                         (begin (set! failed (+ 1 failed))
                                (display ";; ***FAILED*** Expected:")
                                (newline)
                                (display ";;  ")
                                (write expected)
                                (newline))))))
           (t1 (lambda (input expected)
                 (test html->shtml
                       'html->shtml
                       (list input)
                       (cons shtml-top-symbol expected))))
           (t2 (lambda (input expected)
                 (test shtml->html
                       'shtml->html
                       (list input)
                       expected)))
           (at      htmlprag-internal:at)
           (comment shtml-comment-symbol)
           (decl    shtml-decl-symbol)
           (entity  shtml-entity-symbol)
           (pi      shtml-pi-symbol)
           (lf      (string (htmlprag-internal:a2c 10))))
    (tests-begin)

    (t1 "<a>>" '((a ">")))
    (t1 "<a<>" '((a "<" ">")))

    (t1 "<>" '("<" ">"))
    (t1 "< >" '("<" ">"))
    (t1 "< a>" '((a)))
    (t1 "< a / >" '((a)))

    (t1 "<a<" '((a "<")))
    (t1 "<a<b" '((a (b))))

    (t1 "><a>" '(">" (a)))

    (t1 "</>" '())

    (t1 "<\">" '("<" "\"" ">"))

    (t1 (string-append "<a>xxx<plaintext>aaa" lf "bbb" lf "c<c<c")
        `((a "xxx" (plaintext ,(string-append "aaa" lf)
                              ,(string-append "bbb" lf)
                              "c<c<c"))))

    (t1 "aaa<!-- xxx -->bbb"   `("aaa" (,comment " xxx ")   "bbb"))
    (t1 "aaa<! -- xxx -->bbb"  `("aaa" (,comment " xxx ")   "bbb"))
    (t1 "aaa<!-- xxx --->bbb"  `("aaa" (,comment " xxx -")  "bbb"))
    (t1 "aaa<!-- xxx ---->bbb" `("aaa" (,comment " xxx --") "bbb"))
    (t1 "aaa<!-- xxx -y-->bbb" `("aaa" (,comment " xxx -y") "bbb"))
    (t1 "aaa<!----->bbb"       `("aaa" (,comment "-")       "bbb"))
    (t1 "aaa<!---->bbb"        `("aaa" (,comment "")        "bbb"))
    (t1 "aaa<!--->bbb"         `("aaa" (,comment "->bbb")))

    (t1 "<hr>"   '((hr)))
    (t1 "<hr/>"  '((hr)))
    (t1 "<hr />" '((hr)))

    (t1 "<hr noshade>"     `((hr (,at (noshade)))))
    (t1 "<hr noshade/>"    `((hr (,at (noshade)))))
    (t1 "<hr noshade />"   `((hr (,at (noshade)))))
    (t1 "<hr noshade / >"  `((hr (,at (noshade)))))
    (t1 "<hr noshade=1 />" `((hr (,at (noshade "1")))))
    (t1 "<hr noshade=1/>"  `((hr (,at (noshade "1")))))

    (t1 "<q>aaa<p/>bbb</q>ccc</p>ddd" '((q "aaa" (p) "bbb") "ccc" "ddd"))

    (t1 "&lt;" '("<"))
    (t1 "&gt;" '(">"))

    (t1 "Gilbert &amp; Sullivan" '("Gilbert & Sullivan"))
    (t1 "Gilbert &amp Sullivan"  '("Gilbert & Sullivan"))
    (t1 "Gilbert & Sullivan"     '("Gilbert & Sullivan"))

    (t1 "Copyright &copy; Foo" `("Copyright "
                                 (,entity "additional" "copy")
                                 " Foo"))
    (t1 "aaa&copy;bbb" `("aaa" (,entity "additional" "copy") "bbb"))
    (t1 "aaa&copy" `("aaa" (,entity "additional" "copy")))

    (t1 "&#42;"  '("*"))
    (t1 "&#42"   '("*"))
    (t1 "&#42x"  '("*x"))
    (t1 "&#151"  (list (string (htmlprag-internal:a2c 151))))
    (t1 "&#1000" `((,entity "additional-char" "1000")))

    (t1 "&#x42"  '("B"))
    (t1 "&#xA2"  (list (string (htmlprag-internal:a2c 162))))
    (t1 "&#xFF"  (list (string (htmlprag-internal:a2c 255))))
    (t1 "&#x100" `((,entity "additional-char" "256")))

    (t1 "&#X42"  '("B"))

    (t1 "&42;"  '("&42;"))

    (t1 "aaa&copy;bbb&amp;ccc&lt;ddd&&gt;eee&#42;fff&#1000;ggg&#x5a;hhh"
        `("aaa"
          (,entity "additional" "copy")
          "bbb&ccc<ddd&>eee*fff"
          (,entity "additional-char" "1000")
          "gggZhhh"))

    (t1 (string-append
         "<IMG src=\"http://pics.ebay.com/aw/pics/listings/"
         "ebayLogo_38x16.gif\" border=0 width=\"38\" height=\"16\" "
         "HSPACE=5 VSPACE=0\">2</FONT>")
        `((img (,at
                (src
                 "http://pics.ebay.com/aw/pics/listings/ebayLogo_38x16.gif")
                (border "0") (width "38") (height "16")
                (hspace "5") (vspace "0")))
          "2"))

    (t1 "<aaa bbb=ccc\"ddd>eee"  `((aaa (,at (bbb "ccc") (ddd)) "eee")))
    (t1 "<aaa bbb=ccc \"ddd>eee" `((aaa (,at (bbb "ccc") (ddd)) "eee")))

    (t1 (string-append
         "<HTML><Head><Title>My Title</Title></Head><Body BGColor=\"white\" "
         "Foo=42>This is a <B><I>bold-italic</B></I> test of </Erk>"
         "broken HTML.<br>Yes it is.</Body></HTML>")
        `((html (head (title "My Title"))
                (body (,at (bgcolor "white") (foo "42"))
                      "This is a "
                      (b (i "bold-italic"))
                      " test of "
                      "broken HTML."
                      (br)
                      "Yes it is."))))

    (t1 (string-append
         "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
         " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">")
        `((,decl ,(string->symbol "DOCTYPE")
                 html
                 ,(string->symbol "PUBLIC")
                 "-//W3C//DTD XHTML 1.0 Strict//EN"
                 "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")))

    (t1 (string-append
         "<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" "
         "lang=\"en\">")
        `((html (,at (xmlns "http://www.w3.org/1999/xhtml")
                     (lang "en") (lang "en")))))

    (t1 (string-append
         "<html:html xmlns:html=\"http://www.w3.org/TR/REC-html40\">"
         "<html:head><html:title>Frobnostication</html:title></html:head>"
         "<html:body><html:p>Moved to <html:a href=\"http://frob.com\">"
         "here.</html:a></html:p></html:body></html:html>")
        `((html (,at (xmlns:html "http://www.w3.org/TR/REC-html40"))
                (head (title "Frobnostication"))
                (body (p "Moved to "
                         (a (,at (href "http://frob.com"))
                            "here."))))))

    (t1 (string-append
         "<RESERVATION xmlns:HTML=\"http://www.w3.org/TR/REC-html40\">"
         "<NAME HTML:CLASS=\"largeSansSerif\">Layman, A</NAME>"
         "<SEAT CLASS=\"Y\" HTML:CLASS=\"largeMonotype\">33B</SEAT>"
         "<HTML:A HREF=\"/cgi-bin/ResStatus\">Check Status</HTML:A>"
         "<DEPARTURE>1997-05-24T07:55:00+1</DEPARTURE></RESERVATION>")
        `((reservation (,at (,(string->symbol "xmlns:HTML")
                             "http://www.w3.org/TR/REC-html40"))
                       (name (,at (class "largeSansSerif"))
                             "Layman, A")
                       (seat (,at (class "Y") (class "largeMonotype"))
                             "33B")
                       (a (,at (href "/cgi-bin/ResStatus"))
                          "Check Status")
                       (departure "1997-05-24T07:55:00+1"))))

    (t1 (string-append
         "<html><head><title></title><title>whatever</title></head><body>"
         "<a href=\"url\">link</a><p align=center><ul compact style=\"aa\">"
         "<p>BLah<!-- comment <comment> --> <i> italic <b> bold <tt> ened </i>"
         " still &lt; bold </b></body><P> But not done yet...")
        `((html (head (title) (title "whatever"))
                (body (a (,at (href "url")) "link")
                      (p (,at (align "center"))
                         (ul (,at (compact) (style "aa"))))
                      (p "BLah"
                         (,comment " comment <comment> ")
                         " "
                         (i " italic " (b " bold " (tt " ened ")))
                         " still < bold "))
                (p " But not done yet..."))))

    (t1 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
        `((,pi xml "version=\"1.0\" encoding=\"UTF-8\"")))

    (t1 "<?php php_info(); ?>" `((,pi php "php_info(); ")))
    (t1 "<?php php_info(); ?"  `((,pi php "php_info(); ?")))
    (t1 "<?php php_info(); "   `((,pi php "php_info(); ")))

    (t1 "<?foo bar ? baz > blort ?>" `((,pi foo "bar ? baz > blort ")))

    (t1 "<?foo b?>x" `((,pi foo "b") "x"))
    (t1 "<?foo ?>x"  `((,pi foo "")  "x"))
    (t1 "<?foo ?>x"  `((,pi foo "")  "x"))
    (t1 "<?foo?>x"   `((,pi foo "")  "x"))
    (t1 "<?f?>x"     `((,pi f   "")  "x"))
    (t1 "<??>x"      `((,pi #f  "")  "x"))
    (t1 "<?>x"       `((,pi #f  ">x")))

    (t1 "<foo bar=\"baz\">blort" `((foo (,at (bar "baz")) "blort")))
    (t1 "<foo bar='baz'>blort"   `((foo (,at (bar "baz")) "blort")))
    (t1 "<foo bar=\"baz'>blort"  `((foo (,at (bar "baz'>blort")))))
    (t1 "<foo bar='baz\">blort"  `((foo (,at (bar "baz\">blort")))))

    (t1 (string-append "<p>A</p>"
                       "<script>line0 <" lf
                       "line1" lf
                       "<line2></script>"
                       "<p>B</p>")
        `((p "A")
          (script ,(string-append "line0 <" lf)
                  ,(string-append "line1"   lf)
                  "<line2>")
          (p "B")))

    (t1 "<xmp>a<b>c</XMP>d"         '((xmp "a<b>c") "d"))
    (t1 "<XMP>a<b>c</xmp>d"         '((xmp "a<b>c") "d"))
    (t1 "<xmp>a<b>c</foo:xmp>d"     '((xmp "a<b>c") "d"))
    (t1 "<foo:xmp>a<b>c</xmp>d"     '((xmp "a<b>c") "d"))
    (t1 "<foo:xmp>a<b>c</foo:xmp>d" '((xmp "a<b>c") "d"))
    (t1 "<foo:xmp>a<b>c</bar:xmp>d" '((xmp "a<b>c") "d"))

    (t1 "<xmp>a</b>c</xmp>d"     '((xmp "a</b>c")     "d"))
    (t1 "<xmp>a</b >c</xmp>d"    '((xmp "a</b >c")    "d"))
    (t1 "<xmp>a</ b>c</xmp>d"    '((xmp "a</ b>c")    "d"))
    (t1 "<xmp>a</ b >c</xmp>d"   '((xmp "a</ b >c")   "d"))
    (t1 "<xmp>a</b:x>c</xmp>d"   '((xmp "a</b:x>c")   "d"))
    (t1 "<xmp>a</b::x>c</xmp>d"  '((xmp "a</b::x>c")  "d"))
    (t1 "<xmp>a</b:::x>c</xmp>d" '((xmp "a</b:::x>c") "d"))
    (t1 "<xmp>a</b:>c</xmp>d"    '((xmp "a</b:>c")    "d"))
    (t1 "<xmp>a</b::>c</xmp>d"   '((xmp "a</b::>c")   "d"))
    (t1 "<xmp>a</xmp:b>c</xmp>d" '((xmp "a</xmp:b>c") "d"))

    (let ((expected `((p "real1")
                      ,lf
                      (xmp ,lf
                           ,(string-append "alpha"       lf)
                           ,(string-append "<P>fake</P>" lf)
                           ,(string-append "bravo"       lf))
                      (p "real2"))))

      (t1 (string-append "<P>real1</P>" lf
                         "<XMP>"        lf
                         "alpha"        lf
                         "<P>fake</P>"  lf
                         "bravo"        lf
                         "</XMP "       lf
                         "<P>real2</P>")
          expected)

      (t1 (string-append "<P>real1</P>" lf
                         "<XMP>"        lf
                         "alpha"        lf
                         "<P>fake</P>"  lf
                         "bravo"        lf
                         "</XMP"        lf
                         "<P>real2</P>")
          expected))

    (t1 "<xmp>a</xmp>x"                       '((xmp "a")   "x"))
    (t1 (string-append "<xmp>a" lf "</xmp>x") `((xmp ,(string-append "a" lf))
                                                "x"))
    (t1 "<xmp></xmp>x"                        '((xmp)       "x"))

    (t1 "<xmp>a</xmp" '((xmp "a")))
    (t1 "<xmp>a</xm"  '((xmp "a</xm")))
    (t1 "<xmp>a</x"   '((xmp "a</x")))
    (t1 "<xmp>a</"    '((xmp "a</")))
    (t1 "<xmp>a<"     '((xmp "a<")))
    (t1 "<xmp>a"      '((xmp "a")))
    (t1 "<xmp>"       '((xmp)))
    (t1 "<xmp"        '((xmp)))

    (t1 "<xmp x=42 " `((xmp (,at (x "42")))))
    (t1 "<xmp x= "   `((xmp (,at (x)))))
    (t1 "<xmp x "    `((xmp (,at (x)))))
    (t1 "<xmp x"     `((xmp (,at (x)))))

    (t1 "<script>xxx"  '((script "xxx")))
    (t1 "<script/>xxx" '((script) "xxx"))

    ;; TODO: Add verbatim-pair cases with attributes in the end tag.

    (t2 '(p)            "<p></p>")
    (t2 '(p "CONTENT")  "<p>CONTENT</p>")
    (t2 '(br)           "<br />")
    (t2 '(br "CONTENT") "<br />")

    (t2 `(hr (,at (clear "all"))) "<hr clear=\"all\" />")

    (t2 `(hr (,at (noshade)))           "<hr noshade />")
    (t2 `(hr (,at (noshade #t)))        "<hr noshade />")
    (t2 `(hr (,at (noshade "noshade"))) "<hr noshade=\"noshade\" />")

    (t2 `(hr (,at (aaa "bbbccc")))       "<hr aaa=\"bbbccc\" />")
    (t2 `(hr (,at (aaa "bbb'ccc")))      "<hr aaa=\"bbb'ccc\" />")
    (t2 `(hr (,at (aaa "bbb\"ccc")))     "<hr aaa='bbb\"ccc' />")
    (t2 `(hr (,at (aaa "bbb\"ccc'ddd"))) "<hr aaa=\"bbb&quot;ccc'ddd\" />")

    (t2 `(,pi xml "version=\"1.0\" encoding=\"UTF-8\"")
        "<?xml version=\"1.0\" encoding=\"UTF-8\"?>")

    (t2 `(,decl ,(string->symbol "DOCTYPE")
                html
                ,(string->symbol "PUBLIC")
                "-//W3C//DTD XHTML 1.0 Strict//EN"
                "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd")
        (string-append
         "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\""
         " \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"))

    ;; TODO: Write more test cases for HTML encoding.

    ;; TODO: Document this.
    ;;
    ;; (define html-1 "<myelem myattr=\"&\">")
    ;; (define shtml   (html->shtml html-1))
    ;; shtml
    ;; (define html-2 (shtml->html shtml))
    ;; html-2

    (tests-end)))

;;; @unnumberedsec History

;;; @table @asis
;;;
;;; @item Version 0.11 --- 13 May 2004
;;; To reduce likely namespace collisions with SXML tools, and in anticipation
;;; of a forthcoming set of new features, introduced the concept of ``SHTML,''
;;; which will be elaborated upon in a future version of HtmlPrag.  Renamed
;;; @code{sxml-@var{x}-symbol} to @code{shtml-@var{x}-symbol},
;;; @code{sxml-html-@var{x}} to @code{shtml-@var{x}}, and
;;; @code{sxml-token-kind} to @code{shtml-token-kind}.  @code{html->shtml},
;;; @code{shtml->html}, and @code{write-shtml-as-html} have been added as
;;; names.  Considered deprecated but still defined (see the ``Deprecated''
;;; section of this documentation) are @code{sxml->html} and
;;; @code{write-sxml-html}.  The growing pains should now be all but over.
;;; Internally, @code{htmlprag-internal:error} introduced for Bigloo
;;; portability.  SISC returned to the test list; thanks to Scott G.  Miller
;;; for his help.  Fixed a new character @code{eq?}  bug, thanks to SISC.
;;;
;;; @item Version 0.10 --- 11 May 2004
;;; All public identifiers have been renamed to drop the ``@code{htmlprag:}''
;;; prefix.  The portability identifiers have been renamed to begin with an
;;; @code{htmlprag-internal:} prefix, are now considered strictly
;;; internal-use-only, and have otherwise been changed.  @code{parse-html} and
;;; @code{always-empty-html-elements} are no longer public.
;;; @code{test-htmlprag} now tests @code{html->sxml} rather than
;;; @code{parse-html}.  SISC temporarily removed from the test list, until an
;;; open source Java that works correctly is found.
;;;
;;; @item Version 0.9 --- 7 May 2004
;;; HTML encoding procedures added.  Added
;;; @code{htmlprag:sxml-html-entity-value}.  Upper-case @code{X} in hexadecimal
;;; character entities is now parsed, in addition to lower-case @code{x}.
;;; Added @code{htmlprag:always-empty-html-elements}.  Added additional
;;; portability bindings.  Added more test cases.
;;;
;;; @item Version 0.8 --- 27 April 2004
;;; Entity references (symbolic, decimal numeric, hexadecimal numeric) are now
;;; parsed into @code{*ENTITY*} SXML.  SXML symbols like @code{*TOP*} are now
;;; always upper-case, regardless of the Scheme implementation.  Identifiers
;;; such as @code{htmlprag:sxml-top-symbol} are bound to the upper-case
;;; symbols.  Procedures @code{htmlprag:html->sxml-0nf},
;;; @code{htmlprag:html->sxml-1nf}, and @code{htmlprag:html->sxml-2nf} have
;;; been added.  @code{htmlprag:html->sxml} now an alias for
;;; @code{htmlprag:html->sxml-0nf}.  @code{htmlprag:parse} has been refashioned
;;; as @code{htmlprag:parse-html} and should no longer be directly.  A number
;;; of identifiers have been renamed to be more appropriate when the
;;; @code{htmlprag:} prefix is dropped in some implementation-specific
;;; packagings of HtmlPrag: @code{htmlprag:make-tokenizer} to
;;; @code{htmlprag:make-html-tokenizer}, @code{htmlprag:parse/tokenizer} to
;;; @code{htmlprag:parse-html/tokenizer}, @code{htmlprag:html->token-list} to
;;; @code{htmlprag:tokenize-html}, @code{htmlprag:token-kind} to
;;; @code{htmlprag:sxml-token-kind}, and @code{htmlprag:test} to
;;; @code{htmlprag:test-htmlprag}.  Verbatim elements with empty-element tag
;;; syntax are handled correctly.  New versions of Bigloo and RScheme tested.
;;;
;;; @item Version 0.7 --- 10 March 2004
;;; Verbatim pair elements like @code{script} and @code{xmp} are now parsed
;;; correctly.  Two Scheme implementations have temporarily been dropped from
;;; regression testing: Kawa, due to a Java bytecode verifier error likely due
;;; to a Java installation problem on the test machine; and SXM 1.1, due to
;;; hitting a limit on the number of literals late in the test suite code.
;;; Tested newer versions of Bigloo, Chicken, Gauche, Guile, MIT Scheme, PLT
;;; MzScheme, RScheme, SISC, and STklos.  RScheme no longer requires the
;;; ``@code{(define get-output-string close-output-port)}'' workaround.
;;;
;;; @item Version 0.6 --- 3 July 2003
;;; Fixed uses of @code{eq?} in character comparisons, thanks to Scott G.
;;; Miller.  Added @code{htmlprag:html->normalized-sxml} and
;;; @code{htmlprag:html->nonnormalized-sxml}.  Started to add
;;; @code{close-output-port} to uses of output strings, then reverted due to
;;; bug in one of the supported dialects.  Tested newer versions of Bigloo,
;;; Gauche, PLT MzScheme, RScheme.
;;;
;;; @item Version 0.5 --- 26 February 2003
;;; Removed uses of @code{call-with-values}.  Re-ordered top-level definitions,
;;; for portability.  Now tests under Kawa 1.6.99, RScheme 0.7.3.2, Scheme 48
;;; 0.57, SISC 1.7.4, STklos 0.54, and SXM 1.1.
;;;
;;; @item Version 0.4 --- 19 February 2003
;;; Apostrophe-quoted element attribute values are now handled.  A bug that
;;; incorrectly assumed left-to-right term evaluation order has been fixed
;;; (thanks to MIT Scheme for confronting us with this).  Now also tests OK
;;; under Gauche 0.6.6 and MIT Scheme 7.7.1.  Portability improvement for
;;; implementations (e.g., RScheme 0.7.3.2.b6, Stalin 0.9) that cannot read
;;; @code{@@} as a symbol (although those implementations tend to present other
;;; portability issues, as yet unresolved).
;;;
;;; @item Version 0.3 --- 5 February 2003
;;; A test suite with 66 cases has been added, and necessary changes have been
;;; made for the suite to pass on five popular Scheme implementations.  XML
;;; processing instructions are now parsed.  Parent constraints have been added
;;; for @code{colgroup}, @code{tbody}, and @code{thead} elements.  Erroneous
;;; input, including invalid hexadecimal entity reference syntax and extraneous
;;; double quotes in element tags, is now parsed better.
;;; @code{htmlprag:token-kind} emits symbols more consistent with SXML.
;;;
;;; @item Version 0.2 --- 2 February 2003
;;; Portability improvements.
;;;
;;; @item Version 0.1 --- 31 January 2003
;;; Dusted off old Guile-specific code from April 2001, converted to emit SXML,
;;; mostly ported to R5RS and SRFI-6, added some XHTML support and
;;; documentation.  A little preliminary testing has been done, and the package
;;; is already useful for some applications, but this release should be
;;; considered a preview to invite comments.
;;;
;;; @end table

;;; @unnumberedsec References

;;; @table @asis
;;;
;;; @item [HTML]
;;; Dave Raggett, Arnaud Le Hors, Ian Jacobs, eds., ``HTML 4.01
;;; Specification,'' W3C Recommendation, 24 December 1999.@*
;;; @uref{http://www.w3.org/TR/1999/REC-html401-19991224/}
;;;
;;; @item [LGPL]
;;; Free Software Foundation, ``GNU Lesser General Public License,'' Version
;;; 2.1, February 1999, 59 Temple Place, Suite 330, Boston, MA 02111-1307
;;; USA.@*
;;; @uref{http://www.gnu.org/copyleft/lesser.html}
;;;
;;; @item [SRFI-6]
;;; William D. Clinger, ``Basic String Ports,'' SRFI 6, 1 July 1999.@*
;;; @uref{http://srfi.schemers.org/srfi-6/srfi-6.html}
;;;
;;; @item [SRFI-23]
;;; Stephan Houben, ``Error reporting mechanism,'' SRFI 23, 26 April 2001.@*
;;; @uref{http://srfi.schemers.org/srfi-23/srfi-23.html}
;;;
;;; @item [SSAX]
;;; Oleg Kiselyov, ``A functional-style framework to parse XML documents,''
;;; 5 September 2002.@*
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#XML-parser}
;;;
;;; @item [SSAX-HTML]
;;; Oleg Kiselyov, ``Permissive parsing of perhaps invalid HTML,'' Version 1.1,
;;; 3 November 2001.@*
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#HTML-parser}
;;;
;;; @item [SXML]
;;; Oleg Kiselyov, ``SXML,'' revision 3.0.@*
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/SXML.html}
;;;
;;; @item [SXML-Tools]
;;; Kirill Lisovsky, ``SXPath and SXPointer,''@*
;;; @uref{http://pair.com/lisovsky/query/sxpath/}
;;;
;;; @item [SXPath]
;;; Oleg Kiselyov, ``SXPath,'' version 3.5, 12 January 2001.@*
;;; @uref{http://pobox.com/~oleg/ftp/Scheme/xml.html#SXPath}
;;;
;;; @item [XHTML]
;;; ``XHTML 1.0: The Extensible HyperText Markup Language: A Reformulation of
;;; HTML 4 in XML 1.0,'' W3C Recommendation, 26 January 2000.@*
;;; @uref{http://www.w3.org/TR/2000/REC-xhtml1-20000126/}
;;;
;;; @item [XML-Names]
;;; Tim Bray, Dave Hollander, Andrew Layman, eds., ``Namespaces in XML,'' W3C
;;; Recommendation, 14 January 1999.@*
;;; @uref{http://www.w3.org/TR/1999/REC-xml-names-19990114/}
;;;
;;; @end table
;; ############## END CANONICAL htmlprag.scm ##############
(export
shtml-comment-symbol
shtml-decl-symbol
shtml-empty-symbol
shtml-end-symbol
shtml-entity-symbol
shtml-pi-symbol
shtml-start-symbol
shtml-text-symbol
shtml-top-symbol
shtml-named-char-id
shtml-numeric-char-id
shtml-entity-value
make-html-tokenizer
tokenize-html
shtml-token-kind
parse-html/tokenizer
html->sxml-0nf
html->sxml-1nf
html->sxml-2nf
html->sxml
html->shtml
write-shtml-as-html
shtml->html
sxml->html
write-sxml-html
test-htmlprag
)
;;; arch-tag: 491d7e61-5690-4b76-bc8f-d70315c10ed5
;;; htmlprag.scm ends here
