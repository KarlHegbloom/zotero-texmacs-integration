;;; coding: utf-8
;;; ✠ ✞ ♼ ☮ ☯ ☭ ☺

(texmacs-module (test-clipboard-cut-paste)
  (:use (kernel texmacs tm-modes)
        (kernel library content)
        (kernel library list)
        (utils base environment)
        (utils edit selections)
        (utils library cursor)
        (generic document-edit)
        (text text-structure)
        (generic document-part)
        ;;(part part-shared) ;; for buffer-initialize and buffer-notify
        (generic document-style)
        (generic generic-edit)
        (generic format-edit)
        (convert tools sxml)))


(kbd-commands
  ("testtag" "Insert testtag tag."
   (insert-testtag)))

(tm-define (insert-testtag)
  (insert `(testtag ,(create-unique-id) (tuple "3" (raw-data "") "false") "Text")))


(tm-define (clipboard-cut which)
  (:require (selection-active-any?))
  (let ((selection-t (selection-tree)))
    (tm-output (format #f "~s" (tree->stree selection-t)))
    (newline)
    (former which)))


(tm-define (has-testtags? t)
  (tm-find t (cut tm-is? <> 'testtag)))


;;; untested
(tm-define (clipboard-paste which)
  (:require (has-testtags? (clipboard-get which)))
  (let* ((t (tree-ref (clipboard-get which) 1))
         (testtags (map tree->stree (tm-search t (cut tm-is? <> 'testtag)))))
    (tm-output (format #f "~s" (tree->stree t))) (newline)
    (tm-output (format #f "~s" testtags)) (newline)
    (map (lambda (subtree)
           (let* ((st (tree->stree subtree)))
             (cons (car subtree)
                   (cons (create-unique-id)
                         (cddr subtree))))))
         testtags)
    (insert t)
    ;; todo: maintain the new data-structures here.
    ))
