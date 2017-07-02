;;; SPDX BSD-3-Clause

(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))
(use-modules (srfi srfi-26)) ; cut

(define (create-instance type-name attributes)
  (list type-name attributes))

(define input-file-name
  (match (cdr (command-line))
    (() "tests/1/input")
    ((input-file-name) input-file-name)))

(define f (call-with-input-file input-file-name (cut parse <> create-instance)))

(for-each (lambda (node)
            (match node
              (('identifier_node attributes)
                (write "ID"))
              (_ #f))
            (write node)
            (newline)) f)
