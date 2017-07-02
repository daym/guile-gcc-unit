(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))
(use-modules (srfi srfi-26)) ; cut

(define (create-instance type-name attributes)
  (list type-name attributes))

(define f (call-with-input-string "
@1   type_decl   name: @2 type: 2  chain: 4
@2  identifier_node strg: int   lngt: 3" (cut parse <> create-instance)))

(for-each (lambda (node)
            (match node
              (('identifier_node attributes)
                (write "ID"))
              (_ #f))
            (write node)
            (newline)) f)
