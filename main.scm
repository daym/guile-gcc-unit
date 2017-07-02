(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))

(define f (call-with-input-string "
@1   type_decl   name: @2 lngt: 2
@2  identifier_node strg: int   lngt: 3" parse))

(define (visit node)
  (write node)
  (match node
    ((_ 'type-decl . args) (write " TYPE "))
    (_ #f))
  (newline))

(map visit f)
