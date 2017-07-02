(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))

(define f (call-with-input-string "
@1   type_decl   name: @2 type: 2  chain: 4
@2  identifier_node strg: int   lngt: 3" parse))
(write f)
