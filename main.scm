(use-modules (gcc-unit parsers))

(write (call-with-input-string "
@1   type_decl   name: @2 lngt: 2
@2  identifier_node strg: int   lngt: 3" parse))
