;;; SPDX BSD-3-Clause

(define-module (gcc-unit records)
  #:use-module (srfi srfi-9 gnu) ; Records
)

(define-immutable-record-type <addr-expr>
  (addr-expr type op-0)
  addr-expr?
  (type addr-expr-type)
  (op-0 addr-expr-op-0))

(define-immutable-record-type <array-type>
  (array-type name unql size algn elts domn)
  array-type?
  (name array-type-name)
  (unql array-type-unql)
  (size array-type-size)
  (algn array-type-algn)
  (elts array-type-elts)
  (domn array-type-domn))

(define-immutable-record-type <bind-expr>
  (bind-expr type body)
  bind-expr?
  (type bind-expr-type)
  (body bind-expr-body))

(define-immutable-record-type <boolean-type>
  (boolean-type name size algn)
  boolean-type?
  (name boolean-type-name)
  (size boolean-type-size)
  (algn boolean-type-algn))

(define-immutable-record-type <call-expr>
  (call-expr type fn _0)
  call-expr?
  (type call-expr-type)
  (fn call-expr-fn)
  (_0 call-expr-_0))

(define-immutable-record-type <complex-type>
  (complex-type size algn)
  complex-type?
  (size complex-type-size)
  (algn complex-type-algn))

#|
const_decl name type scpe srcp chain cnst
enumeral_type size algn prec sign min max csts
field_decl name type scpe srcp chain size algn bpos
function_decl name type srcp chain body link
function_type size algn retn prms
identifier_node strg lngt
integer_cst type int
integer_type name size algn prec sign min max
modify_expr type op-0 op-1
nop_expr type op-0
parm_decl name type scpe srcp argt size algn used
plus_expr type op-0 op-1
pointer_bounds_type name size algn
pointer_type size algn ptd
real_type name size algn prec
record_type name size algn tag flds
reference_type size algn refd
result_decl type scpe srcp note size algn
return_expr type expr
statement_list _0 _1
translation_unit_decl
tree_list valu chan
trunc_div_expr type op-0 op-1
type_decl name type scpe srcp chain
union_type name size algn tag flds
var_decl name type scpe srcp chain size algn used
vector_type size algn
void_type qual name unql algn
|#
