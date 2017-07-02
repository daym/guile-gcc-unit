;;; SPDX BSD-3-Clause

(define-module (gcc-unit records)
  #:use-module (srfi srfi-9 gnu) ; Records
  #:export (type-decl)
)

(define-immutable-record-type <translation-unit-decl>
  (translation-unit-decl)
  translation-unit-decl?)

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

(define-immutable-record-type <const-decl>
  (const-decl name type scpe srcp chain cnst)
  const-decl?
  (name const-decl-name)
  (type const-decl-type)
  (scpe const-decl-scpe)
  (srcp const-decl-srcp)
  (chain const-decl-chain)
  (cnst const-decl-cnst))

(define-immutable-record-type <enumeral-type>
  (enumeral-type size algn prec sign min max csts)
  enumeral-type?
  (size enumeral-type-size)
  (algn enumeral-type-algn)
  (prec enumeral-type-prec)
  (sign enumeral-type-sign)
  (min enumeral-type-min)
  (max enumeral-type-max)
  (csts enumeral-type-csts))

(define-immutable-record-type <field-decl>
  (field-decl name type scpe srcp chain size algn bpos)
  field-decl?
  (name field-decl-name)
  (type field-decl-type)
  (scpe field-decl-scpe)
  (srcp field-decl-srcp)
  (chain field-decl-chain)
  (size field-decl-size)
  (algn field-decl-algn)
  (bpos field-decl-bpos))

(define-immutable-record-type <function-decl>
  (function-decl name type scpe srcp chain args link body)
  function-decl?
  (name function-decl-name)
  (type function-decl-type) ; look for argument types here.
  (scpe function-decl-scpe)
  (srcp function-decl-srcp)
  (chain function-decl-chain)
  (args function-decl-args) ; argument names.
  (link function-decl-link)
  (body function-decl-body))

(define-immutable-record-type <function-type>
  (function-type size algn retn prms)
  function-type?
  (size function-type-size)
  (algn function-type-algn)
  (retn function-type-retn)
  (prms function-type-prms))

(define-immutable-record-type <identifier-node>
  (identifier-node strg lngt)
  identifier-node?
  (strg identifier-node-strg)
  (lngt identifier-node-lngt))

(define-immutable-record-type <integer-cst>
  (integer-cst type int)
  integer-cst?
  (type integer-cst-type)
  (int integer-cst-int))

(define-immutable-record-type <integer-type>
  (integer-type name size algn prec sign min max)
  integer-type?
  (name integer-type-name)
  (size integer-type-size)
  (algn integer-type-algn)
  (prec integer-type-prec)
  (sign integer-type-sign)
  (min integer-type-min)
  (max integer-type-max))

(define-immutable-record-type <void-type>
  (void-type qual name unql algn)
  void-type?
  (qual void-type-qual)
  (name void-type-name)
  (unql void-type-unql)
  (algn void-type-algn))

(define-immutable-record-type <vector-type>
  (vector-type size algn)
  vector-type?
  (size vector-type-size)
  (algn vector-type-algn))

(define-immutable-record-type <modify-expr>
  (modify-expr type op-0 op-1)
  modify-expr?
  (type modify-expr-type)
  (op-0 modify-expr-op-0)
  (op-1 modify-expr-op-1))

(define-immutable-record-type <nop-expr>
  (nop-expr type op-0)
  nop-expr?
  (type nop-expr-type)
  (op-0 nop-expr-op-0))

(define-immutable-record-type <parm-decl>
  (parm-decl name type scpe srcp argt size algn used)
  parm-decl?
  (name parm-decl-name)
  (type parm-decl-type)
  (scpe parm-decl-scpe)
  (srcp parm-decl-srcp)
  (argt parm-decl-argt)
  (size parm-decl-size)
  (algn parm-decl-algn)
  (used parm-decl-used))

(define-immutable-record-type <plus-expr>
  (plus-expr type op-0 op-1)
  plus-expr?
  (type plus-expr-type)
  (op-0 plus-expr-op-0)
  (op-1 plus-expr-op-1))

(define-immutable-record-type <record-type>
  (record-type name size algn tag flds)
  record-type?
  (name record-type-name)
  (size record-type-size)
  (algn record-type-algn)
  (tag record-type-tag)
  (flds record-type-flds))

(define-immutable-record-type <result-decl>
  (result-decl type scpe srcp note size algn)
  result-decl?
  (type result-decl-type)
  (scpe result-decl-scpe)
  (srcp result-decl-srcp)
  (note result-decl-note)
  (size result-decl-size)
  (algn result-decl-algn))

(define-immutable-record-type <real-type>
  (real-type name size algn prec)
  real-type?
  (name real-type-name)
  (size real-type-size)
  (algn real-type-algn)
  (prec real-type-prec))

(define-immutable-record-type <reference-type>
  (reference-type size algn refd)
  reference-type?
  (size reference-type-size)
  (algn reference-type-algn)
  (refd reference-type-refd))

(define-immutable-record-type <pointer-type>
  (pointer-type size algn ptd)
  pointer-type?
  (size pointer-type-size)
  (algn pointer-type-algn)
  (ptd pointer-type-ptd))

(define-immutable-record-type <tree-list>
  (tree-list valu chan)
  tree-list?
  (valu tree-list-valu)
  (chan tree-list-chan))

(define-immutable-record-type <var-decl>
  (var-decl name type scpe srcp chain size algn used)
  var-decl?
  (name var-decl-name)
  (type var-decl-type)
  (scpe var-decl-scpe)
  (srcp var-decl-srcp)
  (chain var-decl-chain)
  (size var-decl-size)
  (algn var-decl-algn)
  (used var-decl-used))

(define-immutable-record-type <return-expr>
  (return-expr type expr)
  return-expr?
  (type return-expr-type)
  (expr return-expr-expr))

(define-immutable-record-type <type-decl>
  (type-decl name type scpe srcp chain)
  type-decl?
  (name type-decl-name)
  (type type-decl-type)
  (scpe type-decl-scpe)
  (srcp type-decl-srcp)
  (chain type-decl-chain))

(define-immutable-record-type <pointer-bounds-type>
  (pointer-bounds-type name size algn)
  pointer-bounds-type?
  (name pointer-bounds-type-name)
  (size pointer-bounds-type-size)
  (algn pointer-bounds-type-algn))

(define-immutable-record-type <trunc-div-expr>
  (trunc-div-expr type op-0 op-1)
  trunc-div-expr?
  (type trunc-dir-expr-type)
  (op-0 trunc-div-expr-op-0)
  (op-1 trunc-div-expr-op-1))

(define-immutable-record-type <union-type>
  (union-type name size algn tag flds)
  union-type?
  (name union-type-name)
  (size union-type-size)
  (algn union-type-algn)
  (tag union-type-tag)
  (flds union-type-flds))

(define-immutable-record-type <statement-list>
  (statement-list _0 _1)
  statement-list?
  (_0 statement-list-_0)
  (_1 statement-list-_1))
