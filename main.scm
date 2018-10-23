;;; Copyright 2017 - 2017, Danny Milosavljevic
;;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 poe))
(use-modules (ice-9 hash-table))
(use-modules (srfi srfi-26)) ; cut

(define input-file-name
  (match (cdr (command-line))
    (() "tests/1/input")
    ((input-file-name) input-file-name)))

(define (extract-node-attributes node)
  (match node
    ((_ attributes)
     attributes)))

(define (decode-name node)
  "Given an identifier node, extracts the name string out of it."
  (match node
    (('identifier_node attributes)
     (assoc-ref attributes 'strg))))

(define (decode-constant node)
  (match node
   (#f #f)
   (('integer_cst attributes)
    (let ((int (assoc-ref attributes 'int)))
      (string->number int)))
   (_ node)))

(define (follow-chain node)
  "Given a NODE, follow its 'chain attribute until it's not present.
Return a list of all the nodes visited."
  (match node
   (#f '())
   (_ (cons node (follow-chain (match node
                                ((id attributes)
                                 (assoc-ref attributes 'chain))))))))

(define f (follow-chain (call-with-input-file input-file-name (cut parse <>))))

(define decode-name (pure-funcq decode-name))

;; TODO: Support unions.

(define (decode-record-fields flds-node)
  (match flds-node
    (('field_decl attributes) ; name type scpe srcp chain size algn bpos
     ;(pretty-print (assoc-ref attributes 'type))
     ;(newline)
     (let ((name (decode-name (assoc-ref attributes 'name)))
           (bpos (and=> (assoc-ref attributes 'bpos) decode-constant)) ; beginning, bits
           (size (and=> (assoc-ref attributes 'size) decode-constant)) ; size, bits
           (chain (assoc-ref attributes 'chain))
           (type (decode-basic-type (assoc-ref attributes 'type))) ; loops somewhere
           )
       ;(write type)
       ;(newline)
       (cons (list name bpos size type) (if chain (decode-record-fields chain) '()))))))

(define (extract-unique-struct-name record-type-node)
  "typedefs allow type aliases.  But here, we are actually interested in the unique struct definition name."
  (match record-type-node
    (('record_type attributes)  ; TODO handle size, algn, tag=="struct"
     (let ((name (assoc-ref attributes 'name))
           (unql (assoc-ref attributes 'unql)))
       (if unql
           (extract-unique-struct-name unql)
           (match name
            (('type_decl b-attributes)
             (write (decode-name (assoc-ref b-attributes 'name)))
             (newline)
             "?")
            (('identifier_node b-attributes)
             (string-append "struct " (decode-name name)))))))))

(define (emit-struct-definition name fields)
  (write "define-struct")
  (write name)
  (newline)
  (for-each (lambda (field)
              (write field)
              (newline))
            fields)
  (write "end-struct")
  (newline))

(define *structs* (alist->hashv-table '()))

(define (register-struct-definition! node)
  (match node
   (('record_type attributes)
    (let* ((node (or (assoc-ref attributes 'unql) node))
           (name (extract-unique-struct-name node))
           (value node)
           (existing-node (hashv-ref *structs* name)))
      (hashv-set! *structs* name value)
      (when (not existing-node)
        (emit-struct-definition name (decode-record-fields (assoc-ref attributes 'flds))))
      ; FIXME (assert (eqv? existing-node node))
      name))))

(define (decode-basic-type type-node)
  "Decode basic types entirely, but keep composite types as references and deduplicate them."
  ;(write type-node)
  ;(newline)
  (match type-node
    (('function_type attributes)
     #f) ; TODO
    (('pointer_type attributes)
     (list "pointer" (decode-basic-type (assoc-ref attributes 'ptd))))
    (('void_type attributes)
     "void")
    (('integer_type attributes)
     "integer")
    (('real_type attributes)
     "real")
    (('boolean_type attributes)
     "bool")
    (('enumeral_type attributes) ; FIXME which?
     "enum")
    (('record_type attributes)
     (register-struct-definition! type-node))))

(define decode-basic-type (pure-funcq decode-basic-type))

(define (decode-type type-node)
  (match type-node
   (('record_type attributes)
    (register-struct-definition! type-node)
    ;; TODO: Check struct that is already registered (because it's unql).
    (list "record" (decode-record-fields (assoc-ref attributes 'flds))))
   (_ (decode-basic-type type-node))))

(define decode-type (pure-funcq decode-type))

(define (decode-prms prms)
  (if prms
    (match prms
      (('tree_list attributes)
       (cons (decode-type (assoc-ref attributes 'valu))
             (decode-prms (assoc-ref attributes 'chan)))))
    '())) ; Not really.  void foo() means UNSPECIFIED parameter list.

(define decode-prms (pure-funcq decode-prms))

(define (decode-args args)
  (if args
    (match args
      (('parm_decl attributes)
       ;(write "ARG")
       ;(write (decode-name (assoc-ref attributes 'name)))
       (cons (decode-name (assoc-ref attributes 'name)) ; also has type ?!
             (decode-args (assoc-ref attributes 'chain)))))
    '())) ; Not really.  Means UNSPECIFIED parameter names.

(for-each (lambda (node)
            (match node
              (('type_decl attributes)
               (when (assoc-ref attributes 'name)
                 (write (decode-name (assoc-ref attributes 'name))))
               (let* ((type-node (assoc-ref attributes 'type)))
                 (match type-node
                   (('record_type attributes)
                    ;(write "YES")
                    ;(write attributes)
                    (newline))
                   (((or 'integer_type 'real_type 'complex_type 'void_type 'array_type 'pointer_type 'boolean_type) attributes)
                    (write (car type-node))
                    (write (and=> (assoc-ref attributes 'size) decode-constant))
                    (write (and=> (assoc-ref attributes 'sign) decode-constant))
                    (write (and=> (assoc-ref attributes 'min) decode-constant))
                    (write (and=> (assoc-ref attributes 'max) decode-constant))
                    (newline))
                   ((x attributes)
                    (write "ignored")
                    (write x)
                    ;(write attributes)
                    (newline)))))
              (('function_decl attributes)
                 ; Note: body == "undefined"
                 (let* ((name (decode-name (assoc-ref attributes 'name)))
                        (link (assoc-ref attributes 'link))
                        (type-node (assoc-ref attributes 'type)))
                   (if (and (string-prefix? "ped_" name) (string=? "extern" link))
                          (match type-node
                            (('function_type type-attributes)
                              (let* ((type-retn (decode-type (assoc-ref type-attributes 'retn)))
                                     (args (decode-args (assoc-ref attributes 'args))) ; seldomly exist.
                                     (type-prms (decode-prms (assoc-ref type-attributes 'prms))))
                           (write name)
                           ;(write args)
                           (write type-prms)
                           (write "->")
                           (write type-retn)
                           (newline)))))))
              ((x attributes)
               (write x)
               (newline))))
            f)

