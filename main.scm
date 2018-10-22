;;; Copyright 2017 - 2017, Danny Milosavljevic
;;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))
(use-modules (ice-9 pretty-print))
(use-modules (ice-9 poe))
(use-modules (srfi srfi-26)) ; cut

(define input-file-name
  (match (cdr (command-line))
    (() "tests/1/input")
    ((input-file-name) input-file-name)))

(define (decode-name node)
  "Given an identifier node, extracts the name string out of it."
  (match node
    (('identifier_node attributes)
     (assoc-ref attributes 'strg))))

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

(define (decode-record-fields flds-node)
  (match flds-node
    (('field_decl attributes) ; name type scpe srcp chain size algn bpos
     ;(pretty-print (assoc-ref attributes 'type))
     ;(newline)
     (let ((name (decode-name (assoc-ref attributes 'name)))
           (chain (assoc-ref attributes 'chain))
           ;FIXME (type (decode-basic-type (assoc-ref attributes 'type))) ; loops somewhere
           )
       ;(write type)
       ;(newline)
       (cons name (if chain (decode-record-fields chain) '())))
       )))

(define (decode-basic-type type-node)
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
    (('record_type attributes)  ; TODO handle size, algn, tag=="struct"
     (list "record" (decode-record-fields (assoc-ref attributes 'flds))))))

(define decode-basic-type (pure-funcq decode-basic-type))

(define (decode-prms prms)
  (if prms
    (match prms
      (('tree_list attributes)
       (cons (decode-basic-type (assoc-ref attributes 'valu))
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
              (('function_decl attributes)
                 ; Note: body == "undefined"
                 (let* ((name (decode-name (assoc-ref attributes 'name)))
                        (link (assoc-ref attributes 'link))
                        (type-node (assoc-ref attributes 'type)))
                   (if (and (string-prefix? "ped_" name) (string=? "extern" link))
                          (match type-node
                            (('function_type type-attributes)
                              (let* ((type-retn (decode-basic-type (assoc-ref type-attributes 'retn)))
                                     (args (decode-args (assoc-ref attributes 'args))) ; seldomly exist.
                                     (type-prms (decode-prms (assoc-ref type-attributes 'prms))))
                           (write name)
                           ;(write args)
                           (write type-prms)
                           (write "->")
                           (write type-retn)
                           (newline)))))))
              (_ #f))
            ) f)

