;;; Copyright 2017 - 2017, Danny Milosavljevic
;;; SPDX-License-Identifier: BSD-3-Clause

(use-modules (gcc-unit parsers))
(use-modules (ice-9 match))
(use-modules (ice-9 pretty-print))
(use-modules (srfi srfi-26)) ; cut

(define (create-instance type-name attributes)
  (list type-name attributes))

(define input-file-name
  (match (cdr (command-line))
    (() "tests/1/input")
    ((input-file-name) input-file-name)))

(define f (call-with-input-file input-file-name (cut parse <> create-instance)))

(define (decode-name node)
  (match node
    (('identifier_node attributes)
     (assoc-ref attributes 'strg))))

(define (decode-basic-type type-node)
  (match type-node
    ; TODO function type
    (('pointer_type attributes)
     "pointer")
    (('void_type attributes)
     "void")
    (('integer_type attributes)
     "integer")
    (('real_type attributes)
     "real")
    (('boolean_type attributes)
     "bool")
    (('enumeral_type attributes)
     "enum")))

(define (decode-prms prms)
  (if prms
    (match prms
      (('tree_list attributes)
       (cons (decode-basic-type (assoc-ref attributes 'valu))
             (decode-prms (assoc-ref attributes 'chan)))))
    '())) ; Not really.  void foo() means UNSPECIFIED parameter list.


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
                                     (type-prms (decode-prms (assoc-ref type-attributes 'prms))))
                           (write name)
                           (write type-prms)
                           (write "->")
                           (write type-retn)
                           (newline)))))))
              (_ #f))
            ) f)
