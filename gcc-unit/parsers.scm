;;; Copyright 2017 - 2017, Danny Milosavljevic
;;; SPDX-License-Identifier: BSD-3-Clause

(define-module (gcc-unit parsers)
  #:use-module (system base lalr)
  #:use-module (gcc-unit records)
  #:use-module (gcc-unit lexers)
  #:use-module (gcc-unit serializers)
  #:use-module (ice-9 match)
  #:use-module (ice-9 hash-table)
  #:use-module (srfi srfi-26) ; cut
  #:export (parse))

(define (reference value)
  `(reference ,value))

(define (make-parser)
  (lalr-parser
   (def @ value : long short unsigned signed complex sign op) ; terminals
   (program (definition program) : (cons $1 $2)
            (*eoi*) : '())
   (definition (def @ id type-name attributes) : (list $3 $4 $5)
               (def @ id type-name) : (list $3 $4 '()))
   (attributes (attribute attributes) : (cons $1 $2)
               (attribute) : (cons $1 '()))
   (type-name (value) : (string->symbol $1)) ; At the meta level.
   (name (value) : (string->symbol $1)
         (op value) : (string->symbol (string-append "op " $2))) ; Special case because of the space.
   (id (value) : (string->symbol $1))
   (modified-value (value) : $1
                   (short modified-value) : (string-append $1 "-" $2)
                   (long modified-value) : (string-append $1 "-" $2)
                   (unsigned modified-value) : (string-append $1 "-" $2)
                   (signed modified-value) : (string-append $1 "-" $2)
                   (complex modified-value) : (string-append $1 "-" $2))
   (modified-value-b (modified-value unsigned) : (string-append $1 "-" $2) ; WTF
                     (modified-value) : $1)
   (values (@ id) : (reference $2)
           (modified-value-b) : $1)
   (attribute (name : values) : (cons $1 $3)
              (sign : signed) : (cons $1 "signed")
              (sign : unsigned) : (cons $1 "unsigned"))))

(define (hash-ref-or-die hash-table key err)
  "Looks KEY up in HASH-TABLE.  If that's not there, calls ERR with the KEY."
  (let ((result (hash-ref hash-table key #f)))
    (if (eq? result #f)
      (err key)
      result)))

(define* (parse port)
  "Parses GCC LU from PORT, resolves all references, and returns the first node.
Note that attribute values can and will be eq? if they were resolved from the same id - and the caller may want to handle that case.
Nodes cannot be eq? to each other."
  (let* ((entries ((make-parser) (make-lexer port) error))
         (entries (alist->hash-table entries))
         (resolve-attribute-value-reference!
          (lambda (n)
            (match n
             ((key . value)
              (match value
               (('reference x-id) (set-cdr! n (hash-ref-or-die entries x-id error)))
               (_ value))))))
         (resolve-references! (lambda (id entry)
                                (match entry
                                  ((type-name attributes)
                                   (list type-name (for-each resolve-attribute-value-reference! attributes)))))))
    (hash-for-each resolve-references! entries)
    (hash-ref entries (string->symbol "1")))) ; first entry (id 1) chains all the others.
