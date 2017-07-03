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

(define* (parse port #:optional (definition-creator deserialize-record-instance))
  "Parses GCC LU from PORT, resolves all references, and then calls DEFINITION-CREATOR for each of the nodes.
The list of all the DEFINITION-CREATOR results is returned."
  (let* ((entries ((make-parser) (make-lexer port) error))
         (entries (alist->hash-table entries))
         (resolve-references! (lambda (id entry)
                                (define (resolve! n)
                                  (match n
                                    ((key . value)
                                      (match value
                                       (('reference x-id) (set-cdr! n (hash-ref-or-die entries x-id error)))
                                       (_ value)))))
                                (match entry
                                  ((type-name attributes)
                                   (list type-name (for-each resolve! attributes))))))
         (_ (hash-for-each resolve-references! entries))
         (entries (hash-map->list cons entries))
         (create-record-instance (lambda (id type-name attributes)
                                   (cons id
                                     (definition-creator type-name attributes))))
         (result (map (cut apply create-record-instance <>) entries))
         (result (map cdr result))) ; strip ids
    ;(write (hash-ref result (string->symbol "1")))
    result))
