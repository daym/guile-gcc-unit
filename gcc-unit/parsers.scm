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
   (def @ value :) ; terminals
   (program (definition program) : (cons $1 $2)
            (*eoi*) : '())
   (definition (def @ id type-name attributes) : (list $3 $4 $5)
               (def @ id type-name) : (list $3 $4 '()))
   (attributes (attribute attributes) : (cons $1 $2)
               (attribute) : (cons $1 '()))
   (type-name (value) : (string->symbol $1)) ; At the meta level.
   (name (value) : (string->symbol $1))
   (id (value) : (string->symbol $1))
   (nonref-values ;(value nonref-values) : (string-append $1 " " $2)
                  (value) : $1)
   (values ;(value nonref-values) : (string-append $1 " " $2) ; special-case "strg: foo bar baz lngt: 11" - which is really a stupid way to write it but hey.
           (@ id) : (reference $2)
           (value) : $1)
   (attribute (name : values) : (cons $1 $3))))

; TODO eval the result, resolving "reference"s.

(define (resolve-references! id entry)
  (define (resolve! n)
    (match n
     ((key . value)
       (match value
         (('reference x-id) (set-cdr! n (cons "HELLLOOOO" x-id)))
         (_ value)))))
  (write id)
  (match entry
    ((type-name attributes) (list type-name (for-each resolve! attributes)))))

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
