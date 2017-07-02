(define-module (gcc-unit parsers)
  #:use-module (system base lalr)
  #:use-module (gcc-unit records)
  #:use-module (gcc-unit lexers))

(define (skip-header-junk port)
  "Given a port, skips all the junk in front of the first '@' sign, or EOF - whichever comes first."
  (let loop ((c (peek-char port)))
    (if (and (not (eof-object? c)) (not (char=? #\@ c)))
        (begin
          (read-char port)
          (loop (peek-char port))))))

(define (reference value)
  `(reference value))

(define (make-parser)
  (lalr-parser
   (def @ value :) ; terminals
   (program (definition program) : (cons $1 $2)
            (*eoi*) : '())
   (definition (def @ id type-name attributes) : (cons $3 (cons $4 $5))
               (def @ id type-name) : (cons $3 (cons $4 '())))
   ;; TODO "no attribute" case.
   (attributes (attribute attributes) : (cons $1 $2)
               (attribute) : $1)
   (type-name (value) : (string->symbol $1)) ; At the meta level.
   (name (value) : (string->symbol $1))
   (id (value) : (string->symbol $1))
   (values ;(value values) : (string-append $1 " " $2) ; special-case "strg: foo bar baz lngt: 11" - which is really a stupid way to write it but hey.
           (@ id) : (reference $2)
           (value) : $1)
   (attribute (name : values) : (cons $1 $2))))

; TODO eval the result, resolving "reference"s.

(define-public (parse port)
  ((make-parser) (make-lexer port) error))
