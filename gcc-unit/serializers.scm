(define-module (gcc-unit serializers)
  #:use-module (gcc-unit records)
  #:use-module (ice-9 hash-table)
  #:use-module (ice-9 regex)
  #:use-module (srfi srfi-26)) ; cut

(define sentinel (gensym))

(define (mangle-name name)
  (string->symbol (regexp-substitute/global #f "([A-Za-z])_" (symbol->string name) 'pre 1 'post)))

(define (hash-ref-mangle-or-die hash-table key err)
  "Mangles KEY.  Looks that up in HASH-TABLE.  If that's not there, calls ERR with the mangled KEY."
  (let ((result (hash-ref hash-table (mangle-name key) sentinel)))
    (if (eq? result sentinel)
      (err (mangle-name key))
      result)))

(define* (create-record-instance-by-lists proc attribute-names attribute-definitions #:optional (required-attribute-names attribute-names))
  "ATTRIBUTE-NAMES and REQUIRED-ATTRIBUTE-NAMES are mangled.  ATTRIBUTE-DEFINITIONS is not (yet)."
  (let* ((attribute-definitions (alist->hash-table attribute-definitions))
         (constructor-args (map (cut hash-ref-mangle-or-die attribute-definitions <> (lambda (key) (if (member key required-attribute-names) (error key) #f)))
                               attribute-names)))
    (apply proc constructor-args)))

(define-public (deserialize-record-instance type-name attributes)
  (cond
    ((eq? type-name 'type_decl) (create-record-instance-by-lists type-decl '(name type scpe srcp chain) attributes '(name type chain)))
    (else (cons type-name attributes))))
