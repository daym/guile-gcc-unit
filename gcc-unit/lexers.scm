;;; Copyright 2017 - 2017, Danny Milosavljevic
;;; SPDX-License-Identifier: BSD-3-Clause

(define-module (gcc-unit lexers)
  #:use-module (system base lalr)
  #:use-module (ice-9 textual-ports))

(define-syntax-rule (port-source-location port)
  (make-source-location (port-filename port)
                        (+ 1 (port-line port))
                        (+ 1 (port-column port))
                        (false-if-exception (ftell port))
                        #f))

(define-syntax-rule (return port category value)
  (make-lexical-token category (port-source-location port) value))

(define (is-newline? c) (string-contains "\n" (string c)))
(define (is-whitespace? c) (string-contains " \t\n" (string c)))
(define (is-colon? c) (string-contains ":" (string c)))
(define (is-at? c) (string-contains "@" (string c)))
(define (is-delimiter? c)
  (or (eof-object? c)
      (string-contains ":\n\t@ " (string c))))

(define (get-value port)
  (cond
    ((is-delimiter? (peek-char port))
     '())
    (else
     (cons (read-char port) ; consume char
           (get-value port)))))

(define (next-token port)
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c) '*eoi*)
     ((is-colon? c)
      (read-char port)
      (return port ': ':))
     ((is-at? c)
      (read-char port)
      (return port '@ '@))
     ((is-newline? c)
      (read-char port)
      (if (and (not (eof-object? (peek-char port))) (is-at? (peek-char port)))
        (return port 'def 'def)
        (next-token port)))
     ((is-whitespace? c)
      (read-char port)
      (next-token port))
     (else
      (return port 'value (list->string (get-value port)))))))

(define (skip-header-junk port)
  "Given a port, skips all the junk in front of the first '@' sign, or EOF - whichever comes first."
  (let loop ((c (peek-char port)))
    (if (and (not (eof-object? c)) (not (char=? #\@ c)))
      (begin
        (read-char port)
        (loop (peek-char port)))
      (if (not (eof-object? c))
        (unget-char port #\nl))))) ; Make sure that the next token is a def.

(define-public (make-lexer port)
  (skip-header-junk port)
  (lambda ()
    (next-token port)))
