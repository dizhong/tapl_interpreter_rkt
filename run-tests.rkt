#lang racket
(require "typechecker.rkt")
(require "interpreter.rkt")
;(require "utilities.rkt")
(provide interp-tests test-typecheck)

;ripped from compiler class utilities.rkt

(define standard 279923)

(define (read-program path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string in ~s" path))
  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))
  (define input-prog
    (call-with-input-file path
      (lambda (f)
        `(program . ,(for/list ([e (in-port read f)]) e)))))
  (when debug-state
    (printf "read program:\n~s\n\n" input-prog))
  input-prog)

(define debug-state #f)

(define (debug label val)
  ;(if debug-state
      (begin
        (newline)
	(printf "~a:\n" label)
	(pretty-print val)
	(newline)))
      ;(void)))

(define test-typecheck 
  (lambda (tcer exp)
    (if (eq? tcer #f)
        exp
        (let ([res (with-handlers ([exn:fail? (lambda (e) #f)]) (tcer exp))])
          (match res
           [#f #f]
           [`(program ,elts ...) res]
           [else exp])))))

(define (check-tests name typechecker interp)
  (lambda (test-name)
    (newline) (newline)
    (debug "** checking for test " test-name)
    (define input-file-name (format "tests/~a.in" test-name))
    (define program-name (format "tests/~a.rkt" test-name))
    (define sexp (read-program program-name))
;   (debug "program:" sexp)
    (define type-error-expected (file-exists? (format "tests/~a.tyerr" test-name)))
    (define tsexp (test-typecheck typechecker sexp))
    
    (cond
     [(and type-error-expected tsexp)
      (error (format "expected type error in interpreter '~a',
                     but no error raised by typechecker" name))]
     [type-error-expected 'expected-type-error]
     [tsexp
      (let ([result (if (file-exists? input-file-name)
                        (with-input-from-file input-file-name (lambda () (interp tsexp)))
                        (interp tsexp))])
        (if (equal? result standard)
            (display (format "test ~a passed" program-name))
            (error (format "wrong result ~a; expecting 279923" result))))]
     [else (error (format "unexpected type error raised at '~a'" test-name))])))

;; The interp-tests function takes a name (a string), a typechecker
;; (see the comment for check-tests), the interpreter, a test family
;; name (a string), and a list of test numbers, and runs the test and
;; the interpreter to check whether the interpreter is correct.
;; 
;; This function assumes that the subdirectory "tests" has a bunch of
;; Scheme programs whose names all start with the family name,
;; followed by an underscore and then the test number, ending in
;; ".rkt". Also, for each Scheme program there is a file with the same
;; number except that it ends with ".in" that provides the input for
;; the Scheme program. If any program should not pass typechecking,
;; then there is a file with the name number (whose contents are
;; ignored) that ends in ".tyerr".

(define (interp-tests name typechecker interp test-family test-nums)
  (define checker (check-tests name typechecker interp))
  (for ([test-name (map (lambda (n) (format "~a_~a" test-family n)) 
			test-nums)])
       (checker test-name)
       ))


;Run tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interp-tests "interpreter"
              (typecheck '() '())
              (value-of (lambda (y) (error 'value-of "unbound variable ~s" y)) '())
              "c9"
              (range 1 9))