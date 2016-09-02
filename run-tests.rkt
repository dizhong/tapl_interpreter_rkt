#lang racket
;(require "typechecker.rkt")
;(require "interpreter.rkt")
;(require "utilities.rkt")
(provide interp-tests test-typecheck)

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

;; The check-passes function takes a compiler name (a string), a
;; typechecker (see below), a description of the passes (see below),
;; and an initial interpreter to apply to the initial expression, and
;; returns a function that takes a test name and runs the passes and
;; the appropriate interpreters to test the correctness of all the
;; passes. This function assumes there is a "tests" subdirectory and a
;; file in that directory whose name is the test name followed by
;; ".rkt". Also, there should be a matching file with the ending ".in"
;; that provides the input for the Scheme program. If any program
;; should not pass typechecking, then there is a file with the name
;; number (whose contents are ignored) that ends in ".tyerr".
;;
;; The description of the passes is a list with one entry per pass.
;; An entry is a list with three things: a string giving the name of
;; the pass, the function that implements the pass (a translator from
;; AST to AST), and a function that implements the interpreter (a
;; function from AST to result value).
;;
;; The typechecker is a function of exactly one argument that EITHER
;; raises an error using the (error) function when it encounters a
;; type error, or returns #f when it encounters a type error. 

(define (check-passes name typechecker interp)
  (lambda (test-name)
    (debug "** checking for test " test-name)
    (define input-file-name (format "tests/~a.in" test-name))
    (define program-name (format "tests/~a.rkt" test-name))
    (define sexp (read-program program-name))
;   (debug "program:" sexp)
    (define type-error-expected (file-exists? (format "tests/~a.tyerr" test-name)))
    (define tsexp (test-typecheck typechecker sexp))
    
    (cond
     [(and type-error-expected tsexp)
      (error (format "expected type error in interpreter '~a', but no error raised by typechecker" name))]
     [type-error-expected 'expected-type-error]
     [tsexp  (if (file-exists? input-file-name)
                 (with-input-from-file input-file-name (lambda () (interp tsexp)))
                 (interp tsexp))]
     [else (error (format "unexpected type error raised at '~a'" test-name))])))

;(define (compile passes)
;  (let ([prog-file-name (vector-ref (current-command-line-arguments) 0)])
;    ((compile-file passes) prog-file-name)))

;; The interp-tests function takes a compiler name (a string), a
;; typechecker (see the comment for check-passes) a description of the
;; passes (ditto) a test family name (a string), and a list of test
;; numbers, and runs the compiler passes and the interpreters to check
;; whether the passes correct.
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
  (define checker (check-passes name typechecker interp))
  (for ([test-name (map (lambda (n) (format "~a_~a" test-family n)) 
			test-nums)])
       (checker test-name)
       ))