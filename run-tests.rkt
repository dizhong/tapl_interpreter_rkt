#lang racket
(require "typechecker.rkt")
(require "interpreter.rkt")
(require "utilities.rkt")

(define (read-program path)
  (unless (or (string? path) (path? path))
    (error 'read-program "expected a string in ~s" path))
  (unless (file-exists? path)
    (error 'read-program "file doesn't exist in ~s" path))
  (define input-prog
    (call-with-input-file path
      (lambda (f)
        `(program . ,(for/list ([e (in-port read f)]) e)))))
  input-prog)

(define test-typecheck 
  (lambda (tcer exp)
    (if (eq? tcer #f) exp
        (let ([res 
               (with-handlers ([exn:fail?
                                (lambda (e) #f)])
                 (tcer exp))])
          (match res
           [#f #f]
           [`(program ,elts ...) res]
           [else exp])))))

(define (check-passes name typechecker passes initial-interp)
  (lambda (test-name)
    (define input-file-name (format "tests/~a.in" test-name))
    (define program-name (format "tests/~a.rkt" test-name))
    (define sexp (read-program program-name))
    (define type-error-expected (file-exists? (format "tests/~a.tyerr" test-name)))
    (define tsexp (test-typecheck typechecker sexp))
    
    (cond
     [(and type-error-expected tsexp)
      (error (format "expected type error in compiler '~a', but no error raised by typechecker" name))]
     [type-error-expected 'expected-type-error]
     [tsexp 
      (let loop ([passes passes] [p tsexp]
                 [result (if (file-exists? input-file-name)
                             (with-input-from-file input-file-name
                               (lambda () (initial-interp tsexp)))
                             (initial-interp tsexp))])
        (cond [(null? passes) result]
              [else
               (match (car passes)
                 [`(,pass-name ,pass ,interp)
                  (define new-p (pass p))
                  (cond [interp
                         (let ([new-result
                                ;; if there is an input file with the same name
                                ;; as this test bing current-input-port to that
                                ;; file's input port so that the interpreters
                                ;; can use it as test input.
                                (if (file-exists? input-file-name) 
                                    (with-input-from-file input-file-name
                                      (lambda () (interp new-p)))
                                    (interp new-p))])
                           (cond [result
                                  (cond [(equal? result new-result)
                                         (loop (cdr passes) new-p new-result)]
                                        [else
                                         (display "in program")(newline)
                                         (pretty-print new-p)(newline)
                                         (error (format "differing results in compiler '~a' pass '~a', expected ~a, not" name pass-name result )
                                                new-result)])]
                                 [else ;; no result to check yet
                                  (loop (cdr passes) new-p new-result)]))]
                        [else
                         (loop (cdr passes) new-p result)])])]))
      ]
     [else (error (format "unexpected type error raised by compiler '~a'" name))])))