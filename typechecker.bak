#lang racket
(require "interpreter.rkt")
(require "utilities.rkt")
(provide typecheck)

#|
07/11 assuming args is arg. You can only curry lambdas functions
07.14.16 moved typechecker here. Should've done it for the compiler class.
         would've made things so much easier (probably not).
         why when using match T gets transformed to whatever a's type is?
07.15.16 Jeremy pointed out and fixed, like 3 or something cases
|#

;Typecheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define typecheck
  (lambda (env)
    (lambda (exp)
      (match exp
        [(? number?) 'Integer] ;nat
        [(? boolean?) 'Boolean] ;bool
        [(? symbol?)
         ;(pretty-print (format "symbol?: ~a lookup: ~a" env (lookup exp env)))
         (lookup exp env)]
         ;string
        [`(unit ,a) 'Unit] ;unit?
        [`(,a as ,T)
         ;(pretty-print (format "as ~a" T))
         (match T
           [`(,T1 + ,T2)
            (if (or (eqv? ((typecheck env) a) T1) (eqv? ((typecheck env) a) T2))
                T
                (error 'typecheck "'ascribe' expects an ~a" T))]
           [`,T   ;I think there is a better way than this. first line of if
            (if (eqv? ((typecheck env) a) T)
                T
                (error 'typecheck "'ascribe' expects an ~a" T))];ascribe
         ;why when using match T gets transformed to whatever a's type is?
        [`(,t . 1) (pretty-print (format "pair.1: ~a" exp))
         (match ((typecheck env) t)
           [`(Pair ,t1 ,t2) t1])] ;pair (in match returns a pair of types) 07/12
        [`(,t . 2)
         (match ((typecheck env) t)
           [`(Pair ,t1 ,t2) t2])] ;pair
        [`(pair ,t1 ,t2) `(Pair ,((typecheck env) t1) ,((typecheck env) t2))] ;pair
        [`{record (,l1 = ,v1) ...} ;based on vector in P423 07/12
         ;(pretty-print "record cf")
         `{Record ,@(map (lambda (x y) `(,x = ,y))
                  l1
                  (map (lambda (x) ((typecheck env) x)) v1))}] ;record cf
        [`(record-ref (,records ,lj)) ;based on vector-ref in P423 07/12
         (get-record ((typecheck env) records) lj)] ;record
        [`(inl ,t as (,T1 + ,T2)) ((typecheck env) t)]
         ;sum really doubt if these two are right
        [`(inr ,t as (,T1 + ,T2)) ((typecheck env) t)] ;sum +
        [`(case ,v of ,t1 or ,t2)
         (let ([T0 ((typecheck env) v)])
           (let ([T1 ((typecheck env) t1)])
             (let ([T2 ((typecheck env) t2)])
               (if (and (eqv? T0 T1) (eqv? T1 T2))
                   T0
                   (error (format "v and t of sum need matching cases ~a" exp))))))] ;sum
        [`(zero? ,e)
         (match ((typecheck env) e)
           ['Integer 'Boolean] ;think here should return a boolean instead of int 07/12
           [else (error 'typecheck "'zero?' expects an Integer ~a" e)])] ;zero?
        [`(sub1 ,e)
         (match ((typecheck env) e)
           ['Integer 'Integer]
           [else (error 'typecheck "'sub1' expects an Integer ~a" e)])] ;sub1
        [`(* ,e1 ,e2)
         (match ((typecheck env) e1)
           ['Integer (match ((typecheck env) e2)
                       ['Integer 'Integer]
                       [else (error 'typecheck "'*' expects an Integer ~a" e2)])]
           [else (error 'typecheck "'*' expects an Integer ~a" e1)])];*
        [`(lambda (,args) ,body) ;basically copied from the book
         (match args 
           [`[,arg : ,type]
           (let ([new-env (cons (cons arg type) env)])
             (let ([bodyT ((typecheck new-env) body)])
               ;(cond [(equal? rT bodyT)
                      ;(pretty-print l-env)
                      ;(pretty-print `(,@(map cadr l-env) -> ,rT))
                      `(,type -> ,rT)))])]
                     ;[else (error "mismatch in return type" bodyT rT)])))])] ;lambda
        [`(let ([,x ,e]) ,body) ;(pretty-print "let")
         (define T ((typecheck env) e)) 
         (define new-env (cons (cons x T) env))
         ;(pretty-print "got to let")
         ((typecheck new-env) body)] ;let
        [`(if ,cnd ,thn ,els)
         (match ((typecheck env) cnd)
           ;(pretty-print (format "if: ~a" ((typecheck env) cnd))) ;07/13
           ['Boolean (let ([thnT ((typecheck env) thn)] [elsT ((typecheck env) els)])
                       (if (eq? thnT elsT)
                           thnT
                           (error 'typecheck "'if' requires matching types ~a" thnT)))]
           [else (error 'typecheck "'if' expects a Boolean ~a" cnd)])] ;if
        [`(,function ,args) 
         (let ([f-t ((typecheck env) function)])
           (match f-t
             ;(pretty-print (format "rator rand: ~a" f-t) ;07/12
             [`(,arg-types -> ,return-type)
              (let ([passed-arg-types ((typecheck env) args)])
                ;(pretty-print arg-types) (pretty-print passed-arg-types) ;07/12
                (if (not (equal? arg-types passed-arg-types))
                    (error "incorrect types in argument(s) to" function)
                    return-type))]
             ;07/12/16 if want to revert back to multiple args, change to
             ;(map (typecheck env) args) and add a few "..."s here and there
             [else (error "something went wrong in function ~a" function)]))];app
        [else (error (format "nothing matched: ~a" exp))]))))