#lang racket
(require "utilities.rkt")
;(require "run-tests.rkt")
(provide typecheck)

;Typecheck
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(define failure-result (error "given key not find in ref store"))

(define typecheck
  (lambda (env ref)
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
           [`(,T1 + ,T2) ;sum
            ;(pretty-print ((typecheck env ref) a))
            (match ((typecheck env ref) a)
              [`(,arg -> ,res) ;in branches check if left right correspond to T1 T2
               (match a
                 [`(inl ,t1) (if (equal? arg T1) T1 (error "inl branch type wrong"))]
                 [`(inr ,t2) (if (equal? arg T2) T2 (error "inr branch type wrong"))])]
              [`,res ;in t0 check if type is actually one of T1 T2
               (if (or (equal? res T1) (equal? res T2))
                   T
                   (error 'typecheck "'ascribe' expects an ~a" T))])]
           [`,T   ;I think there is a better way than this. first line of if 07/28
            (if (equal? ((typecheck env ref) a) T)
                T
                (error 'typecheck "'ascribe' expects an ~a" T))])];ascribe
         ;why when using match T gets transformed to whatever a's type is?
        [`(,t . 1) ;(pretty-print (format "pair.1: ~a" exp))
         (match ((typecheck env ref) t)
           [`(Pair ,t1 ,t2) t1])] ;pair (in match returns a pair of types) 07/12
        [`(,t . 2)
         (match ((typecheck env ref) t)
           [`(Pair ,t1 ,t2) t2])] ;pair
        [`(pair ,t1 ,t2) `(Pair ,((typecheck env ref) t1) ,((typecheck env ref) t2))] ;pair
        [`{record (,l1 = ,v1) ...} ;based on vector in P423 07/12
         ;(pretty-print "record cf")
         `{Record ,@(map (lambda (x y) `(,x = ,y))
                  l1
                  (map (lambda (x) ((typecheck env ref) x)) v1))}] ;record cf
        [`(record-ref (,records ,lj)) ;based on vector-ref in P423 07/12
         ;(pretty-print (format "'record-ref: ~a"
         ;                      (get-record ((typecheck env ref) records) lj)))
         (get-record ((typecheck env ref) records) lj)] ;record
        [`(inl ,t) ((typecheck env ref) t)] ;sum really doubt if these two are right
        [`(inr ,t) ((typecheck env ref) t)] ;sum +
        [`(case ,v of (,t1 as ,sumT1) or (,t2 as ,sumT2))
         ;(pretty-print ((typecheck env ref) t1))
         (let ([T0 ((typecheck env ref) v)])
           (let ([T1 ((typecheck env ref) `(,t1 as ,sumT1))]);this is shaky?
             (let ([T2 ((typecheck env ref) `(,t2 as ,sumT2))]);this is shaky?
               (if (equal? T0 `(,T1 + ,T2))
                   (last ((typecheck env ref) t1))
                   ;both branches return same b this is shaky?
                   (error (format "sum needs matching types ~a, ~a+~a" T0 T1 T2))))))] ;sum
        [`(loc ,t) ((typecheck env ref) t)] ;ref loc
        [`(ref ,t)
         `(Ref ,((typecheck env ref) t))] ;ref creation + boxing
        [`(! ,t) ((typecheck env ref) t)] ;ref deref
        [`(,t1 := ,t2)
         (if (equal? ((typecheck env ref) t1) ((typecheck env ref) t2))
             ((typecheck env ref) t1)
             (error (format "reference assignment expects matching types ~a ~a" t1 t2)))]
        ;ref ass
        [`(zero? ,e)
         (match ((typecheck env ref) e)
           ['Integer 'Boolean] ;think here should return a boolean instead of int 07/12
           [else (error 'typecheck "'zero?' expects an Integer ~a" e)])] ;zero?
        [`(sub1 ,e)
         (match ((typecheck env ref) e)
           ['Integer 'Integer]
           [else (error 'typecheck "'sub1' expects an Integer ~a" e)])] ;sub1
        [`(* ,e1 ,e2)
         (match ((typecheck env ref) e1)
           ['Integer (match ((typecheck env ref) e2)
                       ['Integer 'Integer]
                       [else (error 'typecheck "'*' expects an Integer ~a" e2)])]
           [else (error 'typecheck "'*' expects an Integer ~a" e1)])];*
        [`(lambda (,args) ,body) ;basically copied from the book
         (match args 
           [`[,arg : ,type]
           (let ([new-env (cons (cons arg type) env)])
             (let ([bodyT ((typecheck new-env ref) body)])
               ;(cond [(equal? rT bodyT)
                      ;(pretty-print bodyT)
                      ;(pretty-print `(,@(map cadr l-env) -> ,rT))
                      `(,type -> ,bodyT)))])]
                     ;[else (error "mismatch in return type" bodyT rT)])))])] ;lambda
        [`(let ([,x ,e]) ,body) ;(pretty-print "let")
         (define T ((typecheck env ref) e)) 
         (define new-env (cons (cons x T) env))
         ;(pretty-print "got to let")
         ((typecheck new-env ref) body)] ;let
        [`(if ,cnd ,thn ,els)
         (match ((typecheck env ref) cnd)
           (pretty-print (format "if: ~a" ((typecheck env ref) cnd))) ;07/13
           ['Boolean (let ([thnT ((typecheck env ref) thn)]
                           [elsT ((typecheck env ref) els)])
                       (if (eq? thnT elsT)
                           thnT
                           (error 'typecheck "'if' requires matching types ~a" thnT)))]
           [else (error 'typecheck "'if' expects a Boolean ~a" cnd)])] ;if
        [`(program ,expr) ((typecheck env ref) expr)]
        [`(,function ,args) 
         (let ([f-t ((typecheck env ref) function)])
           (match f-t
             ;(pretty-print (format "rator rand: ~a" f-t) ;07/12
             [`(,arg-types -> ,return-type)
              (let ([passed-arg-types ((typecheck env ref) args)])
                ;(pretty-print arg-types) (pretty-print passed-arg-types) ;07/12
                (if (not (equal? arg-types passed-arg-types))
                    (error "incorrect types in argument(s) to" function)
                    return-type))]
             ;07/12/16 if want to revert back to multiple args, change to
             ;(map (typecheck env ref) args) and add a few "..."s here and there
             [else (error "something went wrong in function ~a" function)]))];app
        [else (error (format "nothing matched: ~a" exp))]))))