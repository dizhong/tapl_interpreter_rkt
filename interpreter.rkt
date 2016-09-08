#lang racket
(require "utilities.rkt")
(provide value-of)

;Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of
  (lambda (env ref)
    (lambda (exp)
    (match exp
      [(? number?) exp] ;nat
      [(? symbol?) (env exp)] ;var huh so it's different from what we do in typechecker
      [(? boolean?) exp] ;bool
      ;[(? string?) exp] ;well I expect I should do char first...
      [`(unit ,a) exp] ;unit type????
      [`(zero? ,e) (zero? ((value-of env ref) e))] 
      [`(sub1 ,e) (sub1 ((value-of env ref) e))]
      [`(* ,e1 ,e2) (* ((value-of env ref) e1) ((value-of env ref) e2))]
      [`(lambda ((,id : ,type)) ,body)
       (lambda (x)     ;abstr?
         ((value-of (lambda (var)
                          (if (eqv? id var)
                              x
                              (env var))) ref) body))]
      [`(let ([,v ,e]) ,y) ;(pretty-print "got to let")
       ((value-of (lambda (var_let)      ;let
                     (if (eqv? v var_let)
                         ((value-of env ref) e)
                         (env var_let))) ref) y)]
      [`(if ,cnd ,thn ,els)
       (if ((value-of env ref) cnd) ((value-of env ref) thn) ((value-of env ref) els))]
      ;if
      ;[`(,a ; ,b) ((lambda (x) `(unit (value-of b env))) (value-of a env))] ;sequencing
      [`(,a as ,T) ((value-of env ref) a)] ;ascribe
      [`(,t . 1)
       (match ((value-of env ref) t)
         [`(pair ,t1 ,t2) t1])] ;pair
      [`(,t . 2) ;(pretty-print "??")
       (match ((value-of env ref) t)
         [`(pair ,t1 ,t2) t2])] ;pair
      [`(pair ,t1 ,t2) `(pair ,((value-of env ref) t1) ,((value-of env ref) t2))] ;pair 
      [`{record (,l1 = ,v1) ...} ;(pretty-print "got to record cf") (pretty-print exp)
       `{record ,@(map (lambda (x y) `(,x = ,y))
                       l1
                       (map (lambda (x) ((value-of env ref) x)) v1))}];record creation form 
      [`(record-ref (,records ,lj))
       ;(pretty-print (format "record-ref: ~a" records))
       (get-record ((value-of env ref) records) lj)] ;record
      ;originaly messes up `(,rator ,rand) so changed from `(,records ,lj) to current
      [`(inl ,t) `(inl ,((value-of env ref) t))] ;sum creation
      [`(inr ,t) `(inr ,((value-of env ref) t))] ;sum creation
      [`(case ,v of ((inl ,e1) as ,t) or ((inr ,e2) as ,t))
       (match ((value-of env ref) v)
         [`(inl ,v^) ((value-of env ref) `(,e1 ,v^))]
         [`(inr ,v^) ((value-of env ref) `(,e2 ,v^))])] ;sum
      [`(loc ,t) ((value-of env ref) t)] ;reference location ???
      [`(ref ,t) (box ((value-of env ref) t))]
                 ;(let ([l (gensym 'loc)])
                 ;  (cons (cons l ((value-of env ref) t)) ref))] ;reference creation 
      [`(! ,t) (unbox ((value-of env ref)t))];(lookup ((value-of env ref) t) ref)] ;reference dereference
      [`(,t1 := ,t2) (set-box! ((value-of env ref) t1) ((value-of env ref) t2))]
                     ;(ass-ref ((value-of env ref) t1)
                     ;         ((value-of env ref) t2)
                     ;         ref)] ;reference assignment
      [`(program ,expr) ((value-of env ref) expr)]
      [`(,rator ,rand)
       ;(pretty-print (format "rator rand: ~a" exp))
       (((value-of env ref) rator) ((value-of env ref) rand))] ;app
      [else (error (format "nothing matched: ~a" exp))]
      ))))