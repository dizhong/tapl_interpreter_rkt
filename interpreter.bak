#lang racket
(require "utilities.rkt")
(provide value-of)

#|
06.29.16 This is value-of, not small step. to update: pair, record, sum. testing.
07.06.16 ??? sum. ugh.
07.07.16 ??? todo-list: typechecking, string, type alias
07.08.16 uh, stream-line testing, ty, & others; change all a/e/t to one thing
07.11.16 ???? lambda??? pair record sum all too simple?
07.12.16 uh. buncha stuff. basically tryta finish+test typechecker; soo when GRE?!
         think I added ascribe today
         modified typechecking for lambda
         changed record (to record-ref, added label{record ...}, stuff like that)
         do I add a label to sum type tho???????????????????
07.13.16 ugh. Gotten to the point where updating one thing results into a million
         errors. Sum & lambda.
         WHY DID'T I REALIZE TAPL HAS ALL THE TYPE CHECKING RULES
07.14.16 Moved typechecker and helpers to a new file.
|#

;Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of
  (lambda (exp env)
    (match exp
      [(? number?) exp] ;nat
      [(? symbol?) (env exp)]
      ;var huh so it's different from what we do in typechecker
      [(? boolean?) exp] ;bool
      ;[(? string?) exp] ;well I expect I should do char first...
      [`(unit ,a) exp] ;unit type????
      [`(zero? ,e) (zero? (value-of e env))] 
      [`(sub1 ,e) (sub1 (value-of e env))]
      [`(* ,e1 ,e2) (* (value-of e1 env) (value-of e2 env))]
      [`(lambda ((,id : ,type)) : ,rT ,body)
       (lambda (x)     ;abstr?
         (value-of body (lambda (var)
                          (if (eqv? id var)
                              x
                              (env var)))))]
      [`(let ([,v ,e]) ,y) (pretty-print "got to let")
       (value-of y (lambda (var_let)      ;let
                     (if (eqv? v var_let)
                         (value-of e env)
                         (env var_let))))]
      [`(if ,cnd ,thn ,els)
       (if (value-of cnd env) (value-of thn env) (value-of els env))] ;if
      ;[`(,a ; ,b) ((lambda (x) `(unit (value-of b env))) (value-of a env))] ;sequencing
      [`(,a as ,T) (value-of a env)] ;ascribe
      [`(,t . 1)
       (match (value-of t env)
         [`(pair ,t1 ,t2) t1])] ;pair
      [`(,t . 2) ;(pretty-print "??")
       (match (value-of t env)
         [`(pair ,t1 ,t2) t2])] ;pair
      [`(pair ,t1 ,t2) `(pair ,(value-of t1 env) ,(value-of t2 env))] ;pair 
      [`{record (,l1 = ,v1) ...} ;(pretty-print "got to record cf") (pretty-print exp)
       `{record ,@(map (lambda (x y) `(,x = ,y))
                l1
                (map (lambda (x) (value-of x env)) v1))}];record creation form 
      [`(record-ref (,records ,lj))
       (pretty-print (format "record-ref: ~a" records))
       (get-record (value-of records env) lj)] ;record
      ;originaly messes up `(,rator ,rand) so changed from `(,records ,lj) to current
      [`(inl ,t) `(inl ,(value-of t env))] ;sum
      [`(inr ,t) `(inr ,(value-of t env))] ;sum
      [`(case ,t of (inl (lambda (,x1) : ,rT1 ,t1))
                 or (inr (lambda (,x2) : ,rT2 ,t2))) ;also want to shrink this
       (match (value-of t env)
         [`(inl ,v) (value-of `((lambda (,x1) : ,rT1 ,t1) ,v) env)]
         [`(inr ,v) (value-of `((lambda (,x2) : ,rT2 ,t2) ,v) env)])] ;sum
      ;OMG can't believe I stuck at this for like 2 hours. Differentiate inl inr!!!!!
      ;why doing (lambda ((,x1 : t1)) ...) wouldn't work? 07/13
      ;[`({,t1 ,t2 ...} . ,n)
      ;  (let ([a (value-of t1 env)])
      ;       (if (= n 1) a (value-of ..xs `(t2 . ,(- n 1)) env)))] ;tuple/projection
      ;[`{(,f1 : ,v1) ...} ] ;variant types????
      ;[`(fix ,t) ] ;general recursion ????
      ;[`(nil ,T) '()] ;list
      ;[`(isnil ,e) (let ([a (value-of e env)]) (if (eqv? '() a) #t #f))] ;list
       ;list
      [`(,rator ,rand)
       (pretty-print (format "rator rand: ~a" exp))
       ((value-of rator env) (value-of rand env))] ;app
      [else (error (format "nothing matched: ~a" exp))]
      )))