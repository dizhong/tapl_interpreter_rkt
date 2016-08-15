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
07.28.16 Need to update sum and ascription maybe.
07.29.16 So in Sum type. Should the input just be a direction of which type to retrieve
         or should it be something you get into lambda functions?
08.01.16 Need furthur modification corresponds to the change in typechecker for sum
08.15.16 Okay references.
         How do I test Sequencing?
|#

;Interpreter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define value-of
  (lambda (exp env ref)
    (match exp
      [(? number?) exp] ;nat
      [(? symbol?) (env exp)] ;var huh so it's different from what we do in typechecker
      [(? boolean?) exp] ;bool
      ;[(? string?) exp] ;well I expect I should do char first...
      [`(unit ,a) exp] ;unit type????
      [`(zero? ,e) (zero? (value-of e env ref))] 
      [`(sub1 ,e) (sub1 (value-of e env ref))]
      [`(* ,e1 ,e2) (* (value-of e1 env ref) (value-of e2 env ref))]
      [`(lambda ((,id : ,type)) ,body)
       (lambda (x)     ;abstr?
         (value-of body (lambda (var)
                          (if (eqv? id var)
                              x
                              (env var))) ref))]
      [`(let ([,v ,e]) ,y) (pretty-print "got to let")
       (value-of y (lambda (var_let)      ;let
                     (if (eqv? v var_let)
                         (value-of e env)
                         (env var_let))) ref)]
      [`(if ,cnd ,thn ,els)
       (if (value-of cnd env ref) (value-of thn env ref) (value-of els env ref))] ;if
      ;[`(,a ; ,b) ((lambda (x) `(unit (value-of b env))) (value-of a env))] ;sequencing
      [`(,a as ,T) (value-of a env ref)] ;ascribe
      [`(,t . 1)
       (match (value-of t env ref)
         [`(pair ,t1 ,t2) t1])] ;pair
      [`(,t . 2) ;(pretty-print "??")
       (match (value-of t env ref)
         [`(pair ,t1 ,t2) t2])] ;pair
      [`(pair ,t1 ,t2) `(pair ,(value-of t1 env ref) ,(value-of t2 env ref))] ;pair 
      [`{record (,l1 = ,v1) ...} ;(pretty-print "got to record cf") (pretty-print exp)
       `{record ,@(map (lambda (x y) `(,x = ,y))
                l1
                (map (lambda (x) (value-of x env ref)) v1))}];record creation form 
      [`(record-ref (,records ,lj))
       (pretty-print (format "record-ref: ~a" records))
       (get-record (value-of records env ref) lj)] ;record
      ;originaly messes up `(,rator ,rand) so changed from `(,records ,lj) to current
      [`(inl ,t) `(inl ,(value-of t env ref))] ;sum creation
      [`(inr ,t) `(inr ,(value-of t env ref))] ;sum creation
      [`(case ,v of (,e1 as ,t) or (,e2 as ,t))
       (match (value-of v env ref)
         [`(inl ,v^) (value-of `(,e1 ,v^) env ref)]
         [`(inr ,v^) (value-of `(,e2 ,v^) env ref)])] ;sum
      ;ref is an association list, like the env in typechecker. use lookup to find stuff
      [`(loc ,v) v] ;reference location
      [`(ref ,t) (let ([l (gensym 'loc)])
                   (cons (cons l (value-of t env ref)) ref))] ;reference creation 
      [`(! ,t) (lookup (value-of t env ref) ref)] ;reference dereference
      [`(,t1 := ,t2) (ass-ref (value-of t1 env ref)
                              (value-of t2 env ref)
                              ref)] ;reference assignment 
      [`(,rator ,rand)
       ;(pretty-print (format "rator rand: ~a" exp))
       ((value-of rator env ref) (value-of rand env ref))] ;app
      [else (error (format "nothing matched: ~a" exp))]
      )))