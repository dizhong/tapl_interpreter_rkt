#lang racket
(provide get-record get-record-helper lookup)

#|
07.14.16 moved helpers here.
07.15.16 trying to add testing-related stuff
|#

;Helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define get-record ;record helper 1
  (lambda (records lj)
    (match records
      ;(pretty-print (format "get-record: ~a" records))
      [`{record (,l1 = ,v1) ...} (get-record-helper l1 v1 lj)]
      [`{Record (,l1 = ,v1) ...} (get-record-helper l1 v1 lj)])))

(define get-record-helper ;record helper 2
  (lambda (l1 v1 lj)
    (cond
      [(equal? l1 '()) (error "no such record")]
      [(equal? (car l1) lj) (pretty-print l1) (car v1)]
      [else (get-record-helper (cdr l1) (cdr v1) lj)])))

(define lookup ;grabbed from P423 utilities.rkt
  (lambda (x ls)
    (cond [(null? ls)
	   (error "lookup failed for " x)]
	  [(and (pair? (car ls)) (eq? x (car (car ls))))
	   (cdr (car ls))]
	  [(and (mpair? (car ls)) (eq? x (mcar (car ls))))
	   (mcdr (car ls))]
	  [else 
	   (lookup x (cdr ls))])))

;Running tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

