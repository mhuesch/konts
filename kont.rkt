#lang racket
(require redex
         redex/tut-subst)

(define-language L
  [e ::=
     (+ e e)
     (e e)
     (abort e)
     x
     v]
  [v ::=
     (λ x e)
     number
     call/cc]
  [E ::=
     hole
     (+ E e)
     (+ v E)
     (E e)
     (v E)]
  [x variable-not-otherwise-mentioned])

(define red
  (reduction-relation
   L
   (--> (in-hole E (+ number_1 number_2))
        (in-hole E ,(+ (term number_1) (term number_2))))
   (--> (in-hole E ((λ x e) v))
        (in-hole E (subst x v e)))
   (--> (in-hole E (call/cc v))
        (in-hole E (v (λ x (abort (in-hole E x)))))
        (fresh x))
   (--> (in-hole E (abort e))
        e)))
                
                
(define x? (redex-match? L x))

(define-metafunction L
  subst : x v e -> e
  [(subst x v e)
   ,(subst/proc x? 
                (list (term x))
                (list (term v))
                (term e))])

#|
(traces red
        (term (+ (+ (+ 1 2)
                    (+ 3 4))
                 (+ (+ 5 6)
                    (+ 7 8)))))
|#

(traces red
        (term (+ 1
                 (+ 2
                    (call/cc (λ k
                               1))))))
