#lang racket

(define abortk #f)
(define (abort thunk)
  (unless abortk (error "world hasn't started yet!"))
  (abortk thunk))

(define coroutines #f)

(define (coroutine routine)
  (cond
    [(false? coroutines)
     ((call/cc (λ (k)
                 (set! abortk k)
                 (set! coroutines null)
                 (abort routine))))]
    [else
     (set! coroutines (cons (λ () (abort routine)) coroutines))]))

(define (yield)
  (cond
    [(false? coroutines) (error "world hasn't started yet!")]
    [(empty? coroutines) (void)]
    [(pair? coroutines)
     (define abortRoutine (car coroutines))
     (let/cc k
       (set! coroutines (append (cdr coroutines) (list (λ () (k (void))))))
       (abortRoutine))]
    [else
     (error "bad coroutines list")]))

; abort : (-> α) -> β

(coroutine (λ ()
             (displayln 0)
             (for ([i (range 1 5)])
               (coroutine (λ ()
                            (displayln i)
                            (displayln (yield))
                            (displayln (format "~s-2" i))
                            (yield))))
             (displayln 5)
             (yield)
             (displayln 6)
             (yield)))