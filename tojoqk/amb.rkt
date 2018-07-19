#lang racket/base
(require racket/sequence)
(require racket/stream)
(require racket/contract)

(module+ test
  (require rackunit)
  (require racket/function))

(struct amb-fail ())
(provide amb-fail?)

(define current-fail (make-parameter amb-fail))

(define-syntax amb
  (syntax-rules ()
    [(_) ((current-fail))]
    [(_ e0 e1 ...)
     (let/cc escape
       (let ([previous-fail (current-fail)])
         (let/cc current-fail*
           (current-fail current-fail*)
           (escape e0))
         (current-fail previous-fail))
       (amb e1 ...))]))
(provide amb)

(define (call-with-amb th)
  (parameterize ([current-fail amb-fail])
    (th)))
(provide/contract [call-with-amb (-> (-> any/c) any/c)])

(define (in-amb th)
  (let* ([return #f]
         [continue (λ (x) (void))]
         [yield
          (λ (obj)
            (let/cc k
              (set! continue k)
              (return obj)))])
    (define (gen)
      (let/cc k
        (set! return k)
        (continue (void))
        (parameterize ([current-fail (λ () (return (amb-fail)))])
          (yield (th))
          (amb))))
    (in-stream
     (let loop ()
       (let ([result (gen)])
         (if (amb-fail? result)
             empty-stream
             (stream-cons result (loop))))))))
(provide/contract [in-amb (-> (-> any/c) any/c)])

(module+ test
  (check-equal? (sequence->list (in-amb (thunk (amb 1 2 3)))) '(1 2 3))
  (check-equal? (sequence->list (in-amb (thunk
                                         (let* ([x (amb 1 2 3)]
                                                [y (amb 10 20 30)])
                                           (+ x y)))))
                '(11 21 31 12 22 32 13 23 33)))

(define (amb-clear!)
  (current-fail amb-fail))
(provide/contract [amb-clear! (-> void?)])
