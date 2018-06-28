#lang racket/base
(require racket/sequence)
(require racket/stream)

(struct amb-fail ())
(provide amb-fail?)

(define fail (make-parameter amb-fail))

(define-syntax amb
  (syntax-rules ()
    [(_) ((fail))]
    [(_ e0 e1 ...)
     (let/cc escape
       (let ([fail-prev (fail)])
         (let/cc fail-current
           (fail fail-current)
           (escape e0))
         (fail fail-prev))
       (amb e1 ...))]))
(provide amb)

(define (call-with-amb th)
  (parameterize ([fail amb-fail])
    (th)))
(provide call-with-amb)

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
        (parameterize ([fail (λ () (return (amb-fail)))])
          (yield (th))
          (amb))))
    (in-stream
     (let loop ()
       (let ([result (gen)])
         (if (amb-fail? result)
             empty-stream
             (stream-cons result (loop))))))))
(provide in-amb)

(define (amb-clear!)
  (fail amb-fail))
(provide amb-clear!)
