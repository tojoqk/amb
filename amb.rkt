#lang racket

(struct amb-fail ())
(provide amb-fail?)

(define fail (make-parameter amb-fail))

(define-syntax amb
  (syntax-rules ()
    [(_) ((fail))]
    [(_ e0 e1 ...)
     (let/cc escape
       (let ([fail-prev (fail)])
         (let/cc fail-next
           (fail fail-next)
           (escape e0))
         (fail fail-prev))
       (amb e1 ...))]))
(provide amb)

(define-syntax in-amb
  (syntax-rules ()
    [(_ body body* ...)
     (let* ([return #f]
            [continue identity]
            [yield
             (λ (obj)
               (let/cc k
                 (set! continue k)
                 (return obj)))])
       (define (f)
         (let/cc k
           (set! return k)
           (continue (void))
           (parameterize ([fail (λ () (return (amb-fail)))])
             (yield (begin body body* ...))
             (amb))))
       (in-stream
        (let loop ()
          (let ([result (f)])
            (if (amb-fail? result)
                empty-stream
                (stream-cons result (loop)))))))]))
(provide in-amb)

(define (amb-clear!)
  (fail amb-fail))
(provide amb-clear!)
