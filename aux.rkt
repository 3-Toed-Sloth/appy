#lang racket
;; Aux - helper routines that do not fit anywhere else

(provide box-swap! string-trim-length)

;; no contract for box-swap! since this is a performance primtitive
(define (box-swap! box f . args)
  (let loop ()
    (let* ([old (unbox box)]
           [new (apply f old args)])
      (if (box-cas! box old new)
          new
          (loop)))))

(define/contract (string-trim-length str len)
  (-> string? exact-positive-integer? any)
  (define s (string-trim str))
  (cond
    ((< len 6) (substring s 0 len))
    ((> (string-length s) (- len 6))
     (string-append (substring s 0 (- len 6)) " [...]"))
    (else s)))

(module+ test
  (require rackunit)

  (check-equal? (string-trim-length "This is a somewhat long test string." 20) "This is a some [...]")
  (check-equal? (string-trim-length "This is a somewhat long test string." 200)  "This is a somewhat long test string.")
  (check-equal? (string-trim-length "This is a somewhat long test string." 1) "T")
  (check-exn exn:fail:contract? (lambda () (string-trim-length "hello world" 0)))

  )