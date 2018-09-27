#lang racket

(require (prefix-in aux: "aux.rkt"))

(provide stack% queue% datum% limited-stack%)

(define thread-safe-object%
  (class object%
    (field (data (box '())))
    
    (define/public (empty?)
      (null? (unbox data)))
     
    (define/public (get-as-list)
      (unbox data))

    (define/public (set-from-list li)
      (void (aux:box-swap! data
                           (λ (old) li))))

    (define/public (count)
      (length (unbox data)))

    (define/public (clear)
      (aux:box-swap! data
                     (λ (li) '())))
    (super-new)))

(define thread-safe-container%
  (class thread-safe-object%
    (inherit-field data)
   
    (define/public (remove-car!)
      (begin0
        (car (unbox data))
        (aux:box-swap! data
                       (λ (li) (cdr li)))))

    (define/public (peek)
      (car (unbox data)))
    
    (super-new)))

(define stack%
  (class thread-safe-container%
    (inherit remove-car!)
    (inherit-field data)
    
    (define/public (push datum)
      (void (aux:box-swap! data
                           (λ (stack) (cons datum stack)))))

    (define/public (pop)
      (remove-car!))
    
    (super-new)))

(define limited-stack%
  (class stack%
    (inherit count)
    (inherit-field data)
    (init-field maximum)

    (define/override (push datum)
      (if (< (count) maximum)
          (super push datum)
          (void (aux:box-swap! data
                               (λ (stack) (cons datum (reverse (cdr (reverse stack)))))))))

    (super-new)))
  

(define queue%
  (class thread-safe-container%
    (inherit-field data)
    (inherit remove-car!)

    (define/public (put datum)
      (void (aux:box-swap! data
                           (lambda (queue) (append queue (list datum))))))

    (define/public (get)
      (remove-car!))

    (super-new)))

(define datum%
  (class thread-safe-object%
    (inherit-field data)
    
    (define/public (put d)
      (void (aux:box-swap! data
                           (λ (datum) (list d)))))

    (define/public (get)
      (car (unbox data)))

    (super-new)))

(module+ test
  (require rackunit)

  (define a (make-object queue%))
  (define b (make-object stack%))
  (define c (make-object datum%))
  (define d (make-object limited-stack% 3))

  (check-true (send a empty?))
  (check-true (send b empty?))
  (check-true (send c empty?))
  
  (check-not-exn (lambda () (send a put 'first)))
  (check-not-exn (lambda () (send a put 'second)))
  (check-not-exn (lambda () (send a put 'third)))

  (check-not-exn (lambda () (send b push 'first)))
  (check-not-exn (lambda () (send b push 'second)))
  (check-not-exn (lambda () (send b push 'third)))

  (check-equal? (send a count) 3)
  (check-equal? (send b count) 3)

  (check-equal? (begin (send a set-from-list (send a get-as-list)) (send a get-as-list)) (send a get-as-list))
  (check-equal? (begin (send b set-from-list (send b get-as-list)) (send b get-as-list)) (send b get-as-list))
  
  (check-equal? (send a get) 'first)
  (check-equal? (send a get) 'second)
  (check-equal? (send a get) 'third)
  (check-exn exn:fail? (lambda () (send b get)))

  (check-equal? (send b pop) 'third)
  (check-equal? (send b pop) 'second)
  (check-equal? (send b pop) 'first)
  (check-exn exn:fail? (lambda () (send b get)))

  (check-equal? (send a get-as-list) '())
  (check-equal? (send b get-as-list) '())
  (check-equal? (send c get-as-list) '())

  (check-not-exn (lambda () (send c put 'hello)))
  (check-equal? (send c get) 'hello)
  (check-equal? (send c get-as-list) '(hello))
  (check-not-exn (lambda () (send c clear)))
  (check-true (send c empty?))

  (check-true (is-a? a thread-safe-object%))
  (check-true (is-a? a thread-safe-container%))
  (check-true (is-a? b thread-safe-object%))
  (check-true (is-a? b thread-safe-container%))
  (check-true (is-a? c thread-safe-object%))
  (check-false (is-a? c thread-safe-container%))

  (check-not-exn (lambda () (send a put 'test)))
  (check-not-exn (lambda () (send a clear)))
  (check-true (send a empty?))

  (check-not-exn (lambda () (send d push 'first)))
  (check-not-exn (lambda () (send d push 'second)))
  (check-not-exn (lambda () (send d push 'third)))
  (check-not-exn (lambda () (send d push 'fourth)))
  (check-equal? (send d count) 3)
  (check-equal? (send d pop) 'fourth)
  (check-equal? (send d pop) 'third)
  (check-equal? (send d pop) 'second)
  (check-true (send d empty?))
  )