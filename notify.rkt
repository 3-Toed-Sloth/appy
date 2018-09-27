#lang racket

(require srfi/1)

(provide notify subscribe unsubscribe remove-subscriber remove-all-subscribers notify-prune-frequency-msec notify-prune-delta-msec)

(define notify-prune-frequency-msec (make-parameter 5000)) ; ugly hack to prevent memory leaks (see below in Private part)
(define notify-prune-delta-msec (make-parameter 8000)) ; see maybe-add-to-buffer

(define events (make-hash))
(define buffer '())
(define last-prune (current-inexact-milliseconds))

(define-struct receiver (subscriber callback exclusions)) 

(define (notify event notifier data)
  (for ([rec (in-list (hash-ref events event '()))])
    (let ((matching-buffered (get-maybe-matching-buffered event notifier rec))) ; find earlier event that might cause a cycle
    (cond
      (matching-buffered (remove-buffered matching-buffered)) ; if there is a cycle cause, then do not notify and remove the previous event from buffer
      (else 
       (maybe-add-to-buffer event notifier rec)
       ((receiver-callback rec) event notifier data))))))

(define (subscribe subscriber event callback [exclusions'()])
  (hash-update! events
                event
                (lambda (receivers)
                  (cons (make-receiver subscriber callback exclusions)
                        receivers))
                '()))
                      
(define (unsubscribe subscriber event)
  (hash-update! events
                event
                (lambda (receivers)
                  (filter (lambda (rec)
                            (not (equal? (receiver-subscriber rec) subscriber)))
                          receivers)))
  (when (null? (hash-ref events event)) ; filter might return '(), we don't want this event to hang around
    (hash-remove! events event)))

(define (list-subscribed-events)
  (for/list ([(k v) (in-hash events)])
    k))

(define (remove-subscriber subscriber)
  (for ([evt (in-list (list-subscribed-events))])
    (unsubscribe subscriber evt)))

(define (remove-all-subscribers)
  (set! events (make-hash))
  (set! buffer '()))

;; ----------------------------------------
;; Private part

;; The problem is that there is no guarantee that the second of any pairs of mutually exclusive events
;; will occur, and so the buffer might grow indefinitely. That's why it is pruned every call to notify
;; if the last pruning was longer than (notify-prune-frequency-msec) ago and the buffer entry was added
;; more than or equal to (notify-prune-delta-msec) milliseconds ago. This means that event loops may occur
;; with mutually exclusive events if their handling takes longer than (notify-prune-delta-msec) milliseconds.
(define (maybe-add-to-buffer event notifier receiver)
  (define now (current-inexact-milliseconds))
  (when (> (- now last-prune) (notify-prune-frequency-msec))
    (set! buffer (filter (lambda (elem) (< (- now (first elem)) (notify-prune-delta-msec)))
                          buffer))
    (set! last-prune now))
  (unless (empty? (receiver-exclusions receiver))
    (set! buffer
          (cons (list now event notifier receiver) buffer))))

(define (get-maybe-matching-buffered event notifier receiver)
  (find (lambda (buffered)
                  (and (equal? (receiver-subscriber (fourth buffered)) notifier)
                        (equal? (third buffered) (receiver-subscriber receiver))
                        (member event (receiver-exclusions (fourth buffered)))))
                buffer))

(define (remove-buffered buffered)
  (set! buffer (filter (lambda (elem) (not (equal? elem buffered))) buffer)))

;; ----------------------------------------
;; Testing

(module+ test
  (require rackunit
           racket/set)

  (define a #f)

  ;; Test for standard event notifications to multiple subscribers.
  (check-not-exn (lambda () (notify 'test 'notifier1 '(hello world!))))
  (check-not-exn (lambda () (subscribe 'subscriber1 'test (lambda (evt notifier data)
                                                               (check-equal? data '(test-2))) '())))
  (check-not-exn (lambda () (subscribe 'subscriber2 'test (lambda (evt notifier data)
                                                               (check-equal? data '(test-2))) '())))
  (check-not-exn (lambda () (notify 'test 'notifier2 '(test-2))))
  (check-not-exn (lambda () (subscribe 'subscriber2 'test2 (lambda (evt notifier data)
                                                                (check-equal? data '(another test data))) '())))
  ;; Test for cyclic events. The following notifications would create an infinite cycle without the exclusions.
  (check-not-exn (lambda () (subscribe 'model
                                          'view-change
                                          (lambda (evt notifier data)
                                            (notify 'model-change 'model '(never triggered))
                                            (set! a 'model))
                                          '(model-change))))
  (check-not-exn (lambda () (subscribe 'view
                                          'model-change
                                          (lambda (evt notifier data)
                                            (notify 'view-change 'view '(never triggered))
                                            (set! a 'view))
                                          '(view-change))))
  (check-not-exn (lambda () (notify  'view-change 'view  '(view change data))))
  (check-equal? a 'model)
  (check-not-exn (lambda () (notify  'model-change 'model '(model change data))))
  (check-equal? a 'view)
  ;; Unsubscribe and remove-subscriber
  (check-not-exn (lambda () (unsubscribe 'subscriber2 'test)))
  (check-true (set=? (list->set (list-subscribed-events)) (list->set '(test test2 view-change model-change))))
  (check-not-exn (lambda () (remove-subscriber 'subscriber1)))
  (check-not-exn (lambda () (remove-subscriber 'subscriber2)))
  (check-true (set=? (list->set (list-subscribed-events)) (list->set '(view-change model-change))))
  (check-not-exn (lambda () (remove-all-subscribers)))
  (check-true (set-empty? (list->set (list-subscribed-events))))
  ;; Multiple exclusions
  (check-not-exn (lambda () (subscribe 'model
                                        'view-change1
                                          (lambda (evt notifier data)
                                            (notify 'model-change 'model '(never triggered))
                                            (set! a 'model))
                                          '(model-change1 model-change2))))
   (check-not-exn (lambda () (subscribe 'model
                                        'view-change2
                                          (lambda (evt notifier data)
                                            (notify 'model-change1 'model '(never triggered))
                                            (set! a 'model))
                                          '(model-change1 model-change2))))
  (check-not-exn (lambda () (subscribe 'view
                                          'model-change1
                                          (lambda (evt notifier data)
                                            (notify 'view-change2 'view '(never triggered))
                                            (set! a 'view))
                                          '(view-change1 view-change2))))
  (check-not-exn (lambda () (notify 'view 'view-change1 '(view change 1 data))))
  (check-not-exn (lambda () (notify 'model 'model-change2 '(model change 2 data))))
  (check-not-exn (lambda () (remove-all-subscribers)))
  )