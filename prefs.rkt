#lang racket
;; Preferences

(require "storage-interface.rkt"
         "storage.rkt")

(provide pref? pref prefs-open prefs-close prefs-open? defpref current-pref-storage)

(define current-pref-storage (make-parameter #f))
(define pref-defaults (make-parameter #f))

;; Preconditions
(define (precondition:prefs-open?)
  (unless (current-pref-storage)
    (error 'prefs "The preferences system needs to be initialized first!")))

(define (precondition:pref-exists? key)
  (unless (hash-has-key? (pref-defaults) key)
    (error 'prefs "Unknown preference key: ~s" key)))

;; main implementation
(define/contract (prefs-open file #:maintenance-start-proc [start-maintenance (lambda (msg) (void))]
                             #:maintenance-end-proc [end-maintenance (lambda (msg) (void))]
                             #:maintenance-start-message [start-message "Compacting preferences..."]
                             #:maintenance-end-message [end-message "Compacting preferences... done."])
  (->* ((or/c path? path-string?)) (#:maintenance-start-proc (-> string? any)
                                    #:maintenance-end-proc (-> string? any)
                                    #:maintenance-start-message string?
                                    #:maintenance-end-message string?)
       any)
  (pref-defaults (make-hash))
  (current-pref-storage (make-object storage% file))
  (send (current-pref-storage) open)
  (when (send (current-pref-storage) needs-maintenance?)
    (start-maintenance start-message)
    (send (current-pref-storage) start-maintenance (lambda () (end-maintenance end-message)))))

(define (prefs-open?)
  (if (current-pref-storage) #t #f))

(define/contract (pref? key)
  (-> symbol? any)
  (precondition:prefs-open?)
  (hash-has-key? (pref-defaults) key))

(define/contract (defpref key default)
  (-> symbol? any/c any)
  (precondition:prefs-open?)
  (hash-set! (pref-defaults) key default))

(define (prefs-close)
  (when (current-pref-storage)
    (send (current-pref-storage) close)
    (current-pref-storage #f)))
  
(define (get-default-pref key failure-thunk)
  (precondition:pref-exists? key)
  (hash-ref (pref-defaults) key failure-thunk))

(define pref
  (case-lambda
    ((key) (precondition:prefs-open?)
           (cond
             ((pref? key)
              (if (send (current-pref-storage) kv-has-key? key)
                  (send (current-pref-storage) kv-get key (lambda () (error 'prefs "Attempt to obtain preference for unknown key ~s" key)))
                  (get-default-pref key #f)))
             (else (error 'prefs "Attempt to obtain preference for unknown key ~s" key))))
    ((key value) (precondition:prefs-open?)
                 (cond
                  ((pref? key)
                   (if (equal? value (get-default-pref key #f))
                       (send (current-pref-storage) kv-delete key)
                       (send (current-pref-storage) kv-set key value)))
                  (else (error 'prefs "Attempt to set the value of an unknown key ~s, you need to define the preference default with defpref first!" key))))))

(module+ test
  (require rackunit)

   (when (file-exists? "private/testing/prefs") (delete-file "private/testing/prefs"))

  (check-exn exn:fail? (lambda () (defpref 'test 'hello)))
  (check-not-exn (lambda () (prefs-open "private/testing/prefs")))
  (check-true (prefs-open?))
  (check-not-exn (lambda () (defpref 'test 'hello)))
  (check-equal? (pref 'test) 'hello)
  (check-not-exn (lambda () (pref 'test "whatever")))
  (check-equal? (pref 'test) "whatever")
  (check-exn exn:fail? (lambda () (pref 'unknown)))
  (check-not-exn (lambda () (prefs-close)))
  (check-false (prefs-open?))

 )