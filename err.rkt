#lang racket
;; Err - logging facilities and error handling

(require racket/logging
         "lang.rkt"
         "lang-defaults.rkt")

(provide log-fatal log-error log-info log-debug log-warning app-logger register-errors current-error-messages
         warn fatal fail info debug)

(define-logger app)

(current-logger app-logger)

(define current-error-messages (make-parameter (make-hash)))

(define (register-errors alist)
  (for ([spec (in-list alist)])
    (cond
      ((hash-has-key? (current-error-messages) (first spec))
       (log-fatal "register-errors: duplicate error number ~s, already used for message ~s attempting to redefine for message ~s"
                  (first spec) (hash-ref (current-error-messages) (first spec) "unknown") (second spec))
       (error 'register-errors "duplicate error number ~s, already used for message ~s attempting to redefine for message ~s"
              (first spec) (hash-ref (current-error-messages) (first spec) "unknown") (second spec)))
      (else (hash-set! (current-error-messages) (first spec) (second spec))))))

(define (translate-level level)
  (case level
    ((warning) (appy-Warning))
    ((info) (appy-Info))
    ((debug) (appy-Debug))
    ((fatal) (appy-Fatal))
    (else (appy-Error))))

(define (app-error level source num args)
  (let ((msg (if (hash-has-key? (current-error-messages) num)
                 (let ((error-msg (hash-ref (current-error-messages) num 'appy-unknown-error)))
                   (format "[~a] ~a: ~a [~a ~s]" num (translate-level level) 
                           (if (string? error-msg)
                               (if (null? args) error-msg (apply format (cons error-msg args)))
                               (apply tr (cons error-msg args)))
                           (appy-Module)
                           (symbol->string source)))
                 (appy-err-event
                    num (appy-Error) (translate-level level) num (appy-Module) (symbol->string source)))))
    (case level
      ((info) (log-info msg))
      ((fatal) (log-fatal msg) (error msg))
      ((debug) (log-debug msg))
      ((warning) (log-warning msg))
      (else (log-error msg) (error msg)))))

(define (warn source num . args)
  (app-error 'warning source num args))

(define (fatal source num . args)
  (app-error 'fatal source num args))

(define (info source num . args)
  (app-error 'info source num args))

(define (fail source num . args)
  (app-error 'error source num args))

(define (debug source num . args)
  (app-error 'debug source num args))

(module+ test
  (require rackunit)

  (check-not-exn (lambda () (register-errors
                             '((-1 "just some warning ~a")
                               (-2 "some info string with arg1=~s and arg2=~s")
                               (-3 "some debugging information ~s")
                               (-4 "some general error")
                               (-5 "some fatal error condition due to ~a")))))

  (check-not-exn (lambda () (warn 'warning-test -1 "for testing")))
  (check-not-exn (lambda () (info 'info-test -2 "argument 1" "argument 2")))
  (check-not-exn (lambda () (debug 'debug-test -3 '(debug this))))
  (check-exn exn:fail? (lambda () (fail 'fail-test -4)))
  (check-exn exn:fail? (lambda () (fatal 'fatal-error-test -5 "intentional failure")))
  (check-not-exn (lambda () (warn 'warning-without-message -6)))
  (check-exn exn:fail? (lambda () (fatal 'fatal-error-without-message -7)))
  )

