#lang racket
;; App - application definition and component registration

(require racket/runtime-path
         "lang.rkt"
         "lang-defaults.rkt"
         "err.rkt"
         "prefs.rkt"
         "commands.rkt"
         "platform.rkt")

(provide application% the-application valid-application-name? valid-version-string?)

(define the-application (make-parameter #f))

(define appy-default-exception-handler
  (make-parameter
   (lambda (app exn)
     (displayln (format "~a ~s - ~a"
                        (appy-Application)
                        (get-field short-name app)
                        (exn-message exn)) (current-error-port))
     (send app shutdown exn)
     (raise exn))))

;; Contracts
(define (valid-application-name? s)
  (and (string? s)
       (> (string-length (string-trim s)) 0)
       (<= (string-length s) 64)
       (not (or (string-contains? s "?")
                (string-contains? s "/")
                (string-contains? s "\\")
                (string-contains? s "*")
                (string-contains? s "\"")
                (string-contains? s "\0")))))

(define (valid-version-string? s)
  (define s1 (string-split (string-trim s) "-"))
  (regexp-match #px"^\\d+\\.\\d+\\.[\\d+|\\*]" (car s1)))


;; The main application class
(define/contract application%
  (class/c
   (init-field [name valid-application-name?])
   (init-field [short-name valid-application-name?])
   (init-field [version valid-version-string?])
   (init-field [open-procs (listof (-> (is-a?/c application%) any))])
   (init-field [run-procs (listof (-> (is-a?/c application%) any))])
   (init-field [close-procs (listof (-> (is-a?/c application%) any))])
   (init-field [exception-handler (-> (is-a?/c application%) exn? any)])
   (init-field [shutdown-handler (-> (is-a?/c application%) (or/c exn? #f) any)])
   (init-field [command-line-handler (-> (is-a?/c application%) any)])
   (init-field [start-empty-handler (-> (is-a?/c application%) any)])
   (init-field [open-file-handler (-> (is-a?/c application%) (or/c path? path-string?) any)])
   (init-field [pref-dir (or/c path? path-string?)])
   (init-field [maintenance-start-proc (-> string? any)])
   (init-field [maintenance-end-proc (-> string? any)])
   (init-field [language (or/c symbol? #f)])
   (init-field [hardcoded-translations (listof (or/c path? path-string?))])
   (field [prefs-file (or/c path? path-string? #f)])
   (field [undo-file (or/c path? path-string? #f)])
   (field [closing? boolean?])
   (field [is-open? boolean?])
   (field [command-line-files (listof path?)])
   [open (->m any)]
   [run (->m any)]
   [close (->m any)]
   [open-files (->m (listof (or/c path? path-string?)) any)]
   [default-command-line-handler (->m (is-a?/c application%) any)]
   [get-full-name (->m string?)]
   [shutdown (->*m () ((or/c exn? #f)) any)])
  (class object%
    (init-field name)
    (init-field short-name)
    (init-field version)
    (init-field open-procs)
    (init-field run-procs)
    (init-field close-procs)
    (init-field [exception-handler (appy-default-exception-handler)])
    (init-field [shutdown-handler (lambda (app maybe-exn) (send app close) #t)])
    (init-field [command-line-handler (lambda (app) (send app default-command-line-handler app))])
    (init-field [start-empty-handler (lambda (app) (void))])
    (init-field [open-file-handler (lambda (app file) (void))])
    (init-field [pref-dir (system-prefs-dir short-name)])
    (init-field [maintenance-start-proc (lambda (msg) (info 'application:open -200 name msg))])
    (init-field [maintenance-end-proc (lambda (msg) (info 'application:open -201 name msg))])
    (init-field [language #f])
    (init-field [hardcoded-translations '()])

    (field [prefs-file #f])
    (field [undo-file #f])
    (field [closing? #f])
    (field [is-open? #f])
    (field [command-line-files '()])

    ;; Public API
    (define/pubment (open)
      (with-handlers ([exn:fail?
                       (lambda (e) (exception-handler this e))])
        (precondition:is-closed?)
        (create-maybe-paths)
        (prefs-open prefs-file
                    #:maintenance-start-proc maintenance-start-proc
                    #:maintenance-end-proc maintenance-end-proc
                    #:maintenance-start-message (appy-start-clean-preferences )
                    #:maintenance-end-message (appy-end-clean-preferences))
        (when language
          (defpref 'language 'en_US)
          (pref 'language language))
        (when (pref? 'language)
          (load-language (pref 'language)))
        (open-undo-system undo-file
                          #:maintenance-start-proc maintenance-start-proc
                          #:maintenance-end-proc maintenance-end-proc
                          #:maintenance-start-message (appy-start-clean-undo)
                          #:maintenance-end-message (appy-done-clean-undo))
        ((get-field command-line-handler this) this)
        (for [(proc (in-list open-procs))]
          (proc this))
        (set! is-open? #t)
        (if (not (null? (get-field command-line-files this)))
            (open-files (get-field command-line-files this))
            ((get-field start-empty-handler this) this))))

    (define/pubment (run)
      (with-handlers ([exn:fail?
                       (lambda (e) (exception-handler this e))])
        (precondition:is-open?)
        (for [(proc (in-list run-procs))]
          (proc this))))

    (define/pubment (close)
      (with-handlers ([exn:fail?
                       (lambda (e) (exception-handler this e))])
        (cond
          (is-open?
           (for [(proc (in-list close-procs))]
             (proc this))
           (set! is-open? #f)
           (close-undo-system)
           (prefs-close))
          (else (warn 'application:close -204 name)))))

    (define/public (open-files files)
      (for-each (lambda (x) ((get-field open-file-handler this) this x)) files))

    (define/public (default-command-line-handler app)
      (command-line
       #:program (get-field name this)
       #:argv (current-command-line-arguments)
       #:multi
       [("-o" "--open") document
                        "open document"
                        (set-field! command-line-files app (append (get-field command-line-files app)
                                                                   (list document)))]
       [("-l" "--language") lang
                            "set application language"
                            (begin (init-language-system #:application-name name #:hardcoded-paths hardcoded-translations)
                                   (load-language (string->symbol (bytes->string/utf-8 lang))))]))

    (define/public (get-full-name)
      (string-append name " " version))

    (define/pubment (shutdown [exn #f])
      (shutdown-handler this exn))
    
    ;; Private
    (define/pubment (init) ;; not for external calls but subclasses may need to augment this
      (set-field! prefs-file this (build-path pref-dir "prefs.sqlite"))
      (set-field! undo-file this (build-path pref-dir "undo.sqlite")))

    (define (create-maybe-paths)
      (make-directory* pref-dir))

    ;; preconditions
    (define (precondition:is-open?)
      (unless is-open?
        (fail 'application -203 name)))

    (define (precondition:is-closed?)
      (when is-open?
        (fail 'application -202 name)))
    
    (super-new)
    (init)))




;; error messages
(register-errors
 '((-200 appy-maintenance-message)
   (-201 appy-maintenance-ended)
   (-202 appy-open-db-twice)
   (-203 appy-use-closed-db)
   (-204 appy-close-db-twice)))

(module+ test
  (require rackunit)

  (define t1 #f)
  (define t1b #f)
  (define t2 #f)
  (define t2b #f)
  (define t3 #f)
  (define t3b #f)
  
  (define a (new application%
                 [name "Test Application"]
                 (short-name "TestApp")
                 [version "0.1.1-beta"]
                 [open-procs (list (lambda (app) (set! t1 #t)) (lambda (app) (set! t1b #t)))]
                 [run-procs (list (lambda (app) (set! t2 #t)) (lambda (app) (set! t2b #t)))]
                 [close-procs (list (lambda (app) (set! t3 #t)) (lambda (app) (set! t3b #t)))]
                 [maintenance-start-proc (lambda (msg) (displayln msg))]
                 [maintenance-end-proc (lambda (msg) (displayln msg))]))

  (displayln "--------------------------------------------------------------------------" (current-error-port))
  (displayln "APPY: An error on the next line is intended and part of testing \"app.rkt\":" (current-error-port))
  (check-exn exn:fail? (lambda () (send a run)))
  (check-false t1)
  (check-false t1b)
  (check-not-exn (lambda () (send a open)))
  (check-true t1)
  (check-true t1b)
  (displayln "--------------------------------------------------------------------------" (current-error-port))
  (displayln "APPY: An error on the next line is intended and part of testing \"app.rkt\":" (current-error-port))
  (check-exn exn:fail? (lambda () (send a open)))
  (displayln "-------------------------------------------------------------------------" (current-error-port))
  (set! t3 #f)
  (set! t3b #f)
  (check-not-exn (lambda () (send a open)))
  (check-false t2)
  (check-false t2b)
  (check-not-exn (lambda () (send a run)))
  (check-true t2)
  (check-true t2b)
  (check-false t3)
  (check-false t3b)
  (check-not-exn (lambda () (send a close)))
  (check-true t3)
  (check-true t3b)

  )