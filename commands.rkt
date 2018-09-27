#lang racket
;; Commands - abstract commands & persistent undo system (requires links to be equal across runs!)

(require racket/serialize
         anaphoric
         (prefix-in db: db)
         "err.rkt"
         "links.rkt"
         "storage.rkt")

(provide make-command command%
         result% open-undo-system current-undo-manager command-table close-undo-system
         undo-target<%> undo-mixin can-undo? undo can-redo? redo get-undo-command)

(define current-undo-manager (make-parameter #f))
(define command-table (make-parameter (make-hash)))

(define (can-undo?)
 (if (current-undo-manager)
     (send (current-undo-manager) can-undo?)
     #f))

(define (undo)
  (precondition:undo-manager-open?)
  (send (current-undo-manager) undo))

(define (can-redo?)
  (if (current-undo-manager)
      (send (current-undo-manager) can-redo?)
      #f))

(define (redo)
  (precondition:undo-manager-open?)
  (send (current-undo-manager) redo))

(define (get-undo-command)
  (precondition:undo-manager-open?)
  (send (current-undo-manager) get-undo-command))

(define undo-target<%>
  (interface () execute can-execute? undo can-undo? redo can-redo?))

(define undo-mixin
  (mixin () (undo-target<%> link-addressable<%>)
    (init-field link)

    (define/public (get-link)
      link)

    (super-new)
    (associate link this)))

(define/contract command%
  (class/c
   [init-field (numeric exact-integer?)]
   [init-field (symbolic symbol?)]
   [init-field (name string?)]
   [init-field (help (or/c (->* () () #:rest (listof string?) string?)
                           string?))]
   [init-field (shortcuts (listof (or/c symbol?
                                        string?
                                        char?)))]
   [execute (->*m ((is-a?/c link%)) () #:rest (listof any/c) any)]
   [can-execute? (->*m ((is-a?/c link%)) () #:rest (listof any/c) any)])
  
  (class* object% (equal<%>)
    (init-field numeric)
    (init-field symbolic)
    (init-field name)
    (init-field help)
    (init-field shortcuts)

    (define/public (execute target-link . args)
       (aif (resolve/maybe 'execute target-link)
       (let ((result (send it execute this args)))
         (cond
           (result
            (db:call-with-transaction
             (get-field db (current-undo-manager))
             (lambda ()
               (send (current-undo-manager) clear-redo-chain)
               (send (current-undo-manager) add-result result)))
            result)
           (else #f)))
       #f))

    (define/public (can-execute? target-link . args)
        (aif (resolve/maybe 'can-execute? target-link)
       (send it can-execute? this args)
       #f))

    (define/public (equal-to? other recur)
      (= numeric (get-field numeric other)))

    (define/public (equal-hash-code-of hash-code)
      (hash-code numeric))

     (define/public (equal-secondary-hash-code-of hash-code)
      (hash-code numeric))
    
    (super-new)
    (when (hash-has-key? (command-table) numeric)
      (fail 'make-command -103 numeric))
    (hash-set! (command-table) numeric this)))

(define make-command
  (let ((counter 0))
    (lambda (symbolic name help keyboard-shortcuts)
      (set! counter (add1 counter))
      (make-object command% counter symbolic name help keyboard-shortcuts))))

(define/contract result%
  (class/c
   [init-field (command (is-a?/c command%))]
   [init-field (result serializable?)]
   [init-field (target-link (is-a?/c link%))]
   [init-field (undo-data serializable?)]
   [init-field (redo-data serializable?)])
  
  (class object%
    (init-field command)
    (init-field result)
    (init-field target-link)
    (init-field undo-data)
    (init-field redo-data)
    
    (define/public (can-undo?)
      (aif (resolve/maybe 'can-undo? (get-field target-link this))
           (send it can-undo? this)
           #f))

    (define/public (undo)
      (aif (resolve/maybe 'undo (get-field target-link this))
           (send it undo this) 
           #f))
      
    (define/public (can-redo?)
      (aif (resolve/maybe 'can-redo? (get-field target-link this))
           (send it can-redo? this)
           #f))

    (define/public (redo)
      (aif (resolve/maybe 'redo (get-field target-link this))
           (let ((result (send it redo this)))
             (send (current-undo-manager) add-result result)
             result)
           #f))

    (super-new)))

(define/contract undo-manager%
  (class/c
   [inherit-field (db (or/c db:connection? #f))]
   [init (->m any/c)]
   [add-result (->m (is-a?/c result%) any)]
   [can-undo? (->m any)]
   [get-undo-command (->m any)]
   [undo (->m any)]
   [can-redo? (->m any)]
   [redo (->m any)]
   [clear-redo-chain (->m any)])
   
  (class storage%
    (inherit open set-component new-item set get get-latest-item delete-item)
    (inherit-field db)

    ;; Public API
    
    (define/public (init)
      (open)
      (set-component 'UndoChain '(TargetLink blob) '(Command integer) '(UndoData blob) '(RedoData blob) '(Result blob))
      (set-component 'RedoChain '(TargetLink blob) '(Command integer) '(UndoData blob) '(RedoData blob) '(Result blob)))

    (define/public (add-result result)
      (internal-add 'UndoChain result))

    (define/public (can-undo?)
      (if (get-latest-item 'UndoChain) #t #f))

    (define/public (get-undo-command)
      (aif (get-latest-item 'UndoChain)
           (hash-ref (command-table) (get 'UndoChain it 'Command) #f)
           #f))

    (define/public (undo)
      (aif (get-latest-item 'UndoChain)
           (begin0 (send (item->result 'UndoChain it) undo)
                   (shift-item-to-redo it))
           #f))

    (define/public (can-redo?)
      (if (get-latest-item 'RedoChain) #t #f))

    (define/public (redo)
      (aif (get-latest-item 'RedoChain)
           (begin0 (send (item->result 'RedoChain it) redo)
                   (delete-item 'RedoChain it))
           #f))

    (define/public (clear-redo-chain)
      (db:query-exec db "DELETE FROM RedoChain;"))

    ;; Private part
    (define (shift-item-to-redo item)
      (define result (item->result 'UndoChain item))
      (db:call-with-transaction
       db
       (lambda ()
         (delete-item 'UndoChain item)
         (internal-add 'RedoChain result))))

    (define (item->result chain item)
      (letrec ((command (hash-ref (command-table) (get chain item 'Command) #f))
               (TargetLinkBytes (get chain item 'TargetLink))
               (UndoDataBytes (get chain item 'UndoData))
               (RedoDataBytes (get chain item 'RedoData))
               (ResultBytes (get chain item 'Result)))
        (if (and command (bytes? TargetLinkBytes) (bytes? UndoDataBytes) (bytes? RedoDataBytes) (bytes? ResultBytes)
                 (> (bytes-length TargetLinkBytes) 0)
                 (> (bytes-length TargetLinkBytes) 0)
                 (> (bytes-length UndoDataBytes) 0)
                 (> (bytes-length RedoDataBytes) 0)
                 (> (bytes-length ResultBytes) 0))
            (new result%
                 [command command]
                 [target-link (list->link (with-input-from-bytes TargetLinkBytes (lambda () (deserialize (read)))))]
                 [result (with-input-from-bytes ResultBytes (lambda () (deserialize (read))))]
                 [undo-data (with-input-from-bytes UndoDataBytes (lambda () (deserialize (read))))]
                 [redo-data (with-input-from-bytes RedoDataBytes (lambda () (deserialize (read))))])
            (if command (fail 'undo-manager -102) (fail 'undo-manager -104 (get chain item 'Command #f))))))
                 
    (define (internal-add chain result)
      (when result
        (let ((item (new-item chain)))
          (db:call-with-transaction
           db
           (lambda ()
             (set chain item 'Command (get-field numeric (get-field command result)))
             (set chain item 'TargetLink (with-output-to-bytes (lambda ()
                                                                 (write (serialize (link->list (get-field target-link result)))))))
             (set chain item 'UndoData (with-output-to-bytes (lambda () (write (serialize (get-field undo-data result))))))
             (set chain item 'RedoData (with-output-to-bytes (lambda () (write (serialize (get-field redo-data result))))))
             (set chain item 'Result (with-output-to-bytes (lambda () (write (serialize (get-field result result))))))))
          item)))

    (super-new)))

(define/contract (open-undo-system persistent-path #:maintenance-start-proc [maintenance-start (lambda (msg) (void))]
                                   #:maintenance-end-proc [maintenance-end (lambda (msg) (void))]
                                   #:maintenance-start-message [start-message "Compacting undo storage..."]
                                   #:maintenance-end-message [end-message "Compacting undo storage... done."])
  (->* ((or/c path? path-string?)) (#:maintenance-start-proc (-> string? any)
                                    #:maintenance-end-proc(-> string? any)
                                    #:maintenance-start-message string?
                                    #:maintenance-end-message string?)
       any)
  (precondition:undo-manager-closed?)
  (current-undo-manager (new undo-manager% [file persistent-path]))
  (send (current-undo-manager) init)
  (when (send (current-undo-manager) needs-maintenance?)
    (maintenance-start start-message)
    (send (current-undo-manager) start-maintenance (lambda () (maintenance-end end-message)))))

(define (close-undo-system)
  (define manager (current-undo-manager))
  (current-undo-manager #f)
  (when (and manager (send manager is-open?))
    (send manager close)))

(define (resolve/maybe caller target)
  (resolve target (lambda () (warn caller -100 (link->list target)) #f)))

;; preconditions
(define (precondition:undo-manager-closed?)
  (when (is-a? (current-undo-manager) undo-manager%)
    (fail 'undo-manager -105)))

(define (precondition:undo-manager-open?)
  (unless (is-a? (current-undo-manager) undo-manager%)
    (fail 'undo-manager -106)))

;; error messages
(register-errors
 '((-100 appy-command-target-not-found)
   (-101 appy-command-empty-data-field)
   (-102 appy-undo-empty-data)
   (-103 appy-command-numeric-twice)
   (-104 appy-undo-version-err)
   (-105 appy-undo-manager-already-open)
   (-106 appy-undo-manager-closed)))

(module+ test
  (require rackunit)

  (define undo-tester%
    (undo-mixin
     (class* object% (undo-target<%>)
       (define/public (execute cmd args)
         (make-object result% cmd 'test-result (send this get-link) '(undo data) '(some redo data)))
       (define/public (can-execute? cmd args)
         #t)
       (define/public (undo result)
         #t)
       (define/public (can-undo? result)
         #t)
       (define/public (redo result)
         (make-object result% (get-field command result) 'test-result (send this get-link) '(undo data) '(some redo data)))
       (define/public (can-redo? result)
         #t)
       (super-new))))

  (define undoable-stack%
    (undo-mixin
     (class* object% (undo-target<%>)
       (define data '())
       (define/public (push arg)
         (set! data (cons arg data)))
       (define/public (pop)
         (let ((result (first data)))
           (set! data (rest data))
           result))
       (define/public (execute cmd args)
         (case (get-field symbolic cmd)
           ((push) (let ((previous data))
                     (make-object result% cmd (push (first args)) (send this get-link) previous data))) 
           ((pop) (let ((previous data))
                    (make-object result% cmd (pop) (send this get-link) previous data)))
           (else (error 'undoable-stack% "unknown command, I only understand 'push and 'pop, given ~s" (get-field symbolic cmd)))))
       (define/public (can-execute? cmd args)
         (case (get-field symbolic cmd)
           ((push) (and (list? args) (not (null? args))))
           ((pop) (not (null? data)))
           (else #f)))
       (define/public (can-undo? result)
         #t)
       (define/public (undo result)
         (set! data (get-field undo-data result)) #t)
       (define/public (can-redo? result)
         #t)
       (define/public (redo result)
         (let ((previous data))
           (set! data (get-field redo-data result))
           (make-object result% (get-field command result) (void) (send this get-link) previous data)))
       (super-new))))
       
      
  (define (setup)
    (when (file-exists? "private/testing/undo-tests") (delete-file "private/testing/undo-tests"))
    (when (file-exists? "private/testing/undo-tests-shm") (delete-file "private/testing/undo-tests-shm"))
    (when (file-exists?  "private/testing/undo-tests-wal") (delete-file "private/testing/undo-tests-wal"))
    (open-undo-system "private/testing/undo-tests"))

  (define a (make-object undo-tester% (make-link '(this is a testlink))))
  (define teststack (make-object undoable-stack% (make-link '(link to the stack))))
 
  (define (teardown)
    (close-undo-system))

  ;; basic functionality
  (check-exn exn:fail? (lambda () (undo)))
  (check-false (can-undo?))
  (check-false (can-redo?))
  (check-not-exn (lambda () (setup)))
  (check-false (can-undo?))
  (check-false (can-redo?))
  (define cmd1 (make-command 'cmd1 "Command 1" "just a test command" '()))
  (define cmd2 (make-command 'cmd2 "Command 2" "another test command" '()))
  (check-equal? (send a get-link) (make-link '(this is a testlink)))
  (check-true (send cmd1 can-execute? (send a get-link)))
  (check-equal? (get-field result (send cmd1 execute (send a get-link))) 'test-result)
  (check-true (can-undo?))
  (check-equal? (get-field symbolic (get-undo-command)) 'cmd1)
  (check-true (not (false? (undo))))
  (check-true (can-redo?))
  (check-true (not (false? (redo))))
  (check-false (can-redo?))
  (check-true (can-undo?))

  ;; test the undoable stack example
  (define push (make-command 'push "Stack Push" "push a value on the stack" '()))
  (define pop (make-command 'pop "Stack Pop" "pop a value from the stack" '()))
  (define s (make-link '(link to the stack)))
  (check-true (send teststack can-execute? push '(test)))
  (check-false (send teststack can-execute? cmd1 '()))
  (check-not-exn (lambda () (send teststack execute push '(a))))
  (check-not-exn (lambda () (send teststack execute push '(b))))
  (check-true (can-undo?))
  (check-true (not (false? (undo))))
  (check-true (can-redo?))
  (check-true (not (false? (send push execute s 'c))))
  (check-equal? (get-field result (send pop execute s)) 'c)
  (check-true (not (false? (undo))))
  (check-equal? (get-field result (send pop execute s)) 'c)
  (check-equal? (get-field result (send pop execute s)) 'b)
  (check-equal? (get-field result (send pop execute s)) 'a)
  (check-false (can-redo?))
  (check-true (not (false? (undo))))
  (check-true (not (false? (undo))))
  (check-true (can-redo?))
  (check-not-exn (lambda () (redo)))
  (check-true (can-redo?))
  (check-not-exn (lambda () (redo)))
  (check-not-exn (lambda () (teardown)))
  )