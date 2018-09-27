#lang racket
;; Storage - generic <component, obj, attr> -> value storage class
(require racket/serialize
         (prefix-in db: db)
         (prefix-in srfi13- srfi/13)
         "storage-interface.rkt")

(provide storage% is-valid-search-term?)

;; Attributes: symbols
;; Items: number, returned by storage%
;; Components: symbols
;; Values: text, bytes, numbers, and lists of them

(define (is-valid-search-term? term)
  (not (or (srfi13-string-contains term "_")
           (srfi13-string-contains term "%"))))

(define/contract storage%
  (class/c
   [init-field (file (or/c path? path-string?))]
   [field (db (or/c db:connection? #f))]
   [index (->*m (symbol?) () #:rest (listof symbol?) any)]
   [search-text (->*m (symbol? symbol? is-valid-search-term?)
                      (#:search-mode (one-of/c 'starting-with 'substring 'exact)
                       #:sort-by (or/c #f (listof symbol?))
                       #:sort-order (one-of/c 'asc 'desc)) any)]
   [get (->*m (symbol? exact-integer? symbol?) (#:on-fail (-> any)
                                         #:deleted? boolean?) any)]
   [get-latest-item (->*m (symbol?) (#:on-fail (-> any)
                                         #:deleted? boolean?) any)]
   [set (->m symbol? exact-integer? symbol? any/c any)]
   [new-item (->m symbol? any)]
   [delete-item (->m symbol? exact-integer? any)]
   [add (->m symbol? exact-integer? symbol? any/c any)]
   [contains? (->*m (symbol? exact-integer? symbol?) (#:deleted? boolean?) any)]
   [is-list-attribute? (->m symbol? symbol? any)]
   [open (->m any)]
   [close (->m any)]
   [is-open? (->m any)]
   [needs-maintenance? (->m any)]
   [get-info (->m any)]
   [start-maintenance (->m (-> any) any)]
   [set-component (->*m (symbol?) #:rest (listof
                                          (or/c
                                           (list/c
                                            (and/c symbol?
                                                 (not/c (one-of/c '_Internal '_InternalKVStore 'sqlite)))
                                            (one-of/c 'text 'integer 'blob))
                                           (list/c
                                            (and/c symbol?
                                                 (not/c (one-of/c '_Internal '_InternalKVStore 'sqlite)))
                                            (one-of/c 'list-of)
                                             (one-of/c 'text 'integer 'blob)))) any)]
   [kv-set (->m symbol? serializable? any)]
   [kv-get (->m symbol? any/c any)]
   [kv-has-key? (->m symbol? any)]
   [kv-delete (->m symbol? any)]
   [with-transaction (->m (-> any) any)]
  )
  
  (class* object% (storage<%>)
    (init-field file)
    (field (db #f))
    
    (define internal-key:launches 1)
    (define internal-key:deleted-objects 2)

    ;; PUBLIC API

    (define/public (index component . attributes)
      (precondition:is-open?)
      
      (define (attributes->name-list attributes)
        (when (null? attributes)
          (error 'storage:index "no attributes provided for component '~a" component))
        (define attribute-names (map attribute->column-name attributes))
        (if (null? (cdr attribute-names))
            (car attribute-names)
            (apply string-append
                   (for/fold ([result (list  (car attribute-names))])
                             ([s (in-list (cdr attribute-names))])
                     (append result (list "," s))))))
      (db:query-exec
       db
       (string-append
        "CREATE INDEX IF NOT EXISTS `"
        (component->unquoted-table-name component)
        "Index` ON "
        (component->table-name component)
        " (" (attributes->name-list attributes) ");"))
      )

    (define/public (search-text component attribute search-term #:search-mode [search-mode 'starting-with] #:sort-by [sort-by #f] #:sort-order [sort-order 'asc])
      (precondition:is-open?)
      (precondition:is-valid-search-term? search-term)
      (precondition:is-valid-search-mode? search-mode)
      (precondition:is-valid-sort-order? sort-order)
      (define refined-term (case search-mode
                             ((starting-with) (format "~a%" search-term))
                             ((substring) (format "%~a%" search-term))
                             (else search-term)))
      (define (compile-sort-by-clause attributes)
        (if (not (pair? attributes))
            "Item"
            (apply string-append
                   (for/fold ([result (list (attribute->column-name (car attributes)))])
                             ([att (in-list (cdr attributes))])
                     (append result (list "," (attribute->column-name att)))))))
      (define result
        (if sort-by
            (db:query-list
             db
             (string-append "SELECT Item FROM "
                            (component->table-name component)
                            " WHERE _Status=0 AND "
                            (attribute->column-name attribute)
                            " LIKE $1 ORDER BY "
                            (compile-sort-by-clause sort-by)
                            (if (equal? sort-order 'desc)
                                " DESC;"
                                " ASC;"))
             refined-term)
            (db:query-list
             db
             (string-append "SELECT Item FROM "
                            (component->table-name component)
                            " WHERE _Status=0 AND "
                            (attribute->column-name attribute)
                            " LIKE $1")
             refined-term)))
      result)
    
    (define/public (get component item attribute #:on-fail [failure-thunk #f] #:deleted? [deleted? #f])
      (precondition:is-open?)
      (define value (internal-get component item attribute deleted?))
      (if (db:sql-null? value)
          (if (procedure? failure-thunk) (failure-thunk) failure-thunk)
          value))

    (define/public (get-latest-item component #:on-fail [failure-thunk #f] #:deleted? [deleted? #f])
      (precondition:is-open?)
      (define item
        (if deleted?
            (db:query-maybe-value db (format "SELECT MAX(Item) FROM ~a LIMIT 1;" (component->table-name component)))
            (db:query-maybe-value db (format "SELECT MAX(Item) FROM ~a WHERE _Status=0 LIMIT 1;" (component->table-name component)))))
      (if (db:sql-null? item)
          (if (procedure? failure-thunk) (failure-thunk) failure-thunk)
          item))

    (define/public (set component item attribute value)
      (precondition:is-open?)
      (cond
        ((or (is-list-attribute? component attribute)
             (list? value))
         (db:call-with-transaction
          db
          (lambda ()                       
            (db:query-exec db (format "DELETE FROM ~a WHERE Item=$1;"
                                      (list-attribute->table-name component attribute)) item)
            (for-each (lambda (datum) (add component item attribute datum)) value))))
        (else
         (db:query-exec db (format "UPDATE ~a SET ~a=$1 WHERE Item=$2 LIMIT 1;" 
                                   (component->table-name component)
                                   (attribute->column-name attribute))
                        value item))))

    (define/public (new-item component)
      (precondition:is-open?)
      (db:query-exec db (format "INSERT INTO ~a DEFAULT VALUES;" (component->table-name component)))
      (last-row-id))

    (define/public (delete-item component item)
      (precondition:is-open?)
      (db:call-with-transaction
       db
       (lambda ()
         (for ([table (in-list (get-component-tables component))])
           (db:query-exec db (format "UPDATE ~a SET _Status=1 WHERE Item=$1;" table) item))
         (db:query-exec db (format "UPDATE ~a SET _Status=1 WHERE Item=$1;" (component->table-name component)) item))))

    (define/public (add component item attribute value)
      (precondition:is-open?)
      (precondition:is-list-attribute? component attribute)
      (db:query-exec db (format "INSERT INTO ~a (Item,~a) VALUES ($1,$2);"
                                (list-attribute->table-name component attribute)
                                (attribute->column-name attribute))
                     item  value))

    (define/public (contains? component item attribute #:deleted? [deleted? #f])
      (not (db:sql-null? (internal-get component item attribute deleted?))))

    (define/public (is-list-attribute? component attribute)
      (precondition:is-open?)
      (db:table-exists? db (list-attribute->unquoted-table-name component attribute)))
    
    (define/public (open)
      (create-or-open-db))

    (define/public (close)
      (precondition:is-open?)
      (close-db))

    (define/public (is-open?)
      (if db #t #f))

    (define/public (needs-maintenance?)
      (precondition:is-open?)
      (define launches (get-internal internal-key:launches 0))
      (define trash (get-internal internal-key:deleted-objects 0))
      (or (= (modulo launches 200) 0)
          (> trash 1000)))

    (define/public (get-info)
      (precondition:is-open?)
      (list
       (list 'needs-maintenance? (needs-maintenance?))
       (list 'launches (get-internal internal-key:launches 0))
       (list 'trash-count (get-internal internal-key:deleted-objects 0))))

    (define/public (start-maintenance done-callback)
      (precondition:is-open?)
      (empty-trash)
      (db:query-exec db "VACUUM;")
      (done-callback))

    (define/public (set-component component . attributes)
      (precondition:is-open?)
      (unless (db:table-exists? db (component->unquoted-table-name component))
        (create-component component attributes)))

    (define/public (kv-set key value)
      (precondition:is-open?)
      (db:call-with-transaction
       db
       (lambda ()
         (when (kv-has-key? key)
           (db:query-exec db "DELETE FROM _InternalKvStore WHERE Symbol=$1;" (key->db-key key)))
         (db:query-exec db "INSERT INTO _InternalKvStore (Symbol,Data,Status) VALUES ($1,$2,0);" (key->db-key key) (datum->db-blob value)))))
    
    (define/public (kv-get key failure-thunk)
      (precondition:is-open?)
      (define raw-data (db:query-maybe-value
                        db
                        "SELECT Data FROM _InternalKvStore WHERE Symbol=$1 AND Status=0 LIMIT 1;"
                        (key->db-key key)))
      (if (bytes? raw-data)
          (db-blob->datum raw-data failure-thunk)
          (if (procedure? failure-thunk) (failure-thunk) failure-thunk)))

    (define/public (kv-has-key? key)
      (precondition:is-open?)
      (if (equal?
           (db:query-maybe-value db "SELECT EXISTS (SELECT Symbol FROM _InternalKvStore WHERE Symbol=$1 AND Status=0 LIMIT 1);" (key->db-key key))
           1)
          #t
          #f))
    
    (define/public (kv-delete key)
      (when (kv-has-key? key)
        (db:call-with-transaction
         db
         (lambda ()
           (db:query-exec db "UPDATE _InternalKvStore SET Status=1 WHERE Symbol=$2;" (key->db-key key))
           (inc-objects-deleted)))))

    (define/public (with-transaction proc)
      (db:call-with-transaction db proc))

    ;; PRIVATE PART
    (define (internal-get component item attribute deleted?)
      (define status (if deleted? 1 0))
      (cond
        ((is-list-attribute? component attribute)
         (map
          (lambda (v) (vector-ref v 1))
          (db:query-rows db (format "SELECT Item,~a FROM ~a WHERE Item=$1 AND _Status=$2 ORDER BY rowid ASC;"
                                    (attribute->column-name attribute)
                                    (list-attribute->table-name component attribute))
                         item status)))
        (else
         (db:query-maybe-value
          db
          (format "SELECT ~a FROM ~a WHERE Item=$1 and _Status=$2 LIMIT 1;"
                  (attribute->column-name attribute)
                  (component->table-name component))
          item status))))
    
    (define (int->bool v)
      (if (equal? v 1) #t #f))
    
    (define (get-component-tables component)
      (define component-name (string-append (component->unquoted-table-name component) "_"))
      (filter
       (lambda (name) (string-prefix? name component-name))
       (db:list-tables db)))

    (define (get-non-internal-tables)
      (filter (lambda (name) (and (not (string-prefix? name "_Internal"))
                                  (not (string-prefix? name "sqlite_"))))

              (db:list-tables db)))
    
    (define (key->db-key k)
      (cond
        ((symbol? k) (symbol->string k))
        ((string? k) k)
        (else (error 'storage% "a key in the key/value store must be a string or a symbol, given ~s" k))))


    (define (datum->db-blob d)
      (with-output-to-bytes (lambda () (write (serialize d)))))

    (define (db-blob->datum b failure-thunk)
      (if (and (bytes? b) (> (bytes-length b) 0))
          (call-with-default-reading-parameterization
           (lambda () (with-input-from-bytes b (lambda () (deserialize (read))))))
          (if (procedure? failure-thunk) (failure-thunk) failure-thunk)))
    
    (define (last-row-id)
      (db:query-value db "SELECT LAST_INSERT_ROWID();"))
    
    (define (create-component component attributes)
      (precondition:valid-attributes? attributes)
      
      (define component-name (component->table-name component))
      
      (define-values
        (list-attributes normal-attributes) (partition (λ (spec) (equal? (second spec)
                                                                         'list-of))
                                                       attributes))
      (define (spec->attribute-definition spec)
        (case (second spec)
          ((text) (format "~a VARCHAR" (first spec)))
          ((blob) (format "~a BLOB" (first spec)))
          (else (format "~a INTEGER" (first spec)))))
      
      (define (attributes->column-spec attributes)
        (for/fold ([result ""])
                  ([spec (in-list attributes)])
          (string-append result
                         ","
                         (spec->attribute-definition spec))))
    
      (db:query-exec db (format "CREATE TABLE IF NOT EXISTS ~a (Item INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL~a,_Status INTEGER NOT NULL DEFAULT 0)"
                                component-name
                                (attributes->column-spec normal-attributes)))
      (for ([lspec (in-list list-attributes)])
        (db:query-exec
         db
         (format "CREATE TABLE IF NOT EXISTS ~a (Item INTEGER NOT NULL,~a,_Status INTEGER NOT NULL DEFAULT 0, FOREIGN KEY (Item) REFERENCES ~a (Item))"
                 (list-attribute->table-name component (first lspec))                            
                 (spec->attribute-definition (cons (first lspec) (cddr lspec)))
                 component-name))))
      
    (define (create-or-open-db)
      (cond
        (db (error 'storage% "Db already open ~s" file))
        ((file-exists? file) (open-db))
        (else (create-db)))
      (let ((launches (get-internal internal-key:launches #f)))
        (if (number? launches)
            (set-internal internal-key:launches (add1 launches))
            (set-internal internal-key:launches 1))))

    (define (create-db)
      (set! db (db:kill-safe-connection (db:sqlite3-connect #:database file #:mode 'create)))
      (db:query-exec db "PRAGMA journal_mode=WAL;")
      (db:query-exec db "PRAGMA synchronous=FULL;")
      (db:query-exec db (format "PRAGMA application_id=~s;" #x5E47E3))
      (db:query-exec db "CREATE TABLE IF NOT EXISTS _Internal (Id INTEGER PRIMARY KEY,Data BLOB);")
      (db:query-exec db "CREATE TABLE IF NOT EXISTS _InternalKvStore (Symbol VARCHAR NOT NULL UNIQUE ON CONFLICT REPLACE, Data BLOB NOT NULL,Status INTEGER NOT NULL DEFAULT 0);"))

    (define (open-db)
      (set! db (db:kill-safe-connection (db:sqlite3-connect #:database file #:mode 'read/write))))

    (define (close-db)
      (db:disconnect db)
      (set! db #f))

    ;; Internal preferences
    (define (status-deleted? status)
      (equal? status 1))
    
    (define (status-normal? status)
      (equal? status 0))
    
    (define (set-internal id value)
      (precondition:is-open?)
      (db:query-exec db "INSERT OR REPLACE INTO _Internal (Id,Data) VALUES ((SELECT Id FROM _Internal WHERE Id=$1 LIMIT 1),$2);" id (datum->db-blob value)))

    (define (get-internal id failure-thunk)
      (precondition:is-open?)
      (db-blob->datum (db:query-maybe-value db "SELECT Data FROM _Internal WHERE Id=$1 LIMIT 1;" id) failure-thunk))

    (define (inc-objects-deleted)
      (define deleted (get-internal internal-key:deleted-objects #f))
      (if (number? deleted)
          (set-internal internal-key:deleted-objects (add1 deleted))
          (set-internal internal-key:deleted-objects 1)))

    ;; Trash
    (define (empty-trash)
      (precondition:is-open?)
      (db:query-exec db "BEGIN;")
      (db:query-exec db "DELETE FROM _InternalKvStore WHERE Status=1")
      (for-each
       (lambda (table-name)
         (db:query-exec db (format "DELETE FROM ~a WHERE _Status=1;" table-name)))
       (get-non-internal-tables))
      (db:query-exec db "COMMIT;")
      (set-internal internal-key:deleted-objects 0))

    ;; Table name helpers for sqlite (to prevent invalid names)
    (define (string-remove-underscore s)
      (string-replace s "_" ""))

    (define (string-clean s)
      (string-replace (string-normalize-nfd
                       (string-replace s "\000" "")) "\"" "\"\""))
    
    (define (escape-sql-table-name name)
      (string-append
       "`"
       (string-remove-underscore
        (string-clean name))
       "`"))

    (define (component->table-name c)
      (escape-sql-table-name (symbol->string c)))

    (define (component->unquoted-table-name c)
      (string-remove-underscore (string-clean (symbol->string c))))

    (define (list-attribute->unquoted-table-name component attribute)
      (string-append
       (string-remove-underscore (string-clean (symbol->string component)))
       "_"
       (string-remove-underscore (string-clean (symbol->string attribute)))))
    
    (define (list-attribute->table-name component attribute)
      (string-append
       "`"
       (list-attribute->unquoted-table-name component attribute)
       "`"))

    (define (attribute->column-name attribute)
      (escape-sql-table-name (symbol->string attribute)))

    (define (table-entry-exists? table id)
      (equal? 1
              (db:query-maybe-value
               db
               (string-append "SELECT EXISTS (SELECT Item FROM " table
                              " WHERE Item=$1 LIMIT 1);")
               id)))
    
    ;; Preconditions
    (define (precondition:is-open?)
      (unless db 
        (error 'storage% "A storage instance must be opened first, this storage ~s is closed." file)))

    (define (precondition:is-list-attribute? component attribute)
      (unless (is-list-attribute? component attribute)
        (error 'storage% "The attribute '~a' in component '~a' needs to be a list attribute!" attribute component)))

    (define (precondition:valid-attributes? attributes)
      (for-each
       (lambda (spec) (when (or (equal? (first spec) '_Internal)
                                (equal? (first spec) '_InternalKvStore)
                                (equal? (first spec) 'sqlite))
                        (error 'storage% "Attribute symbol is not allowed: ~s" (first spec))))
       attributes))

    (define (precondition:is-valid-search-term? term)
      (when (or (srfi13-string-contains term "_")
                (srfi13-string-contains term "%"))
        (error 'storage% "Invalid search term, a search term may not contain characters '_' or '%', given ~s" term)))

    (define (precondition:is-valid-search-mode? mode)
      (unless (member mode '(starting-with substring exact))
        (error 'storage% "Search mode must be one of '(starting-with substring exact), given '~s" mode)))

    (define (precondition:is-valid-sort-order? order)
      (unless (member order '(asc desc))
        (error 'storage% "search: the sort-order keyword argument must be either 'asc or 'desc, given '~s" order)))

    (super-new)))

(module+ test
  (require rackunit)

  (define a #f)
  (define item1 #f)
  (define item2 #f)
  
  (define (setup)
    (when (file-exists? "private/testing/test1") (delete-file "private/testing/test1"))
    (when (file-exists? "private/testing/test1-shm") (delete-file "private/testing/test1-shm"))
    (when (file-exists?  "private/testing/test1-wal") (delete-file "private/testing/test1-wal"))
    (set! a (make-object storage% "private/testing/test1"))
    (send a open)
    (send a set-component 'Notes '(Title text) '(Note text) '(MoreStuff list-of text) '(SomeInt integer ) '(SomeBlob blob)))

  (define (teardown)
    (send a close))
  
  (setup)
  (check-false (send a get-latest-item 'Notes))
  (check-not-exn (lambda () (send a index 'Notes 'Note)))
  (set! item1 (send a new-item 'Notes))
  (set! item2 (send a new-item 'Notes))
  (check-not-exn (lambda () (send a close) (send a open)))
  (check-not-exn (lambda () (send a set 'Notes item1 'Note "Hello world!")))
  (check-not-exn (lambda () (send a set 'Notes item1 'SomeInt 267)))
  (check-not-exn (lambda () (send a set 'Notes item2 'SomeInt "wrong type")))
  (check-not-exn (lambda () (send a set 'Notes item2 'SomeBlob #"This is a test bytes string")))
  (check-not-exn (lambda () (send a set 'Notes item1 'Title "First title")))
  (check-not-exn (lambda () (send a set 'Notes item2 'Title "Second title")))
  (check-not-exn (lambda () (send a set 'Notes item2 'Note "aaa The last test note. world")))
  (check-equal? (send a get-latest-item 'Notes) item2)
  (check-equal? (send a get 'Notes item1 'Note) "Hello world!")
  (check-equal? (send a get 'Notes item1 'SomeInt) 267)
  (check-equal? (send a get 'Notes item2 'SomeBlob) #"This is a test bytes string")
  (check-not-exn (lambda () (send a set 'Notes item1 'MoreStuff '("This" "is" "a" "test"))))
  (check-equal? (send a get 'Notes item1 'MoreStuff) '("This" "is" "a" "test"))
  (check-not-exn (lambda () (send a set 'Notes item1 'MoreStuff '("And" "this" "is" "the" "second" "test"))))
  (check-equal? (send a get 'Notes item1 'MoreStuff) '("And" "this" "is" "the" "second" "test"))
  (check-not-exn (lambda () (send a get 'Notes item2 'MoreStuff)))
  (check-equal? (send a search-text 'Notes 'Note "llo wo" #:search-mode 'substring) (list item1))
  (check-equal? (send a search-text 'Notes 'Note "; DROP TABLE Notes;") '())
  (check-equal? (send a search-text 'Notes 'Note "world" #:search-mode 'starting-with) '())
  (check-equal? (send a search-text 'Notes 'Note "world") '())
  (check-equal? (send a search-text 'Notes 'Note "Hell") (list item1))
  (check-equal? (send a search-text 'Notes 'Note "Hello" #:search-mode 'exact) '())
  (check-equal? (send a search-text 'Notes 'Note "Hello world!" #:search-mode 'exact) (list item1))
  (check-equal? (send a search-text 'Notes 'Title "title" #:search-mode 'substring #:sort-by '(Title Note)) '(1 2))
  (check-equal? (send a search-text 'Notes 'Title "title" #:search-mode 'substring #:sort-by '(Title Note) #:sort-order 'asc) '(1 2))
  (check-equal? (send a search-text 'Notes 'Title "title" #:search-mode 'substring #:sort-by '(Title) #:sort-order 'desc) '(2 1))
  (check-equal? (send a search-text 'Notes 'Note "world" #:search-mode 'substring #:sort-by '(Note Title)) '(1 2))
  (check-true (send a contains? 'Notes item1 'Note))
  (check-false (send a contains? 'Notes item1 'SomeBlob))
  (check-not-exn (lambda () (send a kv-get 'hello #f)))
  (check-not-exn (lambda () (send a kv-get 'test (lambda () (+ 1 2)))))
  (check-not-exn (lambda () (send a kv-set 'hello '(hello world this is a list test))))
  (check-equal? (send a kv-get 'hello #f) '(hello world this is a list test))
  (check-true (send a kv-has-key? 'hello))
  (check-not-exn (lambda () (send a kv-delete 'hello)))
  (check-false (send a kv-has-key? 'hello))
  (check-true (list? (send a get-info)))
  (check-not-exn (lambda () (send a needs-maintenance?)))
  (check-equal? (send a start-maintenance (lambda () 'magic)) 'magic)
  (check-equal? (second (assoc  'trash-count  (send a get-info))) 0)
  (check-not-exn (lambda () (send a delete-item 'Notes item2)))
  (check-equal? (send a get-latest-item 'Notes) item1)
  (check-equal? (send a get-latest-item 'Notes #:deleted? #t) item2)

  (teardown)
 
  )