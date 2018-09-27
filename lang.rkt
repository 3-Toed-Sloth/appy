#lang racket
;; Lang - localization support

(require racket/path
         racket/runtime-path
         srfi/13)

(provide tr load-language list-available-languages language-available? current-localization-folder current-translations current-language
         define-localizations init-language-system check-translations write-translation-template)

(define current-language (make-parameter 'en_US))
(define current-translations (make-parameter #f))
(define current-localization-folder (make-parameter #f))

;; PUBLIC PART

(define (load-language lang)
  (precondition:initialized?)
  (cond
    ((file-exists? (get-path-for lang))
     (current-translations (make-hash))
     (with-input-from-file (get-path-for lang)
       (lambda ()
         (call-with-default-reading-parameterization
          (lambda ()
            (for ([translation (in-port)])
              (hash-set! (current-translations) (first translation) (second translation)))))))
     (current-language lang))
    (else
     (displayln
      (format "APPY Warning: The localization for language ~s was not found. Translations-folder=~a."
              lang (path->string (current-localization-folder)))
      (current-error-port))
     (unless (equal? lang 'en_US)
       (display "Switching to default language 'en_US..."(current-error-port))
       (load-language 'en_US)
       (displayln "done." (current-error-port))))))

(define (list-available-languages)
  (precondition:initialized?)
  (map
   string->symbol
   (sort 
    (filter
     (lambda (spec)
       (non-empty-string? (string-trim spec)))
     (for/list
         ([file (in-directory (current-localization-folder)
                              (lambda (dir)
                                (and (file-exists? dir)
                                     (path-has-extension? dir #".loc"))))])
       (first (string-split (path->string (file-name-from-path file)) "."))))
    string<?)))

(define (language-available? lang)
  (if (member lang (list-available-languages)) #t #f))

(define *translations* (make-hash))

(define-syntax-rule (defloc key value)
  (begin
    (define (key . args) (apply tr (cons 'key args)))
    (add-key 'key value)))

(define-syntax-rule (define-localizations (key value) ...)
  (begin (defloc key value)
         ...))
        
(define (tr key . args)
  (if (empty? args)
      (translate key)
      (apply format (cons (translate key) args))))

(define (init-language-system #:application-name [appname "appy"] #:hardcoded-paths [hardcoded-paths '()]
                              #:write-templates? [write-templates? #f])
  (current-localization-folder (build-path (find-system-path 'pref-dir) appname "app-data" "localizations"))
  (for ([src (in-list hardcoded-paths)])
    (unless (file-exists? (build-path (current-localization-folder) (file-name-from-path src)))
      (copy-file src (build-path (current-localization-folder) (file-name-from-path src)))
      (when write-templates?
        (write-translation-template (string->symbol (first (string-split (path->string (file-name-from-path src)) ".")))))))
  (make-directory* (current-localization-folder))
  (precondition:valid-translations-folder? (current-localization-folder))
  (write-keys)
  (load-language 'en_US))

(define (write-translation-template lang)
  (define current (current-language))
  (load-language 'en_US)
  (let ((basis (current-translations))
        (target (cond
                  ((file-exists? (get-path-for lang))
                   (load-language lang)
                   (current-translations))
                  (else (make-hash)))))
    (with-output-to-file
        (get-path-for lang)
      (lambda ()
        (display ";; This file is a template for language ")
        (displayln lang)
        (displayln ";; Translate all the strings from the comment above the entries to the value in the key value list below it")
        (displayln ";; Leave the key untouched.")
        (newline)
        (for ([(k v) (in-hash basis)])
          (display ";; Translation for: ")
          (writeln v)
          (if (hash-has-key? target k)
              (writeln (list k (hash-ref target k)))
              (writeln (list k "")))
          (newline)))
      #:exists 'replace
      #:mode 'text))
  (if current
      (load-language current)
      (current-language current)))

(define (check-translations)
  (define languages (list-available-languages))
  (define current (current-language))
  (load-language 'en_US)
  (define en (current-translations))
  (for ([language (in-list (filter (lambda (x) (not (equal? x 'en_US))) languages))])
    (load-language language)
    (check-translation-keys (current-language) en (current-translations)))
  (load-language current))

;; PRIVATE PART
(define (translate key)
  (hash-ref (current-translations)
            key
            (lambda () (error 'lang "The localization for key '~s was not found in language '~s." key (current-language)))))

(define (get-path-for lang)
  (build-path (current-localization-folder) (string-append (symbol->string lang) ".loc")))

(define (check-translation-keys lang t1 t2)
  (for ([(k v) (in-hash t1)])
    (unless (hash-has-key? t2 k)
      (error 'check-translations "The location for key '~s is missing in language '~s." k lang))
    (when (and (non-empty-string? (hash-ref t1 k))
               (not (non-empty-string? (hash-ref t2 k))))
      (displayln (format "Warning: The string for key '~s in language '~s is empty, but not empty for 'en_US." k lang)
                 (current-error-port)))
    (when (not (= (string-count (hash-ref t1 k) #\~) (string-count (hash-ref t2 k) #\~)))
      (displayln (format "Warning: The string for key '~s in language '~s seems to have ~a format arguments whereas in 'en_US it seems to have ~a."
                         k
                         lang
                         (string-count (hash-ref t2 k) #\~)
                         (string-count (hash-ref t1 k) #\~))
                 (current-error-port)))))

(define (write-keys)
  (with-output-to-file
      (get-path-for 'en_US)
    (lambda ()
      (displayln ";; APPY Language Template for US English")
      (displayln ";; This file is auto-generated, do not change its contents! The command (init-language-system appname) overwrites this file.")
      (displayln ";; Use define-localizations followed by init-language-system for defining en_US file that serves as a basis.")
      (displayln ";; Use (check-translations) before deployment to check the completeness of your translation files.")
      (displayln ";; Use (write-translation-template lang) to create a template for language lang, abbreviated by symbols as above.")
      (newline)
      (for ([(k v) (in-hash *translations*)])
        (writeln (list k v))))
    #:exists 'replace
    #:mode 'text))

(define (add-key key value)
  (hash-set! *translations* key value)
  (current-translations *translations*)
  (current-language 'en_US))

;; Preconditions
(define (precondition:initialized?)
  (unless (current-localization-folder)
    (error 'lang "Attempt to use the localization system without prior initialization. You need to use (init-language-system) first.")))

(define (precondition:valid-translations-folder? f)
  (unless (directory-exists? f)
    (error 'lang "The translations directory does not exist, path=~s" f)))

(define (precondition:source-dir-exists? d)
  (unless (directory-exists? d)
    (error 'lang "The source directory for (generate-template source-dir) does not exist, given path=~s" d)))

(module+ test
  (require rackunit)

  (define (del p)
    (when (file-exists? p) (delete-file p)))
  
  ;; set up tests
  (define testdir (build-path (find-system-path 'pref-dir) "appy-testing" "app-data" "localizations"))
  (make-directory* testdir)
  (del (build-path testdir "en_US.loc"))
  (del (build-path testdir "de_DE.loc"))
  (del (build-path testdir "fr_FR.loc"))
  (del (build-path testdir "xx_XX.loc"))
  (with-output-to-file (build-path testdir "de_DE.loc")
    (lambda ()
      (writeln '(test1 "de test 1"))
      (writeln '(test2 "de test 2: ~a"))
      (writeln '(test3 "de test 3: ~a ~s"))
      (writeln '(test4 "de der Mond"))))
  (with-output-to-file (build-path testdir "fr_FR.loc")
    (lambda ()
      (writeln '(test1 "fr test 1"))
      (writeln '(test2 "fr test 2: ~a"))
      (writeln '(test3 "fr test 3: ~a ~s"))
      (writeln '(test4 "fr la lune"))))

  ;; actual tests
  (check-not-exn (lambda () (define-localizations
                              (test1 "en test 1")
                              (test2 "en test 2: ~a")
                              (test3 "en test 3: ~a ~s")
                              (test4 "en the Moon"))))
  (check-not-exn (lambda () (init-language-system #:application-name "appy-testing")))
  (check-equal? (list-available-languages) '(de_DE en_US fr_FR))
  (check-equal? (tr 'test1) "en test 1")
  (check-equal? (tr 'test2 'hello) "en test 2: hello")
  (check-equal? (tr 'test3 'hello "world") "en test 3: hello \"world\"")
  (check-equal? (tr 'test4) "en the Moon")
  (check-not-exn (lambda () (load-language 'de_DE)))
  (check-equal? (tr 'test1) "de test 1")
  (check-equal? (tr 'test2 'hello) "de test 2: hello")
  (check-equal? (tr 'test3 'hello "world") "de test 3: hello \"world\"")
  (check-equal? (tr 'test4) "de der Mond")
  (check-not-exn (lambda () (load-language 'en_US)))
  (check-equal? (tr 'test1) "en test 1")
  (check-equal? (tr 'test2 'hello) "en test 2: hello")
  (check-equal? (tr 'test3 'hello "world") "en test 3: hello \"world\"")
  (check-equal? (tr 'test4) "en the Moon")
  (check-not-exn (lambda () (load-language 'fr_FR)))
  (check-equal? (tr 'test1) "fr test 1")
  (check-equal? (tr 'test2 'hello) "fr test 2: hello")
  (check-equal? (tr 'test3 'hello "world") "fr test 3: hello \"world\"")
  (check-equal? (tr 'test4) "fr la lune")
  (check-not-exn (lambda () (check-translations)))
  (check-not-exn (lambda () (write-translation-template 'xx_XX)))
  (check-true (file-exists? (build-path testdir "xx_XX.loc")))
  (displayln "-------------------------------------------------------------------------" (current-error-port))
  (displayln "APPY: The following warnings are intended and part of testing \"lang.rkt\":" (current-error-port))
  (check-not-exn (lambda () (check-translations)))
  (displayln "-------------------------------------------------------------------------" (current-error-port))

  (del (build-path testdir "en_US.loc"))
  (del (build-path testdir "de_DE.loc"))
  (del (build-path testdir "fr_FR.loc"))
  (del (build-path testdir "xx_XX.loc"))
  )

