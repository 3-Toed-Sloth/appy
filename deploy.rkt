#lang racket
;; Deploy - application deployment tools

(provide deploy)

(require racket/runtime-path
         compiler/distribute
         compiler/embed
         compiler/cm
         launcher/launcher
         compiler/bundle-dist
         "app.rkt"
         "gui-app.rkt"
         "inform.rkt"
         "private/generic-icon.rkt")

(define (distro-name app)
  (string-append (string-replace (get-field short-name app) " " "_")
                 "_"
                 (string-replace
                  (string-replace (get-field version app) " " "_")
                  "."
                  "-")))
  
(define/contract (deploy #:source-file [source-file (build-path (current-directory) "main.rkt")]
                         #:application [app (the-application)]
                         #:built-dir [built-dir (build-path (current-directory) "bin")]
                         #:distributions-dir [distros-dir (build-path (current-directory) "distributions")]
                         #:distribution-name [distribution-name #f]
                         #:pre-proc [pre-proc (lambda () (void))]
                         #:post-proc [post-proc (lambda (distribution-dir packed-distribution-file) (void))])
  (->* () (#:source-file (or/c path-string? path?)
           #:application (is-a?/c application%)
           #:built-dir (or/c path-string? path?)
           #:distributions-dir (or/c path-string? path?)
           #:distribution-name string?
           #:pre-proc (-> any)
           #:post-proc (-> (or/c path-string? path?) (or/c path-string? path?) any)) any)
  (parameterize ([inform-tab-spaces 4])
    (unless app
      (error 'deploy "Instead of an application #f was provided. This might indicate that the parameter the-application has not been set to a valid application% instance."))
    (with-info
      ("Starting to deploy application ~s version ~s.\n" (get-field name app) (get-field version app))
      (unless (file-exists? source-file)
        (error 'deploy "The source file does not exist: ~s." (maybe-path->string source-file)))
      (with-info
        ("1/8 Running pre-deployment scripts:\n")
        (pre-proc)
        ("1/8 Done.\n"))
      (with-info
        ("2/8 Preparing:\n")
        (unless (directory-exists? built-dir)
          (with-info*
              ("Creating built directory ~s..." (maybe-path->string built-dir))
            (make-directory* built-dir)
            ("done.\n")))
        ("2/8 Done.\n"))
      (with-info
        ("3/8 Compiling application ~s from source file ~s:\n"
         (get-field short-name app)
         (maybe-path->string source-file))
        (managed-compile-zo source-file)
        ("3/8 Done.\n"))
      (create-executable source-file built-dir distros-dir app
                         (is-a? app gui-application%)
                         (if distribution-name distribution-name (distro-name app))
                         post-proc)
      ("Done deploying ~s.\n" (get-field short-name app)))))

(define (create-executable source-file built-dir distros-dir app gui-app? distro-name post-proc)
  (letrec ((aux-dir (simplify-path (build-path built-dir "aux")))
           (app-file (simplify-path (build-path built-dir (get-field short-name app))))
           (distro-dir (build-path distros-dir (string-append (get-field name app) " " (get-field version app))))
           (icon-files #f))  
    (with-info
      ("4/8 Preparing auxiliary files:\n")
      (with-info*
          ("Creating directory ~s if necessary..." (maybe-path->string aux-dir))
        (make-directory* aux-dir)
        ("done.\n"))
      (with-info*
          ("Creating directory ~s if necessary..." (maybe-path->string distro-dir))
        (make-directory* distro-dir)
        ("done.\n"))
      (set! icon-files
            (if gui-app?
                (cond
                  ((procedure? (get-field icons app))
                   (with-info*
                       ("Creating icon files...")
                     (create-icon-files aux-dir (get-field short-name app) #:appname (get-field short-name app) #:draw-proc (get-field icons app))
                     (list (build-path aux-dir (string-append (get-field short-name app) ".png"))
                           (build-path aux-dir (string-append (get-field short-name app) ".ico")))
                     ("done.\n")))
                  (else
                   (with-info*
                       ("Icon files provided by application...")
                     (get-field icons app)
                     ("done.\n"))))
                (begin
                  (with-info*
                      ("Creating simplified icons for command-line executable...")
                    (create-icon-files aux-dir (get-field short-name app) (file-icon-sizes) 256 #:appname "Exe" #:draw-proc make-generic-icon-bitmap)
                    (list (build-path aux-dir (string-append (get-field short-name app) ".png"))
                          (build-path aux-dir (string-append (get-field short-name app) ".ico")))
                    ("done.\n")))))
      ("4/8 Done.\n"))
    (with-info
      ("5/8 Creating executable file ~s:\n" (maybe-path->string app-file))
      (create-embedding-executable
       app-file
       #:modules (list (list #f source-file))
       #:mred? gui-app?
       #:configure-via-first-module? #t
       #:expand-namespace (make-base-namespace)
       #:literal-expression
       (parameterize ([current-namespace (make-base-namespace)])
         (compile `(namespace-require '',(string->symbol (path->string (path-replace-extension (file-name-from-path source-file) #""))))))
       #:aux (build-aux-from-path (path-replace-extension (first icon-files) #"")))
      ("5/8 Done.\n"))
    (with-info
      ("6/8 Assembling distribution:\n")
      (assemble-distribution distro-dir (list app-file))
      ("6/8 Done.\n"))
    (with-info
      ("7/8 Packing distribution ~s:\n" distro-name)
      (bundle-directory distro-name
                        distro-dir
                        #t)
      ("7/8 Done.\n"))
    (with-info
      ("8/8 Running post-deployment stripts:\n")
      (post-proc distro-dir (build-path distro-dir (string-append distro-name
                                                                  "."
                                                                  (case (system-type 'os)
                                                                    ((unix) "tgz")
                                                                    ((osx) "dmg")
                                                                    (else "zip")))))
      ("8/8 Done.\n"))))
  
(define (maybe-path->string p)
  (if (path? p) (path->string p) p))

