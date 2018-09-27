#lang racket/gui

(require  "lang.rkt"
          "lang-defaults.rkt"
          "app.rkt"
          "prefs.rkt"
          "gui-aux.rkt"
          "private/generic-icon.rkt")

(provide gui-application%)

(define (ask-error exn app title msg button1 button2)
  (message-box/custom
   title
   msg button1 button2
   #f
   #f
   '(caution disallow-close default=1)
   1))

(define/contract gui-application%
  (class/c
   (inherit-field [name valid-application-name?])
   (inherit-field [short-name valid-application-name?])
   (inherit-field [version valid-version-string?])
   (inherit-field [open-procs (listof (-> (is-a?/c application%) any))])
   (inherit-field [run-procs (listof (-> (is-a?/c application%) any))])
   (inherit-field [close-procs (listof (-> (is-a?/c application%) any))])
   (inherit-field [exception-handler (-> (is-a?/c application%) exn? any)])
   (inherit-field [command-line-handler (-> (is-a?/c application%) any)])
   (inherit-field [pref-dir (or/c path? path-string?)])
   (inherit-field [maintenance-start-proc (-> string? any)])
   (inherit-field [maintenance-end-proc (-> string? any)])
   (inherit-field [hardcoded-translations (listof (or/c path? path-string?))])
   (init-field (icons (or/c (->* (positive?) ((or/c string? #f)) (is-a?/c bitmap%))
                            (*list/c (or/c path? path-string?)
                                    (or/c path? path-string?)
                                    (or/c path? path-string?)))))
   (init-field [preferences-handler (-> any)])
   (init-field [about-handler (-> any)])
   [open-file (->m path? any)]
   [new-document (->m any)]
   [edit-preferences (->m any)])
   
  (class application%
    (inherit-field name)
    (inherit-field short-name)
    (inherit-field version)
    (inherit-field open-procs)
    (inherit-field run-procs)
    (inherit-field close-procs)
    (inherit-field exception-handler)
    (inherit-field pref-dir)
    (inherit-field maintenance-start-proc)
    (inherit-field maintenance-end-proc)
    (inherit-field command-line-handler)
    (inherit-field hardcoded-translations)
    (init-field [icons make-generic-icon-bitmap])
    (init-field (preferences-handler (lambda () (send this edit-preferences))))
    (init-field (about-handler (lambda ()
                                 (message-box (appy-about-title short-name)
                                              "An application made with APPY, the ectlectic application framework for Racket."
                                              '(ok)))))
    (init (open-file-handler (lambda (app file) (send app open-file file))))
    (init (start-empty-handler (lambda (app) (send this new-document))))

    ;; Dummy methods - to override (not abstract for easier testing, but useless as they are now)
    (define/public (open-file file)
      (displayln (format "Open-file: ~s. You need to override this method to do something useful with the file." file)))

    (define/public (new-document)
      (displayln "New-document method: You need to override this method to do something useful."))

    (define/public (edit-preferences)
      (displayln "Edit-preferences method: You need to override this method to do show a preferences dialog."))

    ;; Private
    (define (init)
      (init-language-system #:application-name name #:hardcoded-paths hardcoded-translations)
      (application-file-handler (lambda (file) (open-file-handler this file)))
      (application-start-empty-handler (lambda () (start-empty-handler this)))
      (application-quit-handler (lambda () ((get-field shutdown-handler this) this #f)))
      (application-preferences-handler preferences-handler)
      (application-about-handler about-handler))

    (super-new
     [exception-handler 
       (lambda (app exn)      
         (case (ask-error exn app
                          (format "~a ~a" (get-field short-name app) (appy-Error))
                          (appy-save-and-quit-message (exn-message exn))
                          (appy-Save-and-Quit)
                          (appy-Continue))
           ((1) (send app shutdown exn))
           ((2) (send app shutdown exn) (send app open))
           (else (send app shutdown exn))))]
     [open-file-handler open-file-handler]
     [start-empty-handler start-empty-handler]
     [shutdown-handler (lambda (app exn) (send app close) #t)]
     [maintenance-start-proc
      (lambda (msg)
        (show-maintenance-dialog this
                                 (appy-routine-maintenance-message short-name))
        (show-maintenance-dialog msg))]
     [maintenance-end-proc
      (lambda (msg)
        (show-maintenance-dialog msg)
        (show-maintenance-dialog))])

     (init)))

(define maintenance-dialog%
  (class frame%
    (inherit show)
    (init-field (vpanel #f))
    (init-field (infocanvas #f))
    (init-field (infotext #f))
    (init-field (anim #f))

    (define/public (show-message msg [animate? #t])
      (send anim show-message msg)
      (send anim animate animate?))

    (define/public (show-general-info msg)
      (send infotext erase)
      (send infotext insert msg))

    (define/public (close)
      (send anim animate #f)
      (send
       (new timer%
            [notify-callback (lambda () 
                               (send anim show-message "")
                               (show #f))]
            [just-once? #t]) start 3000))

    (define (init)
      (set! vpanel (new vertical-panel%
                        [parent this]
                        [alignment '(left top)]))
      (set! infotext (new text%
                          [auto-wrap #t]))
      (set! infocanvas (new editor-canvas%
                            [parent vpanel]
                            [editor infotext]
                            [horiz-margin 12]
                            [vert-margin 12]
                            [stretchable-width #t]
                            [stretchable-height #t]
                            [style '(transparent no-border no-focus hide-hscroll auto-vscroll)]
                            [label #f]))
    
      (set! anim (new wait-animation%
                      [parent vpanel])))

    (super-new
     [width 300]
     [height 180]
     [style '(no-system-menu no-resize-border)])

    (init)
    ))

(define dlg #f)
(define show-maintenance-dialog
  (case-lambda
    (() (send dlg close))
    ((app msg)
     (unless dlg
       (set! dlg (new maintenance-dialog%
                      [label (format "~a ~a" (get-field short-name app) (appy-Maintenance))])))
     (send dlg show-general-info msg))
    ((msg)
     (send dlg show-message msg #t)
     (send dlg show #t))
    ))

(module+ test
  (require rackunit)
  
  (define a #f)

;  (current-command-line-arguments #("-o" "test/sample"))

  (check-not-exn (lambda () (set! a (new gui-application%
                                         [name "Test Application"]
                                         [short-name "TestApp"]
                                         [version "1.0.0"]
                                         [language 'de_DE]
                                         [open-procs (list (lambda (app) (void)))]
                                         [run-procs (list (lambda (app) (void)))]
                                         [close-procs (list (lambda (app) (void)))]
                                         [icons make-generic-icon-bitmap]))))

  (check-not-exn (lambda () (send a open)))
 ; (check-not-exn (lambda () (send a open))) ; causes GUI error dialog - hard to test
  (check-not-exn (lambda () (send a close))) )

; (current-command-line-arguments #("-o" "test/sample"))
;(define a (new gui-application%
;                                         [name "Test Application"]
;                                         [short-name "TestApp"]
;                                        [version "1.0"]
;                                         [language 'de_DE]
;                                         [open-procs (list (lambda (app) (void)))]
;                                         [run-procs (list (lambda (app) (void)))]
;                                         [close-procs (list (lambda (app) (void)))]
;                                         [icons (list #f #f #f)]))
;
;(send a open)
;;(send a open)