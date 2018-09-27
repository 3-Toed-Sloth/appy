#lang racket
(require racket/gui
         "../../gui.rkt")

(provide the-application run)

(define my-app%
  (class gui-application%

    (super-new
     [name "Hello World Example"]
     [short-name "Hello"]
     [version "1.0"]
     [open-procs '()]
     [run-procs (list (lambda (app) (message-box "Example Dialogue" "Hello, world!") (send app close)))]
     [close-procs '()])))

(the-application (make-object my-app%))

(define (run)
  (send (the-application) open)
  (send (the-application) run))
