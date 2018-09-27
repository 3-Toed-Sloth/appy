#lang racket
(require "../../deploy.rkt"
         "application.rkt")

;; Example deployment, notice that we use the implicit application argument that defaults
;; to (the-application), which is defined in "application.rkt" but that the application is started in
;; "main.rkt". Using a separate starter module is recommended.
(deploy)
