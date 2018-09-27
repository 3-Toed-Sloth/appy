#lang racket
(provide storage<%> storage?)

(define storage<%> (interface ()
                     set get add open close is-open? contains? new-item delete-item
                     set-component needs-maintenance? start-maintenance is-list-attribute?
                     kv-set kv-get kv-has-key? kv-delete get-info))
(define (storage? datum)
  (is-a? datum storage<%>))
