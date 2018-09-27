#lang racket

(require racket/draw
         pict
         file/ico
         images/icons/style
         images/icons/symbol
         images/icons/control
         identikon)

(provide make-generic-icon-bitmap bitmap->ico file-icon-sizes create-icon-files)

(define file-icon-sizes (make-parameter '(16 20 24 32 40 48 64 96 128 256)))

(define/contract (make-generic-icon-bitmap size [name #f])
  (->* ((and/c rational? (>=/c 0))) ((or/c boolean? (string-len/c 32))) any)
  (pict->bitmap
   (cc-superimpose
    (bitmap (record-icon #:color "cornflowerblue" #:height size
                         #:material glass-icon-material))

    (bitmap (text-icon (if name (substring name 0 1) "Appy")
                       #:color "red" #:height (exact-round (* size 0.5))
                       #:material metal-icon-material
                       #:trim? #t)))))

(define (bitmap->ico b)
  (define w (send b get-width))
  (define h (send b get-height))
  (define d (send b get-depth))
  (define buffer (make-bytes (* w h 4))) 
  (send b get-argb-pixels 0 0 w h buffer)
  (define sb (make-object bitmap% h h #f #t))
  (define offset (max (exact-round (/ (- h w) 2)) 0))
  (send sb set-argb-pixels offset 0 w h buffer)
  (define new-buffer (make-bytes (* h h 4)))
  (send sb get-argb-pixels 0 0 h h new-buffer)
  (argb->ico h h new-buffer #:depth d))

(define (create-icon-files target-folder filename [sizes (file-icon-sizes)] [highres-size 128] #:appname [appname "Appy"] #:draw-proc [draw-proc make-generic-icon-bitmap])
  (make-directory* target-folder)
  (define png-path (build-path target-folder (string-append filename ".png")))
  (define ico-path (build-path target-folder (string-append filename ".ico")))
  (when (file-exists? png-path) (delete-file png-path))
  (when (file-exists? ico-path) (delete-file ico-path))
  (define highres-icon (draw-proc highres-size appname))
  (send highres-icon save-file png-path 'png)
  (write-icos (for/list ([size (in-list sizes)])
                (bitmap->ico (draw-proc size appname)))
              ico-path))
    
(module+ test
  (require rackunit)
  (check-not-exn (lambda () (make-generic-icon-bitmap 48)))
  (check-not-exn (lambda () (bitmap->ico (make-generic-icon-bitmap 32)))))

(module+ main
  (create-icon-files "testing/icons" "application-icon"))

