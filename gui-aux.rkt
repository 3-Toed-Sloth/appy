#lang racket/gui

(require racket/runtime-path
         pict)
(provide wait-animation%)

(define-runtime-path circles-path "private/assets/Circles.png")

(define circles (bitmap circles-path))


(define wait-animation%
  (class horizontal-panel%

    (init-field [spinner-width 32])
    (init-field [spinner-height 32])
    
    (define to-animate #f)
    (define canvas #f)
    (define message #f)

    (field (timer #f))
    (define theta 0)
    
    (define/public (animate animate?)
      (cond
        (animate?
         (unless timer
           (set! timer (new timer%
                            [notify-callback
                             (lambda () (animate-circles))]
                            [interval 10]))
           (send timer start 25)))
        (else (when timer (send timer stop))
              (set! timer #f))))
    
    (define/public (show-message msg)
      (send message set-label msg))

    (define (animate-circles)
      (set! theta (- theta 0.06))
      (when (< theta 0)
        (set! theta 3.141592653589793))
      (send canvas on-paint)
      (send canvas refresh-now))

    (define (init)
      (set! to-animate (pict->bitmap (scale-to-fit circles spinner-width spinner-height)))
      (set! canvas (new canvas% (parent this)
                             [min-width spinner-width]
                             [min-height spinner-height]
                             [stretchable-width #f]
                             [stretchable-height #f]
                             [vert-margin 5]
                             [horiz-margin 10]
                             [paint-callback (lambda (canvas dc)
                                              (draw-rotated-bitmap dc to-animate theta 0 0))]
                             [style '(transparent no-focus)]))
                             
      (set! message (new message%
                             (parent this)
                             [stretchable-width #t]
                             [stretchable-height #f]
                             [horiz-margin 12]
                             [auto-resize #t]
                             (label ""))))

    (define (draw-rotated-bitmap dc bm theta x y)
      (let ([t (send dc get-transformation)]
            [s (send dc get-smoothing)]
            [dx (/ (send bm get-width) 2)]
            [dy (/ (send bm get-height) 2)])
        (send dc set-smoothing 'smoothed)
        (send dc set-origin (+ x dx) (+ y dy))
        (send dc set-rotation theta)
        (send dc draw-bitmap bm (- dx) (- dy))
        (send dc set-smoothing s)
        (send dc set-transformation t)))
      
    (super-new
     [stretchable-width #t]
     [stretchable-height #t]
     [alignment '(left center)])

    (init)))

(module+ test
  (require rackunit)
  (define a (new frame% [label " GUI Test: wait-animation%"]))
  (define b (new wait-animation% [parent a]))
  (send a show #t)
  (check-not-exn
   (lambda ()
  (for ([i (in-range 10)])
    (send b show-message (format "Doing something, phase ~a." i))
    (send b animate #t)
    (sleep/yield 1.0))
     (send b animate #f)
     (sleep/yield 1.0)
     (send a show #f))))