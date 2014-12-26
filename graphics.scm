(load "lib.scm")

(define (make-bar LEFT TOP WIDTH HEIGHT PADDING)
  (define INNER-LEFT   (+ LEFT   PADDING))
  (define INNER-TOP  (+ TOP  PADDING))
  (define INNER-WIDTH  (- WIDTH  (* 2 PADDING)))
  (define INNER-HEIGHT (- HEIGHT (* 2 PADDING)))

  (define (draw-bar colorlist from)
    (if (pair? colorlist)
      (let (
       (width (round (inexact->exact (* (cdar colorlist) INNER-WIDTH)))) 
       (color (caar colorlist)))
        (fill-rectangle! (- (+ INNER-LEFT INNER-WIDTH) from width) INNER-TOP width INNER-HEIGHT color)
        (draw-bar (cdr colorlist) (+ from width)))
      '()))

  ; (multicolor-bar [(color value)])
  ; Example: (multicolor-bar (list (cons #xf00 0.2) (cons #x0f0 0.3) (cons #x00f 0.5)))
  (lambda (colorlist) (begin
    (fill-rectangle! LEFT TOP WIDTH HEIGHT #x333)
    (draw-bar colorlist 0))))

(define (clear-screen) (fill-rectangle! 0 0 130 130 #xfff))

