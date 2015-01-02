(load "lib.scm")

(define (sq x) (* x x))

(define (range min max)
  (if (< min max) (cons min (range (+ 1 min) max)) '()))

; -> f(x) -> y0 + a*(x-x0)^2
(define (parabola x0 y0 a)
  (lambda (x) (+ y0 (* a (sq (- x x0))))))

; -> f(x) -> Circle upside part
(define (semi-circle-upside cx cy r)
  (lambda (x) (+ cy (sqrt (- (sq r) (sq (- x cx)))))))

; -> f(x) -> Circle downside part
(define (semi-circle-downside cx cy r)
  (lambda (x) (- cy (sqrt (- (sq r) (sq (- x cx)))))))

; -> f(x) -> y0 + a*(x-x0)
(define (line x0 y0 a)
  (lambda (x) (+ y0 (* a (- x x0)))))

(define (draw-func color from-x to-x . funcs)
  (for-each 
    (lambda (func)
      (for-each 
        (lambda (x)
          (let ((y (round (inexact->exact (func x)))))
            (if 
              (and (< 1 y 129) (< 1 x 129))
              (fill-rectangle! (- x 1) (- y 1) 3 3 color) 
              '())))
        (range from-x to-x)))
    funcs))

(define (smile-drawer)
  (let ((previous-happiness 0))
    (lambda (happiness)
      (let ((h (* 0.03 (- happiness 0.5))))
        (if (= previous-happiness h)
          (puts "Don't redraw smile (happiness didn't change much)")
          (begin
            ; Erase previous smile
            (draw-func #xfff 10 120 (parabola 65 70 previous-happiness))
            ; Draw new one
            (draw-func #x000 10 120 (parabola 65 70 h))
            ; Store value to erase it later
            (set! previous-happiness h)))))))

(define (draw-eye cx cy)
  (for-each 
    (lambda (i) (draw-func (ash 5 i) (- cx i) (+ cx i) 
      (semi-circle-upside cx cy i)
      (semi-circle-downside cx cy i)))
    (range 2 10)))

(define (make-bar LEFT TOP WIDTH HEIGHT PADDING color)
  (let ((INNER-LEFT   (+ LEFT PADDING))
        (INNER-TOP    (+ TOP  PADDING))
        (INNER-WIDTH  (- WIDTH  (* 2 PADDING)))
        (INNER-HEIGHT (- HEIGHT (* 2 PADDING)))
        (previous-width 0))
    (lambda (value)
      (begin
        (set! previous-value value)
        (let ((width (round (inexact->exact (* value INNER-WIDTH)))))
          (if (= previous-width width) '() (begin
            (set! previous-width width)
            (fill-rectangle! LEFT TOP WIDTH HEIGHT #x333) ; background
            (fill-rectangle! (- (+ INNER-LEFT INNER-WIDTH) width) INNER-TOP width INNER-HEIGHT color))))))))

(define (draw-Z x0 y0 scale) (begin
  (draw-func #x00f x0 (+ x0 scale)
    (line x0 (+ y0 scale) 0)
    (line x0 y0 0)
    (line (+ x0 scale) y0 -1))))

(define (draw-sleepZ) (begin
  (draw-Z 80  90 24)
  (draw-Z 67 110 12)
  (draw-Z 60 120  6)
  (draw-Z 56 124  3)))

(define (draw-eyes) (begin (draw-eye 33 110) (draw-eye 97 110)))
(define (clear-eyes) (fill-rectangle! 23 88 120 42 #xfff))

(define draw-rest (make-bar 5 5 120 5 1 #x00f))
(define draw-submission (make-bar 5 11 120 5 1 #xf0f))
(define draw-food (make-bar 5 17 120 5 1 #xf00))
(define draw-health (make-bar 5 23 120 5 1 #x0f0))
(define draw-happiness (smile-drawer))

(define (clear-screen) (fill-rectangle! 0 0 130 130 #xfff))

(init-lcd)
