(load "lib.scm")

(define (sq x) (* x x))

(define (range min max)
  (if (< min max) (cons min (range (+ 1 min) max)) '()))

; -> f(x) -> y0 + a*(x-x0)^2
(define (parabola x0 y0 a)
  (lambda (x) (+ y0 (* a (sq (- x x0))))))

;
(define (semi-circle-upside cx cy r)
  (lambda (x) (+ cy (sqrt (- (sq r) (sq (- x cx)))))))

(define (semi-circle-downside cx cy r)
  (lambda (x) (- cy (sqrt (- (sq r) (sq (- x cx)))))))

(define (draw-func func color from-x to-x)
  (for-each 
    (lambda (x)
      (let ((y (round (inexact->exact (func x)))))
        (if 
          (and (< 1 y 129) (< 1 x 129))
          (fill-rectangle! (- x 1) (- y 1) 3 3 color) 
          '())))
    (range from-x to-x)))

(define (smile-drawer)
  (let ((previous-happiness 0))
    (lambda (happiness)
      (let ((h (* 0.03 (- happiness 0.5))))
        (if (= previous-happiness h) '()
          (begin
            ; Erase previous smile
            (draw-func (parabola 65 70 previous-happiness) #xfff 10 120)
            ; Draw new one
            (draw-func (parabola 65 70 h) #x000 10 120)
            ; Store value to erase it later
            (set! previous-happiness h)))))))

(define (draw-eye cx cy)
  (let ((r-outer 9) (r-inner 2)) (begin
    (draw-func (semi-circle-upside cx cy r-outer) #x333 (- cx r-outer) (+ cx r-outer))
    (draw-func (semi-circle-upside cx cy r-inner) #x333 (- cx r-inner) (+ cx r-inner))
    (draw-func (semi-circle-downside cx cy r-outer) #x333 (- cx r-outer) (+ cx r-outer))
    (draw-func (semi-circle-downside cx cy r-inner) #x333 (- cx r-inner) (+ cx r-inner)))))

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

(define draw-rest (make-bar 5 5 120 5 1 #x00f))
(define draw-submission (make-bar 5 11 120 5 1 #xf0f))
(define draw-food (make-bar 5 17 120 5 1 #xf00))
(define draw-health (make-bar 5 23 120 5 1 #x0f0))
(define draw-happiness (smile-drawer))

(define (clear-screen) (fill-rectangle! 0 0 130 130 #xfff))
