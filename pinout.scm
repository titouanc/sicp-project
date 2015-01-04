(load "lib.scm")

(define x-pin (pin 17)) ; x accel axis
(define y-pin (pin 16)) ; y accel axis

(define but-g-pin (pin 23)) ; Button below green led
(define but-y-pin (pin 24)) ; Button below yellow led
(define but-r-pin (pin 25)) ; Button below red led

(define led-g-pin (pin 18)) ; Green led
(define led-y-pin (pin 19)) ; Yellow led
(define led-r-pin (pin 10)) ; Red led

; Led-Button association
(define ledbuttons (list
  (list 'green  but-g-pin led-g-pin)
  (list 'yellow but-y-pin led-y-pin)
  (list 'red    but-r-pin led-r-pin)))

; Led-button getters
(define ledname car)
(define but-pin cadr)
(define led-pin caddr)

; Light named led
(define (show . colors)
  (for-each (lambda (color) (set-pin! (led-pin (assoc color ledbuttons)))) colors))

; Shadow named led
(define (hide . colors)
  (for-each (lambda (color) (clear-pin! (led-pin (assoc color ledbuttons)))) colors))

; -> f(x in [from-min..from-max]) -> y in [to-min..to-max]
(define (remap from-min from-max to-min to-max)
  (lambda (val)
    (+ to-min (* (- to-max to-min) (/ (- val from-min) (- from-max from-min))))))

; Accelerometer getters
(define (get-x) ((remap 10000 26000 -1.0 1.0) (pulse_in x-pin)))
(define (get-y) ((remap 10000 26000 -1.0 1.0) (pulse_in y-pin)))

(define (animate-leds) (begin
  (for-each 
    (lambda (ledbut) (begin
      (set-pin! (led-pin ledbut)) 
      (delayms 50) 
      (clear-pin! (led-pin ledbut)) 
      (delayms 50)))
    ledbuttons)
  #f))
