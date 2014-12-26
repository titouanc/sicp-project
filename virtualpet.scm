(load "fsm.scm")
(load "lib.scm")
(load "graphics.scm")

;;; Pinout ;;;
(define x-pin (pin 16)) ; x accel axis
(define y-pin (pin 17)) ; y accel axis

(define but-g-pin (pin 23)) ; Button below green led
(define but-y-pin (pin 24)) ; Button below yellow led
(define but-r-pin (pin 25)) ; Button below red led

(define led-g-pin (pin 18)) ; Green led
(define led-y-pin (pin 19)) ; Yellow led
(define led-r-pin (pin 10)) ; Red led

; Led-Button association
(define ledbuttons (list
    (list 'yellow but-y-pin led-y-pin)
    (list 'red    but-r-pin led-r-pin)
    (list 'green  but-g-pin led-g-pin)))

(define ledname car)
(define but-pin cadr)
(define led-pin caddr)

(define (show color)
  (set-pin! (led-pin (assoc color ledbuttons))))

(define (hide color)
  (clear-pin! (led-pin (assoc color ledbuttons))))
;;; /Pinout ;;;



;;; Aptitudes ;;;
(define (is-full aptitude) (aptitude >= 1))
(define (is-empty aptitude) (aptitude <= 0))

(define happiness 0.75)
(define submission 0.75)
(define food 0.75)
(define health 0.75)
(define rest 0.75)

(define (draw-aptitudes) (begin
  ((make-bar 5 5 120 5 1)  (list (cons #xff0 happiness)))
  ((make-bar 5 11 120 5 1) (list (cons #xf0f submission)))
  ((make-bar 5 17 120 5 1) (list (cons #xf00 food)))
  ((make-bar 5 23 120 5 1) (list (cons #x0f0 health)))
  ((make-bar 5 29 120 5 1) (list (cons #x00f rest)))))

(define playing-color 'nocolor)
(define (choose-color)
  (set! playing-color (ledname ([] ledbuttons (rand 3)))))
;;; /Aptitudes ;;;



;;; Helpers ;;;
(define (puts text) (begin (display text) (newline)))
(define (remap from-min from-max to-min to-max)
  (lambda (val)
    (+ to-min (* (- to-max to-min) (/ (- val from-min 0.0) (- from-max from-min))))))

(define (get-x) ((remap 110 35500 -1 1) (pulse_in x-pin)))
(define (get-y) ((remap 110 35500 -1 1) (pulse_in y-pin)))

(define (rand n) (modulo (pulse_in x-pin) 3))

; Milliseconds counter
(define milliseconds timer0)

; Uptime in ms
(define (millis) (read-timer milliseconds))

(define (wait-until timestamp)
  (if (< (millis) timestamp) (wait-until timestamp) '()))

(define (delayms ms)
  (wait-until (+ ms (millis))))

; -> l[i] (i = 0..len(l)-1)
(define ([] l i)
  (if (pair? l)
    (if (> i 0)
      ([] (cdr l) (- i 1))
      (car l))
    l))

; Time since last transition
(define last-transition-time 0)
(define (input-func func) (lambda () (begin
  (draw-aptitudes)
  (set! last-transition-time (millis))
  (func))))

; Decorate make-state from fsm with the input function updating last-transition-time
(define (make-state in-func out-func . transitions)
  ; Concat variable arguments
  (let ((args (cons (input-func in-func) (cons out-func transitions))))
    (apply make-fsm-state args)))
;;; /Helpers ;;;



;;; States ;;;
(define sleeping-state (make-state
  (lambda () (begin
    (puts "Going to sleep ZzzzZZzzz...")
    (fill-rectangle! 0 0 130 130 #x000)))
  (lambda () (begin
    (display "Finished sleep ! Rest =")
    (display rest)
    (let ((dt (- (millis) last-transition-time)))
      (set! rest (min 1.0 (+ rest (/ dt 3600000.0)))))
    (display "Rest is now =")
    (display rest)
    (fill-rectangle! 0 0 130 130 #xfff)))))

(define awake-state (make-state
  (lambda () (begin
    (show 'red)
    (puts "Awake !!!")))
  (lambda () (begin
    (hide 'red)
    (puts "No more awake")))))

(define playing-state (make-state
  (lambda () (begin
    (choose-color)
    (display "Playing")
    (puts playing-color)))
  (lambda () (begin
    (show playing-color)
    (delayms 250)
    (hide playing-color)
    (puts "Finished playing")))))

; Wake up if acceleration is significant
(sleeping-state 'add-transition (make-fsm-transition
  'accel (lambda (xy) (> (apply max (apply abs xy)) 0.05)) awake-state))

; Go to sleep if horizontal
(awake-state 'add-transition (make-fsm-transition
  'accel (lambda (xy) (< (apply max (apply abs xy)) 0.025)) sleeping-state))

; Play if green button pressed
(awake-state 'add-transition (make-fsm-transition
  'green (lambda (active) active) playing-state))

; Stop playing if correct button pressed
(for-each (lambda (ledbut)
  (let ((color (ledname ledbut)))
    (playing-state 'add-transition (make-fsm-transition
      color 
      (lambda (active) (and active (string=? playing-color color)))
      awake-state))))
  ledbuttons)


(define boot-state (make-state
  (lambda () (puts "Boot..."))
  (lambda () (begin
    ; Set I/O directions
    (map (lambda (ledbut) (begin
      (set-input-pin! (but-pin ledbut))
      (set-output-pin! (led-pin ledbut))))
    ledbuttons)
    ; https://github.com/RaD/ArmpitScheme/blob/master/mcu_specific/LPC_2000/LPC_H2214/board.h#L52
    (write-timer-period milliseconds 58982)
    (start-timer milliseconds)
    (clear-screen)
    (puts "Initialised")))
  (make-fsm-transition 'start always awake-state)))
;;; /States ;;;


(define (make-virtual-pet) (begin
  (define res (make-finite-state-machine boot-state))
  (res 'feed-context (list (cons 'start #t)))
  res))
