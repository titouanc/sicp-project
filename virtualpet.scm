(load "fsm.scm")
(load "lib.scm")
(load "graphics.scm")

;;; Pinout ;;;
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

(define ledname car)
(define but-pin cadr)
(define led-pin caddr)

(define (show color)
  (set-pin! (led-pin (assoc color ledbuttons))))

(define (hide color)
  (clear-pin! (led-pin (assoc color ledbuttons))))
;;; /Pinout ;;;



;;; Aptitudes ;;;
(define TIME-DIV 3600000.0)

; Return x times incremented aptitude based on time since last change
(define (incx aptitude x) (min 1.0 (+ aptitude (* x (/ (state-uptime) TIME-DIV)))))
(define (inc aptitude) (incx aptitude 1))

; Return x times decremented aptitude based on time since last change
(define (decx aptitude x) (max 0.0 (- aptitude (* x (/ (state-uptime) TIME-DIV)))))
(define (dec aptitude) (decx aptitude 1))

(define happiness 0.75)
(define submission 0.75)
(define food 0.75)
(define health 0.75)
(define rest 0.75)

(define (draw-aptitudes) (begin
  (draw-happiness happiness)
  (draw-rest rest)
  (draw-submission submission)
  (draw-food food)
  (draw-health health)
))

(define playing-color 'nocolor)
(define (choose-color)
  (set! playing-color (ledname ([] ledbuttons (rand 3)))))
;;; /Aptitudes ;;;



;;; Helpers ;;;
(define (puts . texts) (begin (map display texts) (newline)))
(define (remap from-min from-max to-min to-max)
  (lambda (val)
    (+ to-min (* (- to-max to-min) (/ (- val from-min) (- from-max from-min))))))

(define (get-x) ((remap 10000 26000 -1.0 1.0) (pulse_in x-pin)))
(define (get-y) ((remap 10000 26000 -1.0 1.0) (pulse_in y-pin)))

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

; Timestamp of last transition
(define last-transition-time 0)
(define (input-func func) (lambda () (begin
  (draw-aptitudes)
  (set! last-transition-time (millis))
  (func))))

(define (state-uptime) (- (millis) last-transition-time))

; Decorate make-state from fsm with the input function updating last-transition-time
(define (make-state in-func out-func . transitions)
  ; Concat variable arguments
  (let ((args (cons (input-func in-func) (cons out-func transitions))))
    (apply make-fsm-state args)))

(define (<f a b) (< (inexact->exact a) (inexact->exact b)))
(define (>f a b) (> (inexact->exact a) (inexact->exact b)))
;;; /Helpers ;;;



;;; States ;;;
(define sleeping-state (make-state
  (lambda () (begin
    (puts "Going to sleep ZzzzZZzzz...")
    (fill-rectangle! 0 0 130 130 #x000)))
  (lambda () (begin
    (set! rest (inc rest 2))
    (puts "Finished sleep. Rest is now =" rest)
    (fill-rectangle! 0 0 130 130 #xfff)))))

(define awake-state (make-state
  (lambda () (begin (show 'red) (puts "Awake !!!")))
  (lambda () (begin 
    (hide 'red)
    (set! rest (dec rest))
    (set! food (dec food))
    (puts "No more awake")))))

(define FLIP-THRES-RISE 0.5)
(define FLIP-THRES-FALL 0.3)
(define FLIP-MS 1500)

(define awake-flipped-state (make-state 
  (lambda () (begin (puts "Awake flipped") (show 'yellow)))
  (lambda () (hide 'yellow))
  (make-fsm-transition 'accel-y (lambda (y) (begin
    (and (<f y FLIP-THRES-FALL) (> (state-uptime) FLIP-MS)))) sleeping-state)
  (make-fsm-transition 'accel-y (lambda (y) (<f y FLIP-THRES-FALL)) awake-state)))

(awake-state 'add-transition (make-fsm-transition
  'accel-y (lambda (y) (>f y FLIP-THRES-RISE)) awake-flipped-state))

(define sleeping-flipped-state (make-state 
    (lambda () (begin (puts "Sleeping flipped") (show 'yellow)))
    (lambda () (hide 'yellow))
    ; After a certain time -> awake
    (make-fsm-transition 'accel-y (lambda (y) (begin
      (and (<f y FLIP-THRES-FALL) (> (state-uptime) FLIP-MS)))) awake-state)
    ; Otherwise go back to sleep
    (make-fsm-transition 'accel-y (lambda (y) (<f y FLIP-THRES-FALL)) sleeping-state)))

(sleeping-state 'add-transition (make-fsm-transition
  'accel-y (lambda (y) (>f y FLIP-THRES-RISE)) sleeping-flipped-state))

(define playing-state (make-state
  (lambda () (begin
    (puts "Entering playing-state...")
    (choose-color)
    (puts "Choosed color:" playing-color)
    (for-each 
      (lambda (ledbut) (begin
        (puts ledbut)
        (set-pin! (led-pin ledbut)) 
        (delayms 50) 
        (clear-pin! (led-pin ledbut)) 
        (delayms 50)))
      ledbuttons)
    (puts "Playing" playing-color)))
  (lambda () (begin
    (show playing-color)
    (delayms 250)
    (hide playing-color)
    ; If found in a short time, increase happiness
    (if (< (state-uptime) 750) (set! happiness (min 1.0, (+ happiness 0.1))))
    (puts "Finished playing")))))

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
    (for-each (lambda (ledbut) 
      (begin
        (set-input-pin! (but-pin ledbut))
        (set-output-pin! (led-pin ledbut))))
      ledbuttons)
    ; Initialize timer
    ; https://github.com/RaD/ArmpitScheme/blob/master/mcu_specific/LPC_2000/LPC_H2214/board.h#L52
    (write-timer-period milliseconds 58982)
    (start-timer milliseconds)
    (clear-screen)
    (draw-eye 33 110)
    (draw-eye 97 110)
    (puts "Initialised")))
  (make-fsm-transition 'start always awake-state)))
;;; /States ;;;


(define (make-virtual-pet) (begin
  (define res (make-finite-state-machine boot-state))
  (res 'feed-context (list (cons 'start #t)))
  res))
