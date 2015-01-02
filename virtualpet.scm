(load "fsm.scm")
(load "lib.scm")
(load "graphics.scm")
(load "pinout.scm")
(load "time.scm")
(load "helpers.scm")

;;; Aptitudes ;;;
(define happiness 0.75)
(define submission 0.75)
(define food 0.75)
(define health 0.75)
(define rest 0.75)

(define (all-aptitudes) (list happiness submission food health rest))

(define (draw-aptitudes) (begin
  (draw-happiness happiness)
  (draw-rest rest)
  (draw-submission submission)
  (draw-food food)
  (draw-health health)))

(define playing-color 'nocolor)
(define (choose-color)
  (set! playing-color (ledname ([] ledbuttons (rand 3)))))
;;; /Aptitudes ;;;

;;; States ;;;
(define sleeping-state (make-state
  (lambda () 
    (clear-eyes)
    (draw-sleepZ)
    (puts "Going to sleep ZzzzZZzzz..."))
  (lambda () (begin
    (set! rest (incx rest 2))
    (set! happiness (dec happiness))
    (set! food (dec food))
    (puts "Finished sleep. Rest is now =" rest)))))

(define awake-state (make-state
  (lambda () (begin 
    (show 'red) 
    (puts "Awake !!!")))
  (lambda () (begin 
    (hide 'red)
    (set! rest (dec rest))
    (set! food (decx food 2))
    (puts "No more awake")))))

(define awaking-state (make-state 
  (lambda () (begin (clear-eyes) (draw-eyes))) 
  do-nothing
  (make-fsm-transition 'heartbeat always awake-state)))


;;; Flip for sleep/awakening
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
      (and (<f y FLIP-THRES-FALL) (> (state-uptime) FLIP-MS)))) awaking-state)
    ; Otherwise go back to sleep
    (make-fsm-transition 'accel-y (lambda (y) (<f y FLIP-THRES-FALL)) sleeping-state)))

(sleeping-state 'add-transition (make-fsm-transition
  'accel-y (lambda (y) (>f y FLIP-THRES-RISE)) sleeping-flipped-state))

;;; Play
(define playing-state (make-state
  (lambda () (begin
    (choose-color)
    (animate-leds)
    (puts "Playing" playing-color)))
  (lambda () (begin
    (show playing-color)
    (delayms 500)
    (hide playing-color)
    ; If found in a short time, increase happiness
    (if (< (state-uptime) 1000) (begin 
      (puts "I'm more happy:" happiness "is now" (min 1.0, (+ happiness 0.1)))
      (set! happiness (min 1.0, (+ happiness 0.1)))))
    (puts "Finished playing")))))

; Play if green button pressed
(awake-state 'add-transition (make-fsm-transition
  'green identity playing-state))

; Stop playing if correct button pressed
(for-each (lambda (ledbut)
  (let ((color (ledname ledbut)))
    (playing-state 'add-transition (make-fsm-transition
      color 
      (lambda (active) (and active (string=? playing-color color)))
      awake-state))))
  ledbuttons)

;;; f00d
(define meal-state (make-state
  (lambda () (show 'red))
  (lambda () (begin 
    (set! food (min 1.0 (+ food 0.25)))
    ; He doesn't like vegetables
    (set! submission (max 0.0 (- submission 0.05)))
    (delayms 500)
    (hide 'red)))
  (make-fsm-transition 'heartbeat always awake-state)))

(define snack-state (make-state
  (lambda () (show 'green))
  (lambda () (begin 
    (set! food (min 1.0 (+ food 0.1))) 
    (set! health (max 0.0 (- health 0.05)))
    (set! submission (incx submission 30))
    (delayms 500)
    (hide 'green)))
  (make-fsm-transition 'heartbeat always awake-state)))

(define choose-food-state (make-state
  (lambda () (begin
    (show 'red)
    (show 'green)
    (puts "Choose some food: red for a complete meal green for a snack")))
  (lambda () (begin
    (hide 'red)
    (hide 'green)))
  (make-fsm-transition 'red always meal-state)
  (make-fsm-transition 'green always snack-state)))

(awake-state 'add-transition (make-fsm-transition 'yellow always choose-food-state))


;;; Need for attention
(define (need-attention? nothing)
  (reduce or #f (list
    (>= (state-uptime) TIME-AFFECTIVE)
    (map (lambda (x) (< x 0.125)) (all-aptitudes)))))

(define need-attention-state (make-state
  (lambda () (begin (puts "I need attention !")))
  (lambda () (begin 
    (puts "I don't need attention anymore")
    (set! happiness (decx happiness 2))))
  (make-fsm-transition 'accel-y always awake-state)
  (make-fsm-transition 'accel-x always awake-state)
  (make-fsm-transition 'heartbeat (lambda (x) (begin (animate-leds) #f)) do-nothing)))

(for-each 
  (lambda (ledbut) (need-attention-state 'add-transition (make-fsm-transition
    (ledname ledbut) identity awake-state)))
  ledbuttons)

(awake-state 'add-transition (make-fsm-transition 'heartbeat need-attention? need-attention-state))


;;; Initialization
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
    (puts "Initialised")))
  (make-fsm-transition 'start always awaking-state)))
;;; /States ;;;
