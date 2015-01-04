(load "fsm.scm")
(load "time.scm")
(load "pinout.scm")

(define (puts . texts) (begin (map display texts) (newline)))

(define (rand n) (modulo (pulse_in x-pin) n))

(define (int x)
  (if (= x 0) 0 (round (inexact->exact x))))

; Like foldr but traverse nested lists
(define (reduce proc initial a_list)
  (if (null? a_list) 
    initial
    (let ((head (car a_list)) (tail (cdr a_list)))
      (proc (reduce proc initial tail) (if (pair? head) 
        (reduce proc initial head) 
        head)))))

(define (input-func func) (lambda () (begin
  (set! last-transition-time (millis))
  (draw-aptitudes)
  (func))))

; Decorate make-state from fsm with the input function updating last-transition-time
(define (make-state in-func out-func . transitions)
  ; Concat variable arguments
  (let ((args (cons (input-func in-func) (cons out-func transitions))))
    (apply make-fsm-state args)))

; -> l[i] (i = 0..len(l)-1)
(define ([] l i)
  (if (pair? l)
    (if (> i 0)
      ([] (cdr l) (- i 1))
      (car l))
    l))

(define (<f a b) (< (inexact->exact a) (inexact->exact b)))
(define (>f a b) (> (inexact->exact a) (inexact->exact b)))

(define (if-changed changed? act)
  (let ((last-value (sense)))
    (lambda (current-value)
      (if (change? last-value current-value)
        (begin
          (act current-value)
          (set! last-value current-value)
          #t)
        #f))))
