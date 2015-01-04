(load "lib.scm")

; Milliseconds counter
(define milliseconds timer0)

; Uptime in ms
(define (millis) (read-timer milliseconds))

(define (wait-until timestamp)
  (if (< (millis) timestamp) (wait-until timestamp) '()))

(define (delayms ms)
  (wait-until (+ ms (millis))))

; Timestamp of last transition
(define last-transition-time 0)

(define (state-uptime) (- (millis) last-transition-time))

; Divisor for time-based aptitudes evolution
(define TIME-DIV 360000.0)
(define TIME-AFFECTIVE 180000)
(define HEARTBEAT-PERIOD-MS 2000) 

; Return x times incremented aptitude based on time since last change
(define (incx aptitude x) (min 1.0 (+ aptitude (* x (/ (state-uptime) TIME-DIV)))))
(define (inc aptitude) (incx aptitude 1))

; Return x times decremented aptitude based on time since last change
(define (decx aptitude x) (max 0.0 (- aptitude (* x (/ (state-uptime) TIME-DIV)))))
(define (dec aptitude) (decx aptitude 1))
