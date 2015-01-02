(load "virtualpet.scm")

(define virtual-pet (make-finite-state-machine boot-state))
(puts "Created new virtualpet")

(define (feed-if-changed sense is-different? name)
  (let ((last-value (sense)))
    (lambda () (let ((current-value (sense)))
      (if (is-different? last-value current-value)
        (begin
          (virtual-pet 'feed-context (list (cons name current-value)))
          (set! last-value current-value)
          #t)
        #f)))))

(define (xor a b) (or (and a (not b)) (and (not a) b)))
(define (min-diff diff) (lambda (a b) (>f (abs (- a b)) diff)))

(define (button-pressed? ledbut) (lambda () (is-pin-set? (but-pin ledbut))))
(define feed-buttons (map 
  (lambda (ledbut) (feed-if-changed (button-pressed? ledbut) xor (ledname ledbut))) 
  ledbuttons))

(define feed-accel-x (feed-if-changed get-x (min-diff 0.1) 'accel-x))
(define feed-accel-y (feed-if-changed get-y (min-diff 0.1) 'accel-y))

(define feed-heartbeat (feed-if-changed millis (min-diff 1000) 'heartbeat))

(define feeders (cons feed-heartbeat (cons feed-accel-x (cons feed-accel-y feed-buttons))))

(define (mainloop) (begin
  (for-each (lambda (proc) (proc)) feeders)
  (mainloop)))

(virtual-pet 'feed-context (list (cons 'start #t)))
(puts "=== Started ===")
(mainloop)