(load "virtualpet.scm")

; -> (list (cons 'name #t|#f) ...) for each ledbutton pair
(define (read-buttons)
  (map (lambda (ledbut) (cons (ledname ledbut) (is-pin-set? (but-pin ledbut)))) ledbuttons))

(define virtual-pet (make-virtual-pet))
(puts "Created new virtualpet")

(define (feed-sensors)
  (virtual-pet 'feed-context (read-buttons)))

(define (feed-accel)
  (virtual-pet 'feed-context (list 'accel (list (get-x) (get-y)))))

(define (mainloop) (begin
  (feed-sensors)
  (feed-accel)
  (draw-aptitudes)
  (delayms 10)
  (mainloop)))

(mainloop)
