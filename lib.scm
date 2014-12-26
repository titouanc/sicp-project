;Helper functions
(define (bit nr) (ash 1 nr)) 
(define (pin nr) (bit nr))   
(define (wait n) 
  (if (= n 0) '() (wait (- n 1))))
(define (get-bit byte bitnr)  
  (logand (bit bitnr) byte))
(define (send-command cmd . args)
  (send-byte lcd-cmd cmd)
  (apply send-data args))
(define (send-data . data-list) 
  (if (pair? data-list)
      (begin 
        (send-byte lcd-data (car data-list))
        (apply send-data (cdr data-list)))))
;Constants
(define gpio0 #xE002800)
(define pin-status #x00) 
(define pin-set    #x04)  
(define pin-clear  #x0C)  
(define pin-dir    #x08)  
(define timer0 #xE000400)
(define timer1 #xE000800)
(define timer-control #x04)
(define timer-count  #x08)
(define timer-period #x0C)
(define cs (pin 20))
(define sclk (pin 4))
(define sdata (pin 6))
(define reset (pin 21))
(define lcd-cmd 0)
(define lcd-data 1)
(define CASET #x15)
(define COMSCN #xBB)
(define DATCTL #xBC)
(define DISCTL #xCA)
(define DISINV #xA7)
(define DISON #xAF)
(define OSCON #xD1)
(define PTLOUT #xA9)
(define PWRCTR #x20)
(define RGBSET8 #xCE)
(define PASET #x75)
(define RAMWR #x5C)
(define SLPOUT #x94)
(define TMPGRD #x82)
(define VOLCTR #x81)
(define NOP #x25)

;Timer
(define (stop-timer timer)
  (write 0 timer timer-control))
(define (start-timer timer)
  (write 1 timer timer-control))
(define (restart-timer timer)
  (reset-timer timer)
  (start-timer timer))
(define (reset-timer timer)
  (write 2 timer timer-control))
(define (read-timer timer)
  (read timer timer-count))
(define (write-timer-period timer period)
  (write period timer timer-period))

;Pin
(define (set-pin! pin)
  (write pin gpio0 pin-set))
(define (clear-pin! pin)
  (write pin gpio0 pin-clear))
(define (set-output-pin! pin)
  (write (logior (read gpio0 pin-dir) pin) gpio0 pin-dir))
(define (set-input-pin! pin)
  (write (logand (read gpio0 pin-dir) (lognot pin)) gpio0 pin-dir))  
(define (is-pin-set? pin)
  (= (logand pin (read gpio0 pin-status)) pin))

;Display
(define (init-lcd)
  (for-each set-output-pin! (list cs sclk sdata reset))
  (set-pin! cs)
  (clear-pin! reset)
  (wait 1)
  (set-pin! reset)
  (wait 20)
  (send-command DISCTL #x03 32 12 0)
  (send-command COMSCN #x01)
  (send-command OSCON)
  (send-command SLPOUT)
  (send-command VOLCTR 50 3)
  (send-command TMPGRD 0)
  (send-command PWRCTR #x0F)
  (wait 100)
  (send-command DISINV)
  (send-command PTLOUT)
  (send-command DATCTL 0 0 #x02)
  (send-command NOP)
  (send-command DISON)
  (wait 200))

(define (set-pixel! x y width height colour)
  (let* ((x1 x)
         (x2 (+ x width -1))
         (y1 (+ y 2))
         (y2 (+ y height 1))
         (i (round (+ 130 (/ (* (+ 1 (- (+ x width) x1))  (+ 1 (- (+ y height) y))) 2))))
         (A (ash colour -4))
         (B (logior (ash (logand #xf colour) 4) (logand (ash colour -8) #xF)))
         (C (logand colour #xFF)))
    (send-command PASET y1 y2) 
    (send-command CASET x1 x2)
    (send-command RAMWR A B)
    (send-command NOP)))
 
(define (fill-rectangle! x y width height colour)
  (let* ((x1 x)
         (x2 (+ x width -1))
         (y1 (+ y 2))
         (y2 (+ y height 1))
         (i (round (+ 130 (/ (* (+ 1 (- (+ x width) x1))  (+ 1 (- (+ y height) y))) 2))))
         (A (ash colour -4))
         (B (logior (ash (logand #xf colour) 4) (logand (ash colour -8) #xF)))
         (C (logand colour #xFF)))
    (send-command PASET y1 y2) 
    (send-command CASET x1 x2)
    (send-command RAMWR)         
    (send-byte-n A B C i)))

(init-lcd)