(define (get-input context input-name) (assoc input-name context))
(define input-value cdr)

(define (make-finite-state-machine start-state)
  (let ((current-state start-state)
        (current-transitions (start-state 'transitions)))

    ;;; get the name of all input channels needed by the current state's transitions
    (define (get-transition-inputs)
      (map transition-input-name current-transitions))

    ;;; feed the FSM with some sensor information (context)
    (define (feed-context context) (begin
      (map display (list "Feeding" context))
      (newline)
      ;; will trigger all transitions that satisfy their predicate with given context
      (for-each (lambda (transition)
        (let ((input (get-input context (transition-input-name transition))))
          (if (and input ((transition-predicate transition) (input-value input)))
            (change-state (transition-state transition)))))
        current-transitions)))

    (define (change-state new-state)
      ;; (if (not (equal? new-state current-state)) ; check for transition to itself
      (begin
        (current-state 'exit-action)
        (new-state 'entry-action)
        (set! current-transitions (new-state 'transitions))
        (set! current-state new-state)))

  (lambda (msg . args) 
    (case msg
      ('feed-context (apply feed-context args))
      ('get-inputs (get-transition-inputs))
      (else (error "Msg not understood: " msg))))))

(define (make-fsm-transition input-name predicate new-state) 
  (list input-name predicate new-state))

(define transition-input-name car)
(define transition-predicate cadr)
(define transition-state caddr)

(define (do-nothing) '())
(define (always . args) #t)

;;; State takes at least 2 arguments
;;;  entry-action: zero argument procedure (thunk)
;;;  exit-action:   idem
(define (make-fsm-state entry-action exit-action . transitions)
 (define (add-transition transition)
   (set! transitions (cons transition
                transitions)))
  (lambda (msg . args)
    (case msg
      ('entry-action (entry-action))
      ('exit-action (exit-action))
      ('transitions transitions)
      ('add-transition (apply add-transition args))
      (else (error "Msg not understood: " msg)))))
