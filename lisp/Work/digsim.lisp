
(defineq
    (make-wire
     (lambda ()
       ((closure
            '(progn
              (setq signal-value 0)
              (setq action-procedures nil)
              (setq set-my-signal!
               (closure
                   (lambda (new-value)
                     (cond
                       ((neq signal-value new-value)
                        (setq signal-value new-value)
                        (call-each action-procedures))
                       (t 'done)))
	           '(signal-value action-procedures)))
              (setq accept-action-procedure
               (closure
                   (lambda (proc)
                     (setq action-procedures
                           (cons proc action-procedures))
                     (proc))
	           '(action-procedures)))
              (setq dispatch
               (lambda (m)
                 (cond
                   ((eq m 'get-signal)
                    signal-value)
                   ((eq m 'set-signal!)
                    set-my-signal!)
                   ((eq m 'add-action!)
                    accept-action-procedure)
                   (t nil))))
              (lambda ()
                (closure
                    dispatch
	            '(signal-value set-my-signal! accept-action-procedure
                      action-procedures signal-value))))
            '(set-my-signal! accept-action-procedure dispatch
              signal-value action-procedures)))))

    (call-each
     (lambda (procedures)
       (cond
         ((null procedures) 'done)
         (t ((car procedures))
            (call-each (cdr procedures))))))

  (inverter
   (lambda (input output)
     (add-action! input
                  (closure
                      (lambda ()
                        (let ((new-value (logical-not (get-signal input))))
                          (after-delay inverter-delay
                                       (closure
                                           (lambda ()
                                             (set-signal! output new-value))
                                           '(output new-value)))))
                      '(input output)))))

  (logical-not
   (lambda (s)
     (cond
       ((eq s 0) 1)
       ((eq s 1) 0))))

  (and-gate
   (lambda (a1 a2 output)
     (let (and-action-procedure)
       (setq and-action-procedure
             (closure
                 (lambda ()
                   (let ((new-value (locical-and (get-signal a1) (get-signal a2))))
                     (after-delay and-gate-delay
                                  (closure
                                      (lambda ()
                                        (set-signal! output new-value))
                                      '(output new-value)))))
                 '(a1 a2 output)))
       (add-action! a1 and-action-procedure)
       (add-action! a2 and-action-procedure))))

  (logical-and
   (lambda (s1 s2)
     (cond
       ((and (eq s1 1) (eq s2 1)) 1)
       (t 0))))

  (get-signal
   (lambda (wire)
     (wire 'get-signal)))

  (set-signal!
   (lambda (wire new-value)
     ((wire 'set-signal!) new-value)))

  (add-action!
   (lambda (wire action-procedure)
     ((wire 'add-action!) action-procedure)))

  (after-delay
   (lambda (delay action)
     (add-to-agenda! (plus delay (current-time the-agenda))
                     action
                     the-agenda)))

  (propagate
   (lambda ()
     (while (null (empty-agenda? the-agenda))
       (let ((first-item (first-agenda-item the-agenda)))
         (first-item)
         (remove-first-agenda-item! the-agenda)))
     'done))

  (probe
   (lambda (name wire)
     (add-action! wire
                  (closure
                      (lambda ()
                        (prin2 name)
                        (prin1 ": ")
                        (prin1 (current-time the-agenda))
                        (prin1 " New value = ")
                        (print (get-signal wire)))
                      '(name wire)))))

  (make-time-segment
   (lambda (time queue)
     (cons time queue)))

  (segment-time
   (lambda (s)
     (car s)))

  (segment-queue
   (lambda (s)
     (cdr s)))

  (make-agenda
   (lambda ()
     (list '*agenda* (make-time-segment 0 (make-queue)))))

  (segments
   (lambda (agenda)
     (cdr agenda)))

  (first-segment
   (lambda (agenda)
     (car (segments agenda))))

  (rest-segments
   (lambda (agenda)
     (cdr (segments agenda))))

  (set-segments!
   (lambda (agenda segments)
     (rplacd agenda segments)))

  (current-time
   (lambda (agenda)
     (segment-time (first-segment agenda))))

  (empty-agenda?
   (lambda (agenda)
     (and (empty-queue? (segment-queue (first-segment agenda)))
          (null (rest-segments agenda)))))

  (add-to-agenda!
   (lambda (time action agenda)
     (add-to-segments! (segments agenda))))

  (add-to-segments!
   (lambda (segments)
     (cond
       ((eq (segment-time (car segments)) time)
        (insert-queue! (segment-queue (car segments)) action))
       (t (let ((rest (cdr segments)))
            (cond
              ((null rest)
               (insert-new-time! time action segments))
              ((greaterp (segment-time (car rest)) time)
               (insert-new-time! time action segments))
              (t (add-to-segments! rest))))))))

  (insert-new-time!
   (lambda (time action segments)
     (let ((q (make-queue)))
       (insert-queue! q action)
       (rplacd segments (cons (make-time-segment time q) (cdr segments))))))

  (remove-first-agenda-item!
   (lambda (agenda)
     (delete-queue! (segment-queue (first-segment agenda)))))

  (first-agenda-item
   (lambda (agenda)
     (let ((q (segment-queue (first-segment agenda))))
       (cond
         ((empty-queue? q)
          (set-segments! agenda (rest-segments agenda))
          (first-agenda-item agenda))
         (t (front q))))))

  (front-ptr
   (lambda (queue)
     (car queue)))

  (rear-ptr
   (lambda (queue)
     (cdr queue)))

  (set-front-ptr!
   (lambda (queue item)
     (rplaca queue item)))

  (set-rear-ptr!
   (lambda (queue item)
     (rplacd queue item)))

  (empty-queue?
   (lambda (queue)
     (null (front-ptr queue))))

  (make-queue
   (lambda  ()
     (cons nil nil)))

  (front
   (lambda (queue)
     (cond
       ((empty-queue? queue)
        nil)
       (t (car (front-ptr queue))))))

  (insert-queue!
   (lambda (queue item)
     (let ((new-pair (cons item nil)))
       (cond
         ((empty-queue? queue)
          (set-front-ptr! queue new-pair)
          (set-rear-ptr! queue new-pair)
          queue)
         (t
          (rplacd (rear-ptr queue) new-pair)
          (set-rear-ptr! queue new-pair)
          queue)))))

  (delete-queue!
   (lambda (queue)
     (cond
       ((empty-queue? queue)
        nil)
       (t
        (set-front-ptr! queue (cdr (front-ptr queue)))
        queue))))
  )

(setq the-agenda (make-agenda))
(setq inverter-delay 2)
(setq and-gate-delay 3)
