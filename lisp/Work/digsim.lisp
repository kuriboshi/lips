(setq make-wire
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

(de call-each (procedures)
 (cond
  ((null procedures) 'done)
  (t ((car procedures))
     (call-each (cdr procedures)))))

(de inverter (input output)
 (add-action! input
  (closure
   (lambda ()
    (let ((new-value (logical-not (get-signal input))))
     (after-delay inverter-delay
                  (closure
                   (lambda ()
                    (set-signal! output new-value))
                   '(output new-value)))))
   '(input output))))

(de logical-not (s)
 (cond
  ((eq s 0) 1)
  ((eq s 1) 0)))

(de and-gate (a1 a2 output)
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
  (add-action! a2 and-action-procedure)))

(de logical-and (s1 s2)
 (cond
  ((and (eq s1 1) (eq s2 1)) 1)
  (t 0)))

(de get-signal (wire)
 (wire 'get-signal))
(de set-signal! (wire new-value)
 ((wire 'set-signal!) new-value))
(de add-action! (wire action-procedure)
 ((wire 'add-action!) action-procedure))

(de after-delay (delay action)
 (add-to-agenda! (+ delay (current-time the-agenda))
                 action
                 the-agenda))

(setq propagate
 (lambda ()
  (while (null (empty-agenda? the-agenda))
   (let ((first-item (first-agenda-item the-agenda)))
    (first-item)
    (remove-first-agenda-item! the-agenda)))
  'done))

(de probe (name wire)
 (add-action! wire
              (closure
               (lambda ()
                (prin2 name)
                (prin1 ": ")
                (prin1 (current-time the-agenda))
                (prin1 " New value = ")
                (print (get-signal wire)))
               '(name wire))))

(de make-time-segment (time queue)
 (cons time queue))
(de segment-time (s)
 (car s))
(de segment-queue (s)
 (cdr s))
(setq make-agenda
 (lambda ()
  (list '*agenda* (make-time-segment 0 (make-queue)))))
(de segments (agenda)
 (cdr agenda))
(de first-segment (agenda)
 (car (segments agenda)))
(de rest-segments (agenda)
 (cdr (segments agenda)))
(de set-segments! (agenda segments)
 (rplacd agenda segments))
(de current-time (agenda)
 (segment-time (first-segment agenda)))
(de empty-agenda? (agenda)
 (and (empty-queue? (segment-queue (first-segment agenda)))
      (null (rest-segments agenda))))

(de add-to-agenda! (time action agenda)
 (add-to-segments! (segments agenda)))
(de add-to-segments! (segments)
 (cond
  ((eq (segment-time (car segments)) time)
   (insert-queue! (segment-queue (car segments)) action))
  (t (let ((rest (cdr segments)))
      (cond
       ((null rest)
        (insert-new-time! time action segments))
       ((greaterp (segment-time (car rest)) time)
        (insert-new-time! time action segments))
       (t (add-to-segments! rest)))))))

(de insert-new-time! (time action segments)
 (let ((q (make-queue)))
  (insert-queue! q action)
  (rplacd segments (cons (make-time-segment time q) (cdr segments)))))
(de remove-first-agenda-item! (agenda)
 (delete-queue! (segment-queue (first-segment agenda))))
(de first-agenda-item (agenda)
 (let ((q (segment-queue (first-segment agenda))))
  (cond
   ((empty-queue? q)
    (set-segments! agenda (rest-segments agenda))
    (first-agenda-item agenda))
   (t (front q)))))

(de front-ptr (queue)
 (car queue))
(de rear-ptr (queue)
 (cdr queue))
(de set-front-ptr! (queue item)
 (rplaca queue item))
(de set-rear-ptr! (queue item)
 (rplacd queue item))
(de empty-queue? (queue)
 (null (front-ptr queue)))
(setq make-queue
 (lambda  ()
  (cons nil nil)))

(de front (queue)
 (cond
  ((empty-queue? queue)
   nil)
  (t (car (front-ptr queue)))))

(de insert-queue! (queue item)
 (let ((new-pair (cons item nil)))
  (cond
   ((empty-queue? queue)
    (set-front-ptr! queue new-pair)
    (set-rear-ptr! queue new-pair)
    queue)
   (t
    (rplacd (rear-ptr queue) new-pair)
    (set-rear-ptr! queue new-pair)
    queue))))

(de delete-queue! (queue)
 (cond
  ((empty-queue? queue)
   nil)
  (t
   (set-front-ptr! queue (cdr (front-ptr queue)))
   queue)))

(setq the-agenda (make-agenda))
(setq inverter-delay 2)
(setq and-gate-delay 3)
