; constants
(defparameter *hand-size* 10)

; retrieve value of given suit
(defun suit-val (suit)
  (case suit
    ('H 0)
    ('C 1)
    ('D 2)
    ('S 3)))

; compare two suits
(defun suit-comp (suit1 suit2)
  (< (suit-val suit1) (suit-val suit2)))

; compare two cards
(defun card-comp (card1 card2)
  (cond
    ((< (cdr card1) (cdr card2)) t)
    ((= (cdr card1) (cdr card2)) (suit-comp (car card1) (car card2)))
    (t nil)))

; returns total value of list of cards
(defun sum-cards (&rest cards)
  (reduce
    (lambda (sum card)
      (+ sum (cdr card)))
    cards
    :initial-value 0))

; prettifies seed of card
(defun seed-to-word (seed)
  (princ-to-string
    (case seed
      ('C "clubs")
      ('H "hearts")
      ('S "spades")
      ('D "diamonds")
      (otherwise "???"))))

; prints a list of cards
(defun print-cards (cards &key (long nil))
  (loop for card in cards do
        (if long
          (format t "~a: ~a, " (seed-to-word (car card)) (cdr card))
          (format t "~a~a " (car card) (cdr card)))))

