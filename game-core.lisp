; game constats
; NB: does not work with players != 4
(defparameter *players-num* 4)

; heuristics constants
(defparameter *rating-card* 1)

; game state: describes entire game at a given moment in time
(defstruct game-state
  (table nil)
  (hands nil)
  (piles nil)
  (player 0))

; game move: desecribes only move available in the game:
;            playing a card to acquire other cards
(defstruct card-playing-move
  (card-played nil)
  (cards-collected nil)
  (game-tree nil))

; generates a random intial game state
(defun make-initial-game-state ()
  (let ((deck
          (shuffle
            (coerce
              (loop for seed in '(H D S C)
                    append (loop for value from 1 to 10
                                 collect (cons seed value)))
              'vector)))
        (deck-indx -1))
    (flet ((fish ()
                 (incf deck-indx)
                 (aref deck deck-indx)))
    
      (make-game-state
        :table nil
        :hands (loop repeat *players-num*
                     collect (loop repeat *hand-size*
                                    collect (fish)))

        :piles '(nil nil)
        :player 0))))

; rates a game state for the minmax algorithm
; ERI QUA CICCIOCACCA, da fare:
;   1) sta funza quà sotto (allenarsi a usare closures in lisp)
;   2) roba WIP subito sotto, incluso sistema scope.
;   3) minmax che usa ste funzioni come heuristics.
;   4) alpha-beta trimming.
;   5) la cacca.
(defun rate-game-state (game-state player)
  (reduce
    (let ((pile-n 0))
      (lambda (rating pile)
        (funcall
          (if )))
    (game-state-piles game-state)
    :initial-value 0)

; rates a card pile
; WIP:
;   1) handle napole
;   2) handle scope
;   3) handle carte
;   4) handle primiera
(defun rate-card-pile (pile)
  (reduce
    (lambda (rating card)
        (+ rating (rate-card card)))
    pile
    :initial-value 0))

; rates a single card
(defun rate-card (card)
  *rating-card*)

; attempts to generate entire game tree
(defun game-tree (game-state)
  (lazy-cons
    game-state
    (make-possible-moves game-state)))

; limits tree depth
(defun limit-tree-depth (tree depth)
  (if (zerop depth)
    (lazy-nil)
    (lazy-cons
      (lazy-car tree)
      (lazy-mapcar
        (lambda (move)
          (with-slots (card-played cards-collected game-tree) move
            (make-card-playing-move
              :card-played card-played
              :cards-collected cards-collected
              :game-tree (limit-tree-depth game-tree (1- depth)))))
        (lazy-cdr tree)))))

; generates all possible moves performable from a given game state
; NB the only type of move in scopone is "playing a card"
;   this move can be described by the card played (CAR of the move)
;   and the cards acquired (list in the CDR of the move)
(defun make-possible-moves (game-state)
  (with-slots (table hands piles player) game-state
    (let ((current-hand (nth player hands)))
      (lazy-mapcan
        (lambda (card)
          (mapcar
            (lambda (cards-collected)
              (make-card-playing-move
                :card-played card
                :cards-collected cards-collected
                :game-tree (game-tree (play-card game-state card cards-collected))))
            (collect-cards table card)))
        (make-lazy current-hand)))))

; creates all possible "card-playing-move(s)" on given table
; and played card
(defun collect-cards (table played-card)
  (let ((res (remove-if-not
               (lambda (comb)
                 (=
                   (reduce
                     (lambda (sum card)
                       (+ sum (cdr card)))
                     comb
                     :initial-value 0)
                   (cdr played-card)))
               (make-all-combinations table #'card-comp))))
    (if res
      res
      '(nil))))

; performs the action of playing a given card, returning the resulting game state
(defun play-card (game-state card-played cards-collected)
    (with-slots (table hands piles player) game-state
      (make-game-state
        :table (if cards-collected
                 (set-difference table cards-collected :test #'equal)
                 (cons card-played table))
        :hands (loop for hand in hands
                     for pl from 0
                     collect (if (= pl player)
                               (remove card-played hand :test #'equal)
                               hand))
        :piles (loop for pile in piles
                     for team from 0
                     collect (if (and cards-collected (= (player-team player) team))
                               (cons card-played (append pile cards-collected))
                               pile))
        :player (next-player player))))
