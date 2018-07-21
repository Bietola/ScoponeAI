(defun make-possible-moves (game-state)
  (with-slots (table hands piles player) game-state
    (let ((current-hand (nth player hands)))
      (mapcan
        (lambda (card)
          (mapcar
            (lambda (cards-collected)
              (make-card-playing-move
                :card-played card
                :cards-collected cards-collected
                :game-tree (game-tree (play-card game-state card cards-collected))))
            (collect-cards table card)))
        current-hand))))
