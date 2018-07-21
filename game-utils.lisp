(defun next-player (player)
  (mod (1+ player) *players-num*))

(defun player-team (player)
  (if (zerop (mod player 2)) 0 1))
