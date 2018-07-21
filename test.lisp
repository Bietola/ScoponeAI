; function that goes through the tree randomly for n times
(defun random-traverse (tree depth)
  (if (zerop depth)
    tree
    (let1 random-pos (random (lazy-length (lazy-cdr tree)))
      (random-traverse (card-playing-move-game-tree (lazy-nth random-pos (lazy-cdr tree))) (1- depth)))))

; TEST GAME STATE WITH PREPARED TABLE
(defparameter gstate (make-initial-game-state))
(setf (game-state-table gstate) '((H . 6) (C . 3) (S . 3)))

; TEST TRESS
(defparameter *tree* (game-tree (make-initial-game-state)))
(defparameter *stree* (limit-tree-depth *tree* 2))
(defparameter *atree* (random-traverse *tree* 10))
