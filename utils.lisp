; makes all possible combinations of a (given) length out
; of the elemnts contained in a (given) alphabet
(defun make-combinations (alphabet len comp)
  (if (not (zerop len))
    (remove-duplicates
      (mapcan
        (lambda (comb)
          (mapcar
            (lambda (sym)
              (sort (copy-list (cons sym comb)) comp))
            (set-difference alphabet comb)))
        (make-combinations alphabet (1- len) comp))
      :test #'equal)
    '(nil)))

; as above, but make combinations of all possible lengths
(defun make-all-combinations (alphabet comp)
  (apply #'append (loop for j from 1 below (length alphabet)
                   collect (make-combinations alphabet j comp))))

; shuffles a vector (destructive)
(defun shuffle (vec)
  (loop for j below (length vec)
        do (rotatef (aref vec j) (aref vec (random (length vec)))))
  vec)
