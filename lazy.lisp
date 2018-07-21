(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

(defmacro lazy (&body body)
  (let ((forced (gensym))
	(value (gensym)))
    `(let ((,forced nil)
	   (,value nil))
       (lambda ()
	 (unless ,forced
	   (setf ,value (progn ,@body))
	   (setf ,forced t))
	 ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

(defun lazy-nth (pos lst)
  (if (zerop pos)
    (lazy-car lst)
    (lazy-nth (1- pos) (lazy-cdr lst))))

(defun lazy-length (lst)
  (labels ((f (lst len)
              (if (lazy-null lst)
                len
                (f (lazy-cdr lst) (1+ len)))))
  (f lst 0)))
(compile 'lazy-length)

(defparameter *integers*
  (labels ((f (n)
	      (lazy-cons n (f (1+ n)))))
	  (f 1)))

(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst)) 
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-val)
                (if lst-val
                  (lazy-cons (car lst-val) (f (cdr lst-val)))
                  (lazy-mapcan fun (lazy-cdr lst)))))
    (lazy
      (unless (lazy-null lst)
        (force (f (funcall fun (lazy-car lst))))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
        (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-remove-if (fun lst)
  (if (lazy-null lst)
    (lazy-nil)
    (if (funcall fun (lazy-car lst))
      (lazy-remove-if fun (lazy-cdr lst))
      (lazy-cons (lazy-car lst) (lazy-remove-if fun (lazy-cdr lst))))))

(defun lazy-remove-if-not (fun lst)
  (lazy-remove-if (complement fun) lst))

(defun active-remove-if (fun lst)
  (labels ((f (fun lst new-lst)
              (if (not (lazy-null lst))
                (if (funcall fun (lazy-car lst))
                  (f fun (lazy-cdr lst) new-lst)
                  (f fun (lazy-cdr lst) (cons (lazy-car lst) new-lst)))
                (reverse new-lst))))
    (f fun lst nil)))

(defun active-remove-if-not (fun lst)
  (active-remove-if (complement fun) lst))
