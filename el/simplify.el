(defmacro aif (pred then &optional else)
  `(let ((it ,pred))
     (if it
	 ,then
       ,else)))

(defun thread-through (x pred funs)
  (if (and (car funs)
	   (funcall pred x))
      (thread-through (funcall (car funs) x) pred (cdr funs))
    x))

(defun simp (expr)
  (cond ((listp expr) (aif (gethash (car expr) *simplify-rules*)
			   (thread-through (mapcar 'simp expr)
					   (lambda (x) (and (listp x)
							    (eql (car x) (car expr))))
					   it)
			   (mapcar 'simp expr)))
	(t expr)))

(defmacro simp-rule (key &rest body)
  `(let ((actions ',(mapcar (lambda (x) `(lambda (expr) ,x)) body)))
     (puthash ',key (aif (gethash ',key *simplify-rules*)
			(append actions it)
			actions) *simplify-rules*)))

(setq *assume-vars-non-zero* t)

(setq *simplify-rules* (make-hash-table))

(simp-rule *
	   ; (* x 0) == 0
	   (if (some (lambda (x) (eql x 0)) expr)
	       0
	     expr)
	   ; (* x 1) == x
	   (remove-if (lambda (x) (eql x 1)) expr)
	   ; (* x (* y z)) == (* x y z)
	   (mapcan (lambda (x) (if (and (listp x)
					(eql (car x) '*))
				   (cdr x)
				 (list x))) expr)
	   ; (* x x) == (expt x 2)
	   (flet ((combine (rem acc)
			   (if rem
			       (if (some (lambda (x)
					   (eql x (car rem)))
					 (cdr rem))
				   (combine (remove-if (lambda (x)
							 (eql x (car rem)))
						       rem)
					    (cons `(expt ,(car rem)
							 ,(count (car rem)
								 rem))
						  acc))
				 (combine (cdr rem)
					  (cons (car rem) acc)))
			     (reverse acc))))
	     (cons '* (combine (cdr expr) '())))
	   ; (* (expt x y) (expt x z)) == (* (expt x (+ y z)))
	   ;; (flet ((comb (rem acc)
	   ;; 		(if rem
	   ;; 		    (if (and (listp (car rem))
	   ;; 			     (eql (caar rem) 'expt))
	   ;; 			(comb (remove-if (lambda (x)
	   ;; 					   (eql x (cadar rem)))
	   ;; 					 (cdr rem))
	   ;; 			      (cons `(expt ,(cadar rem)
	   ;; 					   ,(simp `(+ ,@(mapcan (lambda (x)
	   ;; 								  (
	   ; (* x) == x && (*) == 1
	   (cond ((eql (length expr) 1) 1)
		 ((eql (length expr) 2) (cadr expr))
		 (t expr))
	   ; (* x (/ y z)) == (/ (* x y) z)
	   (let ((pred (lambda (x) (and (listp x)
					(eql (car x) '/)))))
	     (if (some (lambda (x) (funcall pred x)) expr)
		 (simp `(/ ,(mapcar (lambda (x) (if (funcall pred x)
						    (cadr x)
						  x)) expr)
			   ,@(mapcan (lambda (x) (if (funcall pred x)
						     (cddr x)
						   '())) expr)))
	       expr)))

(simp-rule +
	   ; (+ x 0) == x
	   (remove-if (lambda (x) (eql x 0)) expr)
	   ; (+ x) == x && (+) == 0
	   (cond ((eql (length expr) 1) 0)
		 ((eql (length expr) 2) (cadr expr))
		 (t expr)))

(simp-rule /
	   ; (/ x 1) == x
	   (cons '/
		 (cons (cadr expr)
		       (remove-if (lambda (x) (eql x 1))
				  (cddr expr))))
	   ; (/ x (* y z)) == (/ x y z)
	   (cons '/
		 (cons (cadr expr) (mapcan (lambda (x)
					     (if (and (listp x)
						      (eql (car x) '*))
						 (cdr x)
					       (list x)))
					   (cddr expr))))
	   ; (/ x y 0) == (/ x 0)
	   (if (some (lambda (x) (eql x 0)) (cddr expr))
	       `(/ ,(cadr expr) 0)
	     expr)