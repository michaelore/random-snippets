(defun dd (var expr)
  (cond ((eql expr var) 1)
	((atom expr) 0)
	((listp expr) (cond ((memq (car expr) *dd-rule-keys*)
			     (funcall (gethash (car expr) *dd-rules*)
				      var
				      (cdr expr)))
			    ((memq (car expr) *dd-fun-keys*)
			     (if (eql 1 (length (cdr expr)))
				 `(* ,(dd var (cadr expr))
				     ,(funcall (gethash (car expr) *dd-funs*)
					       (cadr expr)))
			       nil))
			    (t nil)))
	(t nil)))

(setq *dd-rules* (make-hash-table))

(setq *dd-rule-keys* '())

(defmacro add-dd-rule (fun body)
  `(progn (push ',fun *dd-rule-keys*)
	  (puthash ',fun (lambda (var args)
			   (let ((ddr (lambda (expr) (dd var expr))))
			     ,body))
		   *dd-rules*)))

(add-dd-rule + (cons '+ (mapcar ddr args)))

(add-dd-rule - (cons '- (mapcar ddr args)))

(defun doto-nth-elem (n fun xs)
  (cond ((not xs) xs)
	((eql n 0)
	 (cons (funcall fun (car xs)) (cdr xs)))
	(t (cons (car xs) (doto-nth-elem (- n 1) fun (cdr xs))))))

(add-dd-rule * (cons '+ (progn (setq acc '())
			       (dotimes (i (length args))
				 (push (cons '* (doto-nth-elem i ddr args)) acc))
			       acc)))

(add-dd-rule / (cons '- (cons `(/ ,(funcall ddr (car args))
				  ,@(cdr args))
			      (progn (setq acc '())
				     (dotimes (i (length (cdr args)))
				       (push `(/ (* ,(car args) ,(funcall ddr (nth i (cdr args))))
						 (* ,@(doto-nth-elem i (lambda (g) `(expt ,g 2)) (cdr args)))) acc))
				     acc))))

(add-dd-rule expt `(* (expt ,(car args)
			    ,(cadr args))
		      (+ (/ (* ,(cadr args)
			       ,(funcall ddr (car args)))
			    ,(car args))
			 (* (log ,(car args))
			    ,(funcall ddr (cadr args))))))
		     
(setq *dd-funs* (make-hash-table))

(setq *dd-fun-keys* '())

(defmacro add-dd-fun (fun body)
  `(progn (push ',fun *dd-fun-keys*)
	  (puthash ',fun (lambda (var)
			   ,body)
		   *dd-funs*)))

(add-dd-fun exp `(exp ,var))

(add-dd-fun sin `(cos ,var))

(add-dd-fun cos `(- (sin ,var)))

(add-dd-fun tan `(/ 1 (expt (cos ,var) 2)))