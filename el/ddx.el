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
	(t il)))

(setq *dd-rules* (make-hash-table))

(setq *dd-rule-keys* '())

(defmacro add-rule (fun body)
  `(progn (push ',fun *dd-rule-keys*)
	  (puthash ',fun (lambda (var args)
			   (let ((ddr (lambda (expr) (dd var expr))))
			     ,body))
		   *dd-rules*)))

(add-rule + (cons '+ (mapcar ddr args)))

(add-rule - (cons '- (mapcar ddr args)))

(add-rule * `(+ (* ,(cadr args)
		   ,(funcall ddr (car args)))
		(* ,(car args)
		   ,(funcall ddr (cadr args)))))

(add-rule / `(/ (- (* ,(cadr args)
		      ,(funcall ddr (car args)))
		   (* ,(car args)
		      ,(funcall ddr (cadr args))))
		(expt ,(cadr args) 2)))

;Unfinished
;(add-rule expt `(* ,(ddr (car args))
;		   ,(if (eql (cadr args) 1)
;			`(log (abs ,var))
;		      `(* ,(cadr args)
			  
		     
(setq *dd-funs* (make-hash-table))

(setq *dd-fun-keys* '())

(defmacro add-fun (fun body)
  `(progn (push ',fun *dd-fun-keys*)
	  (puthash ',fun (lambda (var)
			   ',body)
		   *dd-funs*)))

(add-fun exp (exp var))

(add-fun sin (cos var))

(add-fun cos (- (sin var))

(add-fun tan (/ 1 (expt (cos var) 2)))