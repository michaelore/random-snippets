(add-simplify-rule *
		   (if (some (lambda (x) (eql x 0)) expr)
		       0
		     expr)
		   (mapcan (lambda (x) (if (eql x 1) '() x)) expr))

(add-simplify-rule +
		   (mapcan (lambda (x) (if (eql x 0) '() x)) expr))

(add-simplify-rule /
		   (cond ((memq (cadr expr) (cddr expr))
			  (