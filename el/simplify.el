(add-simplify-rule *
		   (if (some (lambda (x) (eql x 0)) expr)
		       0
		     expr)
		   (remove-if (lambda (x) (eql x 1)) expr))

(add-simplify-rule +
		   (remove-if (lambda (x) (eql x 0)) expr))