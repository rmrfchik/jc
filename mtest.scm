(define-syntax p
  (syntax-rules ()
		((p a)
		 (lambda (x)
		 (let ((v (a x)))
		   v)))))

