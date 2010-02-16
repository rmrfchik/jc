
(define-syntax p
  (syntax-rules ()
    ((_ v) (define (ctx:ctx->
    ((_ v k ...) (list v (p k ...)))))

(display (p 1 2 3))
(newline)


(define-syntax >>=
  (syntax-rules ()
    ((_ p) p)
    ((_ (-> v p) ps ...)
     (lambda (s)
       (let* ((a (p s))
              (v (car a)))
         
         ps ...)))))
   
;(>>= (-> x +))
