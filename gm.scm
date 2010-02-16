;; "main" monad
(require 'ctx)

(define (gm:conn gm)
  (ctx:ctx->connection (gm:value gm)))

(define (g:debug . msg)
  (lambda (gm)
    (apply printf msg)
    gm))

(define (gm:in gm)
  (conn:in (gm:conn gm)))

(define (gm:ctx->value gm)
  (printf "gm:ctx->value: ~s~n" gm)
  (ctx:ctx->value (gm:value gm)))

(define gm:id
  (lambda (app)
    (printf "ID: ~s~n" app)
    app))

(define (gm:gm x)
  (list 'Ok x))
(define (gm:fail msg . value)
  (list 'Fail msg value))
(define (gm:fail? gm)
  (and (list? gm)
       (eq? (car gm) 'Fail)))
(define (gm:value gm)
 ; (printf "gm:value ~s~n" gm)
  (cadr gm))
(define gm:fail-msg cadr)

;; "main" monad combinators
(define (gm:return x)
  (lambda (gm)
;    (printf "Return: ~s~n" x)
    (gm:gm (ctx:value->ctx x (gm:value gm)))))

(define-syntax >>
  (syntax-rules (<-)
		((>> p) 
		 p)
		((>> (<- v key p) ps ...)
		 (lambda (gm)
		   (let* ((a (p gm))
			  (v (ctx:get key (gm:value a))))
		     (if (gm:fail? a)
		       a
		       ((>> ps ...) a)))))
		((>> (<- v p) ps ...)
		 (lambda (gm)
		   (let* ((a (p gm))
			  (v (gm:value a)))
		     (if (gm:fail? a)
		       a
		       ((>> ps ...) a)))))
		((>> p ps ...)
		 (lambda (gm)
		   (let ((a (p gm)))
		     (if (gm:fail? a)
		       a
		       ((>> ps ...) a)))))))

;; 
(define (gm:repeat p)
  (lambda (gm)
    (let loop ((a (p gm)))
      (printf "gm:repeat ~s -> ~s~n" gm a)
      (if (gm:fail? a)
	a
	(loop (p a))))))

(define (putter x)
  (lambda (app)
    (gm:gm (cons x (gm:value app)))))

(define put123
  (>> (putter 1) (putter 2) (putter 3)))

(define carr
  (lambda (app)
    (gm:gm (car (gm:value app)))))

(define car123
  (>> put123 (<- c carr) (gm:return c)))

;(display (car123 (gm:return '())))
;(newline)

(define ctx 
  (ctx:user->ctx "jc@localhost" (ctx:host/port->ctx "localhost" 5222)))
(define p (>> (<- port 'port gm:id) (<- host 'hostname gm:id) (g:debug "port: ~s~n" port) (gm:return port)))
