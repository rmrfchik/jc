(require 'xmlp)
(define newline-string (string #\newline))
(define (jc:do-help)
  (format "help: print this help~neval: eval a sexp"))

;; escape < > to entity
(define (escape-special str)
  (define specials '((#\< "&lt;")
		     (#\> "&gt;")))
  (let loop ((strings (map (lambda (c)
		       (let  ((a (assoc c specials)))
			 (if a (cadr a) (string c))))
		     (string->list str)))
	     (result ""))
    (if (null? strings) result (loop (cdr strings) (string-append result (car strings))))))


(define (jc:do-eval sexp)
  (lambda ()
    (let* ((cmdline (format "csi -p '~a' 2>&1" sexp))
	   (port (open-input-pipe cmdline)))
      (printf "CMDLINE: ~a~n" cmdline)
      (let loop ((l (read-line port))
		 (a ""))
	(if (eof-object? l)
	  (escape-special a)
	  (loop (read-line port) (string-append a l newline-string)))))))

(define (jc:help ctx)
  (>>=
    (<- comm xp:qname)
    (if (string=? comm "help")
      (m:return jc:do-help)
      (m:fail (format "help expected. got ~s" comm)))))

(define (jc:eval ctx)
  (>>=
    (<- comm xp:qname)
    (if (string=? comm "eval")
      (>>=
	(<- sexp (mp:+/s (mp:char-parser char? "")))
	(m:return (jc:do-eval sexp)))
      (m:fail (format "eval expected. got ~s" comm)))))

(define (jc:command ctx)
  (mp:any 
    (jc:help ctx)
    (jc:eval ctx)))

