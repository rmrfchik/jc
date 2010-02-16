(define (id x) x)
(define (tr x)
  (display x)
  (newline)
  x)

;; ENDLESS STREAM
(define (stream:car stream)
  (car (force stream)))
(define (stream:cdr stream)
  (cdr (force stream)))

(define (list->stream lst)
  (delay (cons (if (null? lst) #f (car lst)) (list->stream (if (null? lst) lst (cdr lst))))))
(define (string->stream str)
  (list->stream (string->list str)))
(define (reader->stream reader)
  (delay (cons (reader) (reader->stream reader))))
(define (inport->charstream inport)
  (delay (cons (read-char inport) (inport->charstream inport))))
(define (stream:print stream)
  (let loop ((stream stream))
    (let ((c (stream:car stream)))
      (if (and c (not (eof-object? c)))
	(begin
	  (display c)
	  (loop (stream:cdr stream)))
	(void)))))

(define (mp:return* value consumed rest)
  (list value consumed rest))

(define (mp:update-consumed mp mp2)
  (append (mp:consumed mp) (mp:consumed mp2)))

(define (mp:return-update-consumed mp mp2)
  (mp:return* (mp:value mp2) (mp:update-consumed mp mp2) (mp:stream mp2)))

(define (mp:return value rest)
  (mp:return* value '() rest))
(define mp:consumed cadr)
(define (mp:consumed->string mp)
  (list->string (mp:consumed mp)))

(define (mp:fail* msg s)
  ;(printf "fail*: ~s ~n" msg)
  (mp:return (list 'fail msg) s))
(define (mp:value mp)
  (car mp))
(define (mp:stream mp)
  (caddr mp))
(define mp:zero
  (mp:return* '() '() '()))

(define (mp:fail? ctx)
  (and (pair? ctx)
       (pair? (car ctx))
       (eq? (caar ctx) 'fail)))

(define (m:foldl f s lst)
  (tr (format "Fold: ~s ~s ~s~n" f s lst))
  (let loop ((s s)
	     (lst lst))
    (if (null? lst)
      s
      (loop (f s (car lst)) (cdr lst)))))

(define-syntax >>=
  (syntax-rules (<-)
		((>>= p) 
		 p)
		((>>= (<- v p) ps ...)
		 (lambda (s)
		   (let* ((a (p s))
			  (v (mp:value a)))
		     (if (mp:fail? a)
		       a
		       (let ((a2 ((>>= ps ...) (mp:stream a))))
			 (mp:return-update-consumed a a2))))))
		((>>= p ps ...)
		 (lambda (s)
		   (let ((a (p s)))
		     (if (mp:fail? a)
		       a
		       (let ((a2 ((>>= ps ...) (mp:stream a))))
			 (mp:return-update-consumed a a2))))))))

(define-syntax >>==
  (syntax-rules (<-)
		((>>= p) 
		 p)
		((>>= (<- v p) ps ...)
		 (lambda (s)
		   (let* ((a (p s))
			  (v (mp:value a)))
		     (printf "a: ~s~n" a)
		     (if (mp:fail? a)
		       a
		       (let ((a2 ((>>= ps ...) (mp:stream a))))
			 (mp:return-update-consumed a a2))))))
		((>>= p ps ...)
		 (lambda (s)
		   (let ((a (p s)))
		     (printf "a: ~s~n" a)
		     (if (mp:fail? a)
		       a
		       (let ((a2 ((>>= ps ...) (mp:stream a))))
			 (mp:return-update-consumed a a2))))))))

(define (m:return value)
  (lambda (s)
    (mp:return value s)))

(define (m:fail msg)
  (lambda (s)
    (mp:fail* msg s)))

(define (mp:debug msg)
  (lambda (s)
    (printf "DEBUG: ")
    (printf msg)
    (mp:return msg s)))

(define (mp:guard parser func)
  (lambda (stream)
    (let ((a (parser stream)))
      (if (mp:fail? a)
	a
	(mp:return* (func (mp:value a)) (mp:consumed a) mp:stream a)))))

(define (mp:char-parser* guard? func msg)
  (lambda (stream)
    (let ((c (stream:car stream)))
      (if (guard? c)
	(mp:return* (func c) (list c) (stream:cdr stream))
	(mp:fail* (format "char: ~a expected, got ~a" msg c) stream)))))

(define (mp:char-parser guard? msg)
  (mp:char-parser* guard? id msg))

(define (mp:list-folder collected-answer answer)
  (mp:return* (append (mp:value collected-answer)
		      (list (mp:value answer)))
	      (mp:update-consumed collected-answer answer)
	      (mp:stream answer)))

(define (a->string a)
  (if (string? a)
    a
    (if (list? a)
      (list->string a)
      (string a))))

(define (mp:string-folder mp mp2)
  (mp:return* (string-append (mp:value mp) (a->string (mp:value mp2)))
	      (mp:update-consumed mp mp2)
	      (mp:stream mp2)))

(define (mp:any . parsers)
  (lambda (stream)
    (let loop ((parsers parsers))
      (if (null? parsers)
	(mp:fail* "any: no any choices" stream)
	(let* ((parser (car parsers))
	       (ans (parser stream)))
	  (if (mp:fail? ans)
	    (loop (cdr parsers))
	    ans))))))

(define (mp:? parser)
  (lambda (stream)
    (let ((ans (parser stream)))
      (if (mp:fail? ans)
	(mp:return '() stream)
	ans))))

(define (mp:+* folder start parser)
  (lambda (stream)
    (let loop ((stream stream)
	       (answer (mp:return* start '() stream)))
      (let ((a (parser stream)))
	(if (mp:fail? a)
	  answer
	  (loop (mp:stream a) (folder answer a)))))))

(define (mp:+ parser)
  (mp:+* mp:list-folder '() parser))

(define (mp:+/s parser)
  (mp:+* mp:string-folder "" parser))

(define (mp:char c)
  (mp:char-parser (lambda (elt)
	       (eq? c elt)) (format "char ~a" c)))
