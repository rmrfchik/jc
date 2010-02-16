(define (port-stream reader)
  (delay (cons (reader) (port-stream reader))))

(define (list-stream lst)
  (delay (cons (car lst) (list-stream (cdr lst)))))

(define (stream:car stream)
  (car (force stream)))

(define (stream:cdr stream)
  (cdr (force stream)))
