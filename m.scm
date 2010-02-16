(load "mp.scm")
(define xp:qname (mp:+/s xp:qlet))
(define xp:q (mp:any (mp:char #\') (mp:char #\")))
(define xp:nq (mp:parser (lambda (c) (and (char? c) (not (or (char=? c #\') (char=? c #\")))))))

(define xp:qstring (mp:bind/s xp:q (mp:+ xp:nq) xp:q))
(define xp:attr-nvp (mp:bind/s xp:qname (mp:char #\=) xp:qstring))
(define xp:blank (mp:any (mp:char #\space) (mp:char #\return) (mp:char #\tab)))
(define xp:blank* (mp:? (mp:+ xp:blank)))

(define xp:attr (mp:bind/s xp:blank xp:attr-nvp))

(define xp:attr-list 
  (=>
    (? (mp:+ xp:attr))))

(define (tag name)
  (list 'TAG name))

(define xp:tag/a* 
  (>>=
    xp:< 
    (<- name xp:qname)
    (mp:? xp:attr-list) 
    (mp:? xp:/) 
    xp:>
    (-> (tag name))))

