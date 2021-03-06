;;
;; (TAG ATTRS BODY)
;; attrs: ((attr value) ...)
;; BODY: (CDATA | tag)
(define (dom:make-attrs . attrs)
  attrs)
(define (dom:make-attr attr value)
  (cons attr value))
(define (dom:make-tag tag attrs body)
  (list tag attrs body))

(define (dom:tag? dom)
  (and (list? dom)
       (= 3 (length dom))))
(define dom:cdata? string?)

(define (dom:tag dom)
  (car dom))
(define (dom:attrs dom)
  (cadr dom))
(define (dom:body dom)
  (caddr dom))
(define (dom:attr-value attrs attr)
  (let ((nvp (assoc attr attrs)))
    (if nvp (cadr nvp) '())))

(define dom (dom:make-tag 'a (dom:make-attrs (dom:make-attr 'q "ww")
					     (dom:make-attr 'w "qq")) "mycdata"))
(printf "~s~n~s~n~s~n" (dom:tag dom) (dom:attrs dom) (dom:body dom))
