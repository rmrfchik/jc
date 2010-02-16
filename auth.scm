(define (sasl:a1 realm-value username passwd nonce cnonce)
  (md5#md5-digest (string-append 
		    (md5#md5-binary-digest 
		      (string-append username ":" realm-value ":" passwd))
		    ":" nonce ":" cnonce)))
  
(define (sasl:a2 method digest-uri)
  (md5#md5-digest (string-append method ":" digest-uri)))

(define (sasl:cnonce)
  (let loop ((rnd '())
	     (n 8))
    (if (= n 0)
      (md5#md5-digest (list->string (map integer->char rnd)))
      (loop (cons (random 255) rnd) (- n 1)))))

(define sasl:hex-h md5#md5-digest)

(define (sasl:response nonce cnonce realm username password method digest-uri nc qop)
  (let* ((ha1 (sasl:a1 realm username password nonce cnonce))
	 (ha2 (sasl:a2 method digest-uri)))
    (let ((response (md5#md5-digest (string-append ha1 ":" nonce ":" nc ":" cnonce ":" qop ":" ha2))))
      (base64#base64-encode (format "username=~s,nonce=~s,cnonce=~s,nc=~a,digest-uri=~s,qop=~s,response=~s" username nonce cnonce nc digest-uri qop response)))))
