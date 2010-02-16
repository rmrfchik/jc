(load "xmlp.scm")
;(load "direct.scm")
(define (t name val)
  (printf "~s ~s~n" name val))


;(define inport (with-input-from-string "<?xml version='1.0'" (lambda () (current-input-port))))
;(define instream (inport->charstream inport))
(define instream (list->stream (string->list "<?xml version='1.0'")))
(define a3ba3 (list->stream (string->list "aaabaaa")))
(define b<> (list->stream (string->list "<>")))
(define qname (list->stream (string->list "stream:stream")))
(define tag/a (list->stream (string->list "<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='1216579762' from='localhost' version='1.0' xml:lang='en'>")))
(define tag/a1 (list->stream (string->list "<mechanism>DIGEST-MD5</mechanism>")))
(define tag/noa (list->stream (string->list "<stream:features>")))
(define nvp (list->stream (string->list "xmlns='jabber:client'")))
(define attlist (list->stream (string->list "xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='1216579762' from='localhost' version='1.0' xml:lang='en'"))) 
(define blank (list->stream (string->list " >")))
(define decl/stream (list->stream (string->list "<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='1216579762' from='localhost' version='1.0' xml:lang='en'>

<stream:features>
<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tl\"/>
<mechanisms xmlns\"urn:ietf:params:xml:ns:xmpp-sas\">
<mechanism>DIGEST-MD5</mechanism>
<mechanism>PLAIN</mechanism>
</mechanisms>
<register xmlns=\"http://jabber.org/features/iq-register\"/>
</stream:features>")))

(define stream (list->stream (string->list "<stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='1216579762' from='localhost' version='1.0' xml:lang='en'>

<stream:features>
<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tl\"/>
<mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sas\">
<mechanism>DIGEST-MD5</mechanism>
<mechanism>PLAIN</mechanism>
</mechanisms>
<register xmlns=\"http://jabber.org/features/iq-register\"/>
</stream:features></stream:stream>")))
(define stream (list->stream (string->list "<s>
					   <sf>
					   <st/>
<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tl\"/>
<mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sas\">
<mechanism>DIGEST-MD5</mechanism>
<mechanism>PLAIN</mechanism>
</mechanisms>
					   <ms>
					   <m>DIGEST-MD5</m>
					   <m>PLAIN</m>
					   </ms>
					   <r/>
<register xmlns=\"http://jabber.org/features/iq-register\"/>
					   </sf></s>")))

(t "char" ((mp:char #\a) a3ba3))

(define d (mp:any (mp:char #\0) (mp:char #\1)(mp:char #\2)(mp:char #\3)(mp:char #\4)(mp:char #\5)(mp:char #\6)(mp:char #\7)(mp:char #\8)(mp:char #\9)))

(t "mp:+" ((mp:+ (mp:char #\a)) a3ba3))

(define n (>>= (<- digits (mp:+ d))
	       (m:return (m:foldl (lambda (n d)
				    (+ (* n 10) 
				       (string->number (string d)))) 0 digits))))

(define op (mp:any (mp:char #\+) (mp:char #\-)))

(define tex (list->stream (string->list "12+33")))

(t ">>=" ((>>= (<- v1 n) 
	       op (<- v2 n) 
	       (m:return (list op v1 v2))) tex))

(t "bind" ((mp:bind xp:< xp:>) b<>))
(t "xp:<" (xp:< instream))
(define a (mp:char #\a))
(define b (mp:char #\b))
(define a3 (mp:+/s a))
(define t1 (mp:bind a3 b a3))
(define t1-a (t1 a3ba3))
(display t1-a)
(newline)
(t "qlet" (xp:qlet qname))
(t "qlet+" ((mp:+/s xp:qlet) qname))
(t "a3" (a3 a3ba3))
(t "qname" (xp:qname qname))
(t "blank" (xp:blank blank))
(t "nvp" (xp:attr-nvp nvp))
(t "attr-nvp?" ((mp:? xp:attr-nvp) nvp))
;(t "b/s <qname attr?>" ((mp:bind xp:< xp:qname (mp:? xp:attr-list) xp:>) tag/a))
(t "tag/a" (xp:tag/a* tag/a))

(t "xmldecl" (xp:xml-decl decl/stream))
(t "blank-in-decl" ((mp:bind xp:blank* xp:xml-decl xp:blank*) decl/stream))
(t "start" (xp:start decl/stream))
(t "for-tag" ((xp:for-tag "stream:stream")  tag/a))
(t "!for-tag" ((xp:for-tag "stream:stream")  tag/a1))
(t "tag/noa" (xp:tag/a* tag/noa))
(t "tag/body" (xp:tag/body stream))
