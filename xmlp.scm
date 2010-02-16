(require 'mp)
(require 'xmldom)

(define inputxml "<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='1216579762' from='localhost' version='1.0' xml:lang='en'>


<stream:features>
<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tl\"/>
<mechanisms xmlns\"urn:ietf:params:xml:ns:xmpp-sas\">
<mechanism>DIGEST-MD5</mechanism>
<mechanism>PLAIN</mechanism>
</mechanisms>
<register xmlns=\"http://jabber.org/features/iq-register\"/>
</stream:features>")

;; LOL XML
(define (xml:node type . body)
    (apply list type body))

(define (xml:start-tag tag . attrs)
    (apply xml:node 'st tag attrs))
(define xml:tag-attrs cddr)
  
(define (xml:end-tag tag)
    (xml:node 'et tag))

(define (xml:empty-tag tag . attrs)
  (apply xml:node 'tag tag attrs))

;; ala x-sexp
(define (xml:tag tag attrs . body)
  (apply xml:node 'tag tag attrs body))

(define (xml:a name value)
    (list name value))
(define (xml:cdata data)
  (xml:node 'cdata data))
(define xml:node-value cdr)

(define (xml:node-type? node type)
  (and (list? node)
       (eq? (car node) type)))

(define (xml:start-tag? node)
  (xml:node-type? node 'st))
(define (xml:end-tag? node)
  (xml:node-type? node 'et))
(define (xml:empty-tag? node)
  (xml:node-type? node 'tag))

(define (xml:tag-name node)
  (cadr node))

(define xp:< (mp:char #\<))
(define xp:> (mp:char #\>))
(define xp:/ (mp:char #\/))
(define xp:? (mp:char #\?))
(define xp:qlet (mp:char-parser (lambda (c) (and (char? c)
					    (or (and (char>=? c #\a) (char<=? c #\z))
						(and (char>=? c #\A) (char<=? c #\Z))
						(member c '(#\: #\-)))))
			   "qlet"))

(define xp:qname (>>= (<- str (mp:+/s xp:qlet))
		      (m:return str)))

(define xp:q (mp:any (mp:char #\') (mp:char #\")))
(define xp:nq (mp:char-parser (lambda (c) (and (char? c) (not (or (char=? c #\') (char=? c #\")))))
			 "non-quote"))
(define xp:entity-name
  (>>= (mp:char #\&) (<- name xp:qname) (mp:char #\;) (m:return name)))

(define xp:entities '(("lt" "<")
		      ("gt" ">")
		      ("quot" "\"")
		      ("amp" "&")))

(define xp:entity
 (>>= (<- entity xp:entity-name)
      (let ((val (assoc entity xp:entities)))
	(if val 
	  (m:return (cadr val))
	  (m:fail (format "entity ~s is unknown" entity))))))


(define xp:cdata 
  (>>= 
    (<- cdata (mp:+/s (mp:any xp:entity 
			      (mp:char-parser (lambda (c) 
						(and (char? c)
						     (not (eq? c #\<)))) "non <"))))
    (if (= (string-length cdata) 0)
      (m:fail "cdata expected, got empty")
      (m:return (xml:cdata cdata)))))

(define xp:qstring (>>= xp:q (<- qname (mp:+/s xp:nq)) xp:q (m:return qname)))

(define xp:attr-nvp (>>= (<- name xp:qname)
			 (mp:char #\=)
			 (<- value xp:qstring)
			 (m:return (xml:a name value))))

(define xp:blank (mp:any (mp:char #\space) (mp:char #\newline) (mp:char #\return) (mp:char #\tab)))
(define xp:blank* (mp:? (mp:+ xp:blank)))

(define xp:attr (>>= xp:blank (<- nvp xp:attr-nvp) (m:return nvp)))

(define xp:attr-list (mp:? (mp:+ xp:attr)))

(define xp:tag/close
  (>>= xp:< xp:/ (<- name xp:qname) xp:> (m:return (xml:end-tag name))))

(define xp:tag/a* (>>= xp:< 
		       (<- name xp:qname) 
		       (<- attrs (mp:? xp:attr-list))
		       (mp:? xp:blank)
		       (<- closed (mp:? xp:/))
		       xp:>
		       (m:return 
			 (if (null? closed)
			   ;; TODO, how to deal with <aa/>?
			   (apply xml:start-tag name attrs)
			   (apply xml:empty-tag name attrs)))))

(define (xp:for-tag tag-name)
  (>>= 
    xp:blank*
    (<- tag xp:tag/a*)
    (if (string-ci=? tag-name (xml:tag-name tag))
      (m:return tag)
      (m:fail (format "~s expected, got ~s" tag-name tag)))))

(define (xp:for-tag/body tag-name body-parser)
  (>>=
    (xp:for-tag tag-name)
    (<- body body-parser)
    (xp:for-tag/close tag-name)
    (m:return body)))

(define (xp:for-tag/close tag-name)
  (>>=
    xp:blank*
    (<- tag xp:tag/close)
    (if (string-ci=? tag-name (xml:tag-name tag))
      (m:return tag)
      (m:fail (format "close ~s expected, got ~s" tag-name tag)))))

;; processing instruction
(define xp:processing
  (>>=
    xp:< xp:? 
    (<- name xp:qname) 
    (<- attrs (mp:? xp:attr-list))
    xp:? xp:>
    (m:return (xml:node 'pr name attrs))))

(define xp:xml-decl (>>= 
		      (<- pr xp:processing)
		      (if (string=? (xml:tag-name pr) "xml")
			(m:return pr)
			(m:fail "xml declaration expected"))))
;(define xp:stream (xp:tag/a "stream:stream")) ;; tag with attrs


;; node sequence, sequence 

;; parse a tag with a whole body 
;; "tag" is a processing instruction or ta
(define xp:tag/body
  (mp:any xp:processing
	  (>>=
	    (<- tag xp:tag/a*)
	    (<- body (mp:+ xp:tag/body))
	    (xp:for-tag/close (xml:tag-name tag))
	    (m:return (apply xml:node 'tag (xml:tag-name tag) (xml:tag-attrs tag) body)))
	  (>>=
	    (<- tag xp:tag/a*)
	    (if (xml:empty-tag? tag)
	      (m:return tag)
	      (m:fail "empty-tag expected")))
	  xp:cdata))

(define xp:stream xp:tag/a*)
(define xp:xml (>>=  xp:xml-decl xp:blank*))

;; make dom from events
(define xp:dom
  (mp:any (>>= 
	    (<- processing xp:processing)
	    (m:return (dom:make-tag (xml:tag-name processing) (xml:tag-attrs processing) '())))
	  (>>=
	    (<- tag xp:tag/a*)
	    (<- body (mp:+ xp:dom))
	    (xp:for-tag/close (xml:tag-name tag))
	    (m:return (dom:make-tag (xml:tag-name tag) (xml:tag-attrs tag) body)))
	  (>>=
	    (<- tag xp:tag/a*)
	    (if (xml:empty-tag? tag)
	      (m:return (dom:make-tag (xml:tag-name tag) (xml:tag-attrs tag) '()))
	      (m:fail "empty-tag expected")))
	  xp:cdata))

