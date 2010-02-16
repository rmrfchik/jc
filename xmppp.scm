(require 'base64)
(require 'md5)

(require 'mp)
(require 'xmlp)
(require 'connman)
(require 'gm)
(require 'auth)

(define client:greeting
  "<?xml version=\"1.0\"?>
  <stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.0\" xmlns=\"jabber:client\" to=\"localhost\" xml:lang=\"ru-RU\" xmlns:xml=\"http://www.w3.org/XML/1998/namespace\">")
(define client:bind "<iq type=\"set\" id=\"bind_1\" >
  <bind xmlns=\"urn:ietf:params:xml:ns:xmpp-bind\">
  <resource>jc</resource>
  </bind>
  </iq>")
(define client:session "<iq xmlns=\"jabber:client\" type=\"set\" id=\"sess_1\" >
  <session xmlns=\"urn:ietf:params:xml:ns:xmpp-session\"/>
  </iq>")
(define client:get-roster "<iq xmlns=\"jabber:client\" type=\"get\" id=\"roster_1\" >
  <query xmlns=\"jabber:iq:roster\"/>
  </iq>")
(define client:presence "<presence xmlns=\"jabber:client\">
  <priority>55</priority>
  <c xmlns=\"http://jabber.org/protocol/caps\" node=\"jc\" ver=\"0.10\"/>
  </presence>")

(define digest-uri "xmpp/localhost")


(define client:auth-start
  "<auth xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\" mechanism=\"DIGEST-MD5\" />")
(define (client:auth-response response)
  (format "<response xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\">~a</response>" response))


(define (xmpp:send conn line)
  ;(display "CLIENT: ")
  ;(display line)
  ;(newline)
  (display line (conn:out conn)))

(define (xmpp:send* line)
  (lambda (gm)
    (xmpp:send (gm:conn gm) line)
    gm))

(define (xmpp:send** line)
  (lambda (gm)
    (printf "CLIENT: ~a~n" line)
    (xmpp:send (gm:conn gm) line)
    gm))

(define (xmpp:gen-id)
  (lambda (gm)
    (gm:return (format "~a" (random 100000000)))))

(define xmpp:bind (>>=
		    (xp:for-tag "bind")
		    (<- jid (xp:for-tag "jid"))
		    (xp:for-tag/close "bind")
		    (m:return jid)))
(define xmpp:session (>>= (xp:for-tag "session")))

(define xmpp:iq/result
  (>>=
    (xp:for-tag "iq")
    (<- result (mp:any xmpp:query xmpp:bind xmpp:session))
    (xp:for-tag/close "iq")
    (m:return result)))
	

;; send IQ stanza with given type and sub-stanza and returns parsed answer
(define (xmpp:send-iq/result type stanza)
  (>>
    (<- id xmpp:gen-id)
    (xmpp:send* (format "<iq type=\"~a\" id=\"~a\">" type id))
    (xmpp:send* stanza)
    (xmpp:send* "</iq>")
    (<- result (xmpp:in xmpp:iq/result id))
    (gm:return result)))

;; j: namespace is for jabber structures
(define (j:mechanism mechanism)
  mechanism)
(define (j:feature feature)
  feature)

(define xmpp:start-stream (xp:for-tag "stream:stream"))

(define xmpp:mechanism (>>= (xp:for-tag "mechanism")
			    (<- mechanism xp:cdata)
			    (xp:for-tag/close "mechanism")
			    (m:return (j:mechanism (car (xml:node-value mechanism))))))

(define (xmpp:feature feature)
  (>>= (<- feat (xp:for-tag feature))
       (m:return (j:feature feature))))
;; name-value parser. name=value,name1=value,...
(define nvp:name xp:qname)
(define nvp:qv xp:qstring)
(define nvp:non-comma (mp:char-parser (lambda (c) (and (char? c) (not (char=? c #\,))))
			 "non-quote"))
(define nvp:v (>>= (<- value (mp:+/s nvp:non-comma)) (m:return value)))
(define nvp:pair (>>= (<- name nvp:name)
		      (mp:char #\=)
		      (<- value (mp:any nvp:qv nvp:v))
		      (mp:? (mp:char #\,))
		      (m:return (cons name value))))

(define nvp:p (mp:+ nvp:pair))

(define (string->challenge str)
  (let* ((stream (string->stream str))
	 (pairs (nvp:p stream)))
    (mp:value pairs)))
(define (nvp:get-value name nvps) (cdr (assoc name nvps)))
(define xmpp:feature-bind (xp:for-tag "bind"))
(define xmpp:feature-session (xmpp:feature "session"))
(define xmpp:feature-starttls (xmpp:feature "starttls"))
(define xmpp:register (xmpp:feature "register"))
(define xmpp:feature-mechanisms (>>=
				  (xp:for-tag "mechanisms")
				  (<- mechanism-list (mp:+ xmpp:mechanism))
				  (xp:for-tag/close "mechanisms")
				  (m:return (cons 'mechanisms mechanism-list))))

(define xmpp:feature-list (mp:+ (mp:any xmpp:feature-bind xmpp:feature-session xmpp:feature-mechanisms xmpp:feature-starttls xmpp:register)))

(define xmpp:features (>>= (xp:for-tag "stream:features")
			   (<- feature xmpp:feature-list)
			   (xp:for-tag/close "stream:features")
			   (m:return feature)))

(define xmpp:client/start (>>= (<- xml xp:xml) 
			       xmpp:start-stream
			       (<- features xmpp:features)
			       (m:return features)))

(define xmpp:challenge (>>= (xp:for-tag "challenge")
			    (<- challenge xp:cdata)
			    (xp:for-tag/close "challenge")
			    (m:return (string->challenge (base64#base64-decode
							   (car (xml:node-value challenge)))))))
(define xmpp:success (xp:for-tag "success"))
(define (xmpp:rspauth conn) 
  (>>= 
    xmpp:challenge
    (m:return (xmpp:send conn "<response xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>"))
    xmpp:success))

(define xmpp:get-connection
  (lambda (gm)
    (gm:gm (get-connection (gm:value gm)))))

;; app monad to parse input xml
(define (xmpp:in parser)
  (lambda (gm)
    (let* ((parsed (parser (gm:in gm)))
           (value (mp:value parsed)))
      (if (mp:fail? parsed)
        (gm:fail value)
        ;; update gm with new value and new input stream from result
        (begin
          (gm:gm (ctx:value->ctx 
                   value 
                   (ctx:connection->ctx (mp/conn->conn parsed (gm:conn gm)) (gm:value gm)))))))))

(define xmpp:in-client/start 
  (xmpp:in xmpp:client/start))
(define xmpp:in-challenge
  (xmpp:in xmpp:challenge))

(define (mp/conn->conn mp conn)
  (make-connection (mp:stream mp) (conn:out conn)))

(define (xmpp:auth-response user password challenge)
  (define realm "")
  (printf "xmpp:auth-response ~s ~s ~s ~n" user password challenge)
  (gm:return (sasl:response (nvp:get-value "nonce" challenge) (sasl:cnonce) realm user password "AUTHENTICATE" digest-uri "00000001" (nvp:get-value "qop" challenge))))

(define xmpp:bind
  (>>= (xp:for-tag "bind")
       (xp:for-tag "jid")
       (<- jid xp:cdata)
       (xp:for-tag/close "jid")
       (xp:for-tag/close "bind")
       (m:return jid)))

(define (xmpp:iq payload)
  (>>= (xp:for-tag "iq")
       (<- payresult payload)
       (xp:for-tag/close "iq")
       (m:return payresult)))
(define xmpp:query (xp:for-tag "query"))
(define xmpp:item (xp:for-tag "item"))

;; bind, set, get roster, set presence
(define xmpp:client-bind
  (>>
    (xmpp:send* client:bind)
    (<- jid (xmpp:in (xmpp:iq xmpp:bind)))

    (xmpp:send* client:session)
    (<- sess (xmpp:in (xmpp:iq xmpp:session)))

    (gm:return sess)))

(define xmpp:get-roster
  (>>
    (xmpp:send* client:get-roster)
    (<- query (xmpp:in (xmpp:iq (>>= (xp:for-tag/body "query" xmpp:item)))))
    (gm:return query)))

(define xmpp:presence
  (>>= 
    (<- presence (xp:for-tag/body "presence" (>>= (xp:for-tag/body "priority" xp:cdata) (xp:for-tag "c"))))
    (m:return presence)))

(define xmpp:set-presence
    (xmpp:send* client:presence))

(define xmpp:get-message
  (lambda (gm)
    (printf "xmpp:get-message ~s~n: " gm)
    ((xmpp:in xp:dom) gm)))

(define (xmpp:put-answer request process)
  (lambda (gm)
    ((>>
       (g:debug "gm ~s~n" gm)
       (let* ((dom request)
              (tag (dom:tag dom)))
         (printf "RT: ~s~n" tag)
         (cond
           ((string=? tag "message")
            (process dom gm))
           ((string=? tag "presence")
            (process dom gm))
           (else (gm:return request))))) gm)))

(define xmpp:connect
  (>> xmpp:get-connection 
      (xmpp:send* client:greeting)
      xmpp:in-client/start
      (xmpp:send* client:auth-start)
      (<- challenge xmpp:in-challenge)
      (<- auth (xmpp:auth-response (ctx:ctx->user challenge) (ctx:ctx->password challenge) (ctx:ctx->value challenge)))
      (xmpp:send* (client:auth-response (ctx:ctx->value auth)))
      (<- authed xmpp:in-challenge)
      (xmpp:send* "<response xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\"/>")
      (<- s (xmpp:in xmpp:success))

      (xmpp:send* client:greeting)

      (<- greet2 xmpp:in-client/start)
      xmpp:client-bind
      (<- roster xmpp:get-roster)

      (<- presence xmpp:set-presence)
      (gm:return 'Ok)))

(define (xmpp:client ctx)
  (xmpp:connect (gm:gm ctx)))

(define (xmpp:request/response process)
  (>>
    (<- request 'value xmpp:get-message)
    (g:debug "R: ~s~n" request)
    (xmpp:put-answer request process)))

(define (xmpp:client-loop client process)
  ((gm:repeat (xmpp:request/response process)) client))

;(printf "loop: ~s~n" (xmpp:client-loop (xmpp:client (ctx:host/port->ctx "localhost" 5222))))
