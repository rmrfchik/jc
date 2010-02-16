(require 'xmppp)
(require 'ctx)
(require 'jcommand)

(define (process-command command ctx)
  (let ((parsed ((jc:command ctx) (string->stream command))))
    (if (mp:fail? parsed)
      'Fail
      (begin
	(printf "COMMAND: ~s~n" parsed)
	(let ((v ((mp:value parsed))))
	  (printf "RESULT: ~s~n" v)
	  v)))))

(define (xmpp:process-message dom ctx)
  (>>
    (let* ((attr (dom:attrs dom))
	   (from (dom:attr-value attr "from"))
	   (lang (dom:attr-value attr "xml:lang"))
	   (dom-body (dom:body dom))
	   (body (assoc "body" dom-body))
	   (composing (assoc "composing" dom-body)))
      (if body
	(let* ((text-cdata (dom:body body))
	       (text (cadar text-cdata))
	       (answer (process-command text ctx)))
	  (xmpp:send** (format "<message to=\"~a\" from=\"~a\" xml:lang=\"~a\" type=\"chat\" id=\"~a\"><body>~a</body><active xmlns=\"http://jabber.org/protocol/chatstates\"/></message>" from "jc@localhost" lang "aaab" answer)))
	(gm:return dom)))))

(define (xmpp:process-presence dom)
  (>>
    (gm:return dom)))

(define ctx 
  (ctx:user->ctx "jc" (ctx:password->ctx "q1" (ctx:host/port->ctx "localhost" 5222))))

(define client (xmpp:client ctx))

(define (main client)
  (printf "client: ~s~n" client)
  (xmpp:client-loop client xmpp:process-message))

(main client)
