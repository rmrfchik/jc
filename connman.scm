(require 'tcp)
(require 'ctx)
(tcp-read-timeout #f)

(define (make-connection instream oport)
  (cons instream oport))

(define conn:in car)
(define conn:out cdr)

(define decl/stream (string->stream "<?xml version='1.0'?><stream:stream xmlns='jabber:client' xmlns:stream='http://etherx.jabber.org/streams' id='1216579762' from='localhost' version='1.0' xml:lang='en'>


						<stream:features>
						<starttls xmlns=\"urn:ietf:params:xml:ns:xmpp-tl\"/>
						<mechanisms xmlns=\"urn:ietf:params:xml:ns:xmpp-sas\">
						<mechanism>DIGEST-MD5</mechanism>
						<mechanism>PLAIN</mechanism>
						</mechanisms>
						<register xmlns=\"http://jabber.org/features/iq-register\"/>
						</stream:features>"))
(define tls/stream (string->stream "<challenge xmlns=\"urn:ietf:params:xml:ns:xmpp-sasl\">bm9uY2U9IjM1ODYzMjE3NjkiLHFvcD0iYXV0aCIsY2hhcnNldD11dGYtOCxhbGdvcml0aG09bWQ1LXNlc3M=</challenge>"))
(define (tcp-stream host port)
  (let-values (((in out) (tcp-connect host port)))
    (make-connection (reader->stream (lambda () (let ((c (read-char in)))
						  ;(display c)
						  c))) 
		     out)))

(define (get-connection* ctx)
  ;(make-connection decl/stream (current-output-port)))
  (tcp-stream (car ctx) (cdr ctx)))

(define (get-connection ctx)
  ;(make-connection decl/stream (current-output-port)))
  (ctx:connection->ctx (tcp-stream (ctx:ctx->hostname ctx) (ctx:ctx->port ctx)) ctx))

(define (switch-tls ctx)
  (make-connection tls/stream (cdr ctx)))
