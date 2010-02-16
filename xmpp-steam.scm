(load "xmpp-ctx.scm")

(define (xmpp:core:start-stream ctx)
  (let ((stream:stream "<stream:stream xmlns:stream=\"http://etherx.jabber.org/streams\" version=\"1.\0" xmlns=\"jabber:client\" to=\"localhos\t" xml:lang=\"ru-RU\" xmlns:xml=\"http://www.w3.org/XML/1998/namespace\" >"))



(define xmpp:core:streamer-handlers
  `(start-stream ,xmpp:core:start-stream)
  `(stop-stream ,xmpp:core:stop-stream))

(define (xmpp:core:make-streamer host auth-h stanza-h)
  (let ((stream-ctx `((host ,host) `(auth ,auth-h) `(stanza ,stanza-h))))
    (lambda (event ctx)
      (let ((handler (assoc event xmpp:core:streamer-handlers)))
	(handler (xmpp:ctx-put ctx stream-ctx))))))
