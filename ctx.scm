;; connection context
;; contains input/output ports, user name, server info, etc...
(require 'srfi-1)

;; creates new empty context
(define ctx:empty
  '())

(define (ctx:put key datum ctx)
  (alist-cons key datum (alist-delete key ctx)))

(define (ctx:get key ctx)
  (let ((datum (assoc key ctx)))
    (if datum (cdr datum) datum)))

(define (ctx:conn->ctx conn ctx)
  (ctx:put 'conn conn ctx))
(define (ctx:conn ctx)
  (ctx:get 'conn ctx))

(define (ctx:user->ctx user ctx)
  (ctx:put 'user user ctx))
(define (ctx:ctx->user ctx)
  (ctx:get 'user ctx))

(define (ctx:password->ctx password ctx)
  (ctx:put 'password password ctx))
(define (ctx:ctx->password ctx)
  (ctx:get 'password ctx))

(define (ctx:hostname->ctx hostname ctx)
  (ctx:put 'hostname hostname ctx))
(define (ctx:ctx->hostname ctx)
  (ctx:get 'hostname ctx))

(define (ctx:port->ctx port ctx)
  (ctx:put 'port port ctx))
(define (ctx:ctx->port ctx)
  (ctx:get 'port ctx))

(define (ctx:value->ctx value ctx)
  (ctx:put 'value value ctx))
(define (ctx:ctx->value ctx)
  (ctx:get 'value ctx))

(define (ctx:connection->ctx connection ctx)
  (ctx:put 'connection connection ctx))
(define (ctx:ctx->connection ctx)
  (ctx:get 'connection ctx))

(define (ctx:host/port->ctx hostname port)
  (ctx:port->ctx port (ctx:hostname->ctx hostname ctx:empty)))
