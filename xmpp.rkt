#lang racket/base

(require xml
         net/base64
         openssl
         racket/list
         racket/match
         racket/tcp)

(require (for-syntax racket/base))
(require "structs.rkt")

(provide (all-defined-out))


(define (stream-error! con msg)
  (close-output-port (connection-out con))
  (error msg))

(define (connect jid #:handlers handlers #:host host #:port [port 5222] #:pass pass #:lang [lang "en"])
  (let-values ([(in out) (tcp-connect host port)])
    (let ([to (jid-domain jid)])
      (authenticate (connection host in out to lang jid #f pass handlers)))))

(define (authenticate con)
  (match con
    [(connection host in out to lang jid _ _ handlers)
     (initiate out jid to lang)
  
     (match (regexp-try-match #px"^.*?[^?]>" in 0 1024)
       [#f (error "Server did not send a stream")]
       [_  (void)])
  
     (match (read-xml/element in)
       ;; start TLS
       [(X 'stream:features _
           (cons (X 'starttls _ _) _))
        (starttls! con)]
       ;; auth and binding
       [(and (X 'stream:features _ _) elt)
        ;; TODO: support more auth options
        (let ([mechs (get-mechanisms elt)])
          (if mechs
              (cond [(member "PLAIN" mechs)
                     (plain-auth! con)]
                    [else
                     (error (format "Auth mechanisms not supported: ~s" mechs))])
              (begin
                (bind-id! con (hash-ref handlers 'connect null-handler))
                (start con))))]
       [x
        (stream-error! con (format "Unexpected response: ~s" x))])]))

(define (start con)
  (let loop ()
    (match (read-xml/element (connection-in con))
      [(X 'iq attrs data)
       (let* ([id   (attr-ref attrs 'id)]
              [type (attr-ref attrs 'type)]
              [fn   (hash-ref iq-table id #f)])
         (when fn (fn (iq type data))))]
      [(and x (X 'message attrs data))
       (let ([type (attr-ref attrs 'type)]
             [body (text (child x 'body))]
             [from (attr-ref attrs 'from)])
         (when body 
           (match type
             ["groupchat" (when (not (child x 'delay))
                            (emit con 'message  (list (jid-bare from) (jid-resource from) body)))]
             ["chat" (emit con 'private-message (list (jid-bare from) body))]
             [_ (printf "Received: ~s\n" (xml->xexpr x))])))]
      [(X 'presence _ body)
       (void)]
      [x
       (printf "~s\n\n" (xml->xexpr x))
       (stream-error! con "FIXME")])
    
    (loop)))

(define (emit con accessor args)
  (let* ([handlers (connection-handlers con)]
         [fn       (hash-ref handlers accessor null-handler)])
    (apply fn con args)))

(define (starttls! con)
  (define in  (connection-in con))
  (define out (connection-out con))
  
  (write-string "<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>" out)
  (flush-output out)
  
  (match (read-xml/element in)
    [(X 'proceed _ _)
     (let-values ([(sin sout) (ports->ssl-ports in out)])
       (authenticate (struct-copy connection con [in sin] [out sout])))]
    [_ 
     (stream-error! con "Server failed to start TLS")]))

(define (plain-auth! con)
  (define in  (connection-in con))
  (define out (connection-out con))
  
  (define username (string->bytes/utf-8 (connection-jid  con)))
  (define domain   (string->bytes/utf-8 (connection-to   con)))
  (define password (string->bytes/utf-8 (connection-pass con)))
  
  (define byte-data 
    (bytes-append username #"@" domain #"\0" username #"\0" password))

  (define enc-port      (open-output-bytes))
  (base64-encode-stream (open-input-bytes byte-data) enc-port #"")
  (define encoded       (bytes->string/utf-8 (get-output-bytes enc-port)))
  
  (write-string 
   (string-append "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='PLAIN'>"
                  encoded
                  "</auth>")
   out)
  (flush-output out)
  
  (match (read-xml/element in)
    [(X 'success _ _) (authenticate con)]
    [_                (stream-error! con "Auth failed")]))

(define (bind-id! con on-connect)
  (send-iq! con '((type "set")) '(bind ((xmlns "urn:ietf:params:xml:ns:xmpp-bind")))
            (λ (x)
              (match x
                [(iq "result" (list (X 'bind _ (list (X 'jid _ (list (pcdata _ _ jid)))))))
                 (set-connection-jid! con jid)
                 (on-connect con)]
                [_ (stream-error! con (format "Failed to bind JID resource: ~s" x))]))))

(define (send-iq! con attrs xexpr handler)
  (define out (connection-out con))
  (define id  (next-id))
  (define a   (cons (list 'id id) attrs))
  (define x  `(iq ,a ,xexpr))
  
  (hash-set! iq-table id handler)
  
  (write-string (xexpr->string x) out)
  (flush-output out))
     
(define (get-mechanisms elt)
  (define mechs (child elt 'mechanisms))
  (and mechs
       (map (λ (x)
              (match x
                [(X 'mechanism _ (list (pcdata _ _ s))) s]
                [_ ""]))
            (element-content mechs))))

(define (initiate out from to lang)
 (define txt
   (string-append "<?xml version='1.0'?>"
                  "<stream:stream"
                  "  from='" from "'"
                  "  to='" to "'"
                  "  version='1.0'"
                  "  xml:lang='" lang "'"
                  "  xmlns='jabber:client'"
                  "  xmlns:stream='http://etherx.jabber.org/streams'>"))
 (write-string txt out)
 (flush-output out))
