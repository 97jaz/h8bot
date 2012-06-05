#lang racket/base

(require xml
         racket/list
         racket/match)
(require "structs.rkt"
         "xmpp.rkt")

(provide (all-defined-out))

(struct room (jid name id topic privacy owner num-participants guest-url archived?) #:transparent)

(define (get-rooms con k)
  (send-iq! con `((type "get") (to ,(connection-host con)))
            '(query ((xmlns "http://jabber.org/protocol/disco#items")))
            (λ (res)
              (let* ([query (car (iq-data res))]
                     [items (children query 'item)])
                (k (map (λ (item)
                          (let ([x (child item 'x)]
                                [attrs (element-attributes item)])
                            (room (attr-ref attrs 'jid)
                                  (attr-ref attrs 'name)
                                  (text (child x 'id))
                                  (text (child x 'topic))
                                  (text (child x 'privacy))
                                  (text (child x 'owner))
                                  (text (child x 'num_participants))
                                  (text (child x 'guest_url))
                                  (child x 'is_archived))))
                        items))))))

(struct user (jid name) #:transparent)

(define (get-roster con k)
  (send-iq! con '((type "get"))
            '(query ((xmlns "jabber:iq:roster")))
            (λ (res)
              (let* ([query (car (iq-data res))]
                     [items (children query 'item)])
                (k (map (λ (item)
                          (let ([attrs (element-attributes item)])
                            (user (attr-ref attrs 'jid)
                                  (attr-ref attrs 'name))))
                        items))))))

(define (join con room-jid)
  (define out (connection-out con))
  (write-string (xexpr->string `(presence ((to ,(string-append room-jid "/" (connection-nick con))))
                                          (x ((xmlns "http://jabber.org/protocol/muc")))))
                out)
  (flush-output out))

(define (say con bare txt)
  (define exp
    (if (string=? (jid-domain bare) (connection-host con))
        `(message ((to ,(string-append bare "/" (connection-nick con)))
                   (type "groupchat"))
                  (body () ,txt))
        `(message ((to ,bare) (type "chat"))
                  (body () ,txt)
                  (inactive ((xmlns "http://jabber/protocol/chatstates"))))))
  (define out (connection-out con))
  (write-string (xexpr->string exp) out)
  (flush-output out))

(define (nick-of roster jid)
  (match roster
    [(cons (user ajid anick) xs)
     (if (equal? jid ajid) 
         anick 
         (nick-of xs jid))]
    [_ #f]))

(define (sobriquet-regexp-strings name)
  (define fst   (regexp-quote (first-name name)))
  (define whole (regexp-quote name))
  
  (format "^(?i:~a|~a)[:,]?\\s*|(^|\\s)@(?i:~a|~a)\\b"
          fst whole fst whole))

(define (first-name name)
  (second (regexp-match #px"^\\s*([^\\s]+)" name)))

(define (make-bot connect-script on-message on-private-message)
  (λ (#:jid jid #:pass pass #:host host #:port [port 5222])
    (connect 
     jid #:host host #:port port #:pass pass #:handlers
     (hasheq
      'connect (λ (con)
                 ;; start a keep-alive thread
                 (thread (λ ()
                           (with-handlers ([(λ (_) #t) void])
                             (let loop ()
                               (write-char #\space (connection-out con))
                               (sleep 60)
                               (loop)))))
                 ;; get a list of rooms and the roster
                 (get-rooms 
                  con 
                  (λ (rooms) 
                    (get-roster 
                     con 
                     (λ (roster)
                       (set-connection-nick! con (nick-of roster jid))
                       ;; run the supplied connection script
                       (connect-script con rooms roster))))))
      
      'message         on-message
      
      'private-message on-private-message))))
                   


  



#;(define (test)
  (connect "7301_15146@chat.hipchat.com" #:nick "Jon Zeppieri" #:host "conf.hipchat.com" #:pass "NoHope4You"
           #:handlers 
           (handle
            ;; connect
            (λ (con)
              (join con "7301_echo_chamber@conf.hipchat.com"))
            ;; message
            (λ (con channel nick body)
              (cond [(regexp-match #rx"(?i:hubot )" body)
                     (when (not (string=? body "Hubot die."))
                       (say con channel "I warned you...")
                       (say con channel "Hubot die."))]
                    [(regexp-match #px"(?i:\\bstag!)" body)
                     (say con channel "http://29.media.tumblr.com/tumblr_lyzybjbQ8u1roa7nbo1_500.jpg")]
                    [(regexp-match #px"SOX|(?i:\\bnot sox compliant\\b)" body)
                     (say con channel "http://skio.org/sox_compliance_kitten.jpg")]
                    [(regexp-match #px"^(?i:\\s*bob me\\b(.*))" body)
                     =>
                     (λ (xs)
                       (let* ([query "bob twin peaks"]
                              [query (if (second xs) (string-append query " " (second xs)) query)]
                              [url   (image-search query)])
                         (when url (say con channel url))))]
                    [(regexp-match #px"(?i:(\\w+\\s+all the things)[!.]?)" body) 
                     =>
                     (λ (xs) 
                       (let ([url (image-search (second xs))])
                         (when url
                           (say con channel url))))]
                    [(phi? body)
                     (say con channel (string-append "/me etches \"[PHI]\" into "
                                                     "his monitor, sets it on fire, and throws it "
                                                     "out the window."))]
;                    [(regexp-match #rx"(?i:h8bot)" body)
;                     (say con channel "Cool story bro.")]
                    [(regexp-match #px"(?i:^(h8bot|@h8bot)[, :\\-!.;])" body)
                     (cond [(regexp-match #rx"(?i:what is best in life)" body)
                            (say con channel (string-append "@" nick ": crush your enemies, "
                                                            "see them driven before you, "
                                                            "and hear the lamentation of the women."))]
                           [else (void)])]
                    [else (void)]))
;              (if (string=? nick (connection-nick con))
;                  (printf "Hey, I talked. I said: ~a\n" body)
;                  (printf "Hey, ~a talked. They said: ~a\n" nick body)))
            ;; private message
            (λ (con channel body)
              (printf "~s\n" body)))))
                                  