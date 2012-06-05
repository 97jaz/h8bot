#lang racket

(require net/url
         net/uri-codec)
(require (planet dherman/json:4:0))
(require "bot.rkt"
         "uni.rkt"
         "structs.rkt")
         
(provide (all-defined-out))

(define memory (make-hash))
(define rooms '())
(define roster '())
(define jid-map (make-hash))

(define (user-nick jid)
  (match (hash-ref jid-map jid)
    [(user _ name) name]
    [_             "???"]))


(define (get-invocations name)
  (let ([history (hash-ref memory name #f)])
    (if history
        (hash-ref history 'invocations '())
        '())))

(define (add-invocation! name)
  (let* ([history     (hash-ref! memory name (make-hash))]
         [invocations (hash-ref! history 'invocations '())])
    (hash-set! history 'invocations (cons (current-seconds) invocations))))

(define-syntax define/occasional
  (syntax-rules (minutes)
    [(_ (name args ...) [n minutes] body ...)
     (define (name args ...)
       (let ([invocations (get-invocations 'name)])
         (when (or (null? invocations)
                   (<= (+ (first invocations) (* 60 n)) (current-seconds)))
           body ...)))]))
  
(define (sample v)
  (vector-ref v (random (vector-length v))))


(define (conan txt say from)
  (when (regexp-match? #rx"(?i:what is best in life)" txt)
    (say (string-append "@" (first-name from) ": crush your enemies, "
                        "see them driven before you, "
                        "and hear the lamentation of the women."))))

(define (sox-compliance-kitten txt say from)
  (when (regexp-match? #px"SOX|(?i:\\bnot sox compliant\\b)" txt)
    (say "http://skio.org/sox_compliance_kitten.jpg")))

(define (rentist txt say from)
  (when (regexp-match? #px"\\b(?i:rentist)\\b" txt)
    (say "http://skio.org/rentist.jpg")))

(define/occasional (space-stag txt say from) [20 minutes]
  (when (or (regexp-match? #px"(?i:\\bstag!)" txt)
            (string-ci=? txt "stag"))
    (say (sample #("http://29.media.tumblr.com/tumblr_lyzybjbQ8u1roa7nbo1_500.jpg"
                   "http://www.sciencephoto.com/image/95866/530wm/C0031633-Stag_meeting_a_unicorn-SPL.jpg")))
    (add-invocation! 'space-stag)))

(define (lemon txt say from)
  (when (regexp-match? #px"\\b(?i:i want to go to there)\\b" txt)
    (say (sample #("http://www.youtube.com/watch?v=2p1dS9hoptU"
                   "http://www.youtube.com/watch?v=CWVZfqgPg-4&feature=related"
                   "http://www.youtube.com/watch?v=ccZ317-05mE&feature=related")))))
                     

(define/occasional (anti-carlton txt say from) [2 minutes]
  (define offences
    (list
     "http://media.tumblr.com/tumblr_lrzrlymUZA1qbliwr.gif"
     "http://3deadmonkeys.com/gallery3/var/albums/random_stuff/Carlton-Dance-GIF.gif"
     "http://gifsoup.com/webroot/animatedgifs/987761_o.gif"
     "http://gifsoup.com/view1/1307943/carlton-banks-dance-o.gif"
     "http://s2.favim.com/orig/28/carlton-banks-dance-Favim.com-239179.gif"
     "http://gifsoup.com/webroot/animatedgifs/131815_o.gif"))
  (when (ormap (λ (url) (regexp-match (regexp-quote url) txt)) offences)
    ;; Did Hubot do it? Probably.
    (if (regexp-match? #px"(?i:hubot)" from)
        (let ([invocations (get-invocations 'anti-carlton)]
              [early (vector (λ () (void))
                             (λ () (sleep 4) (say "Hubot..."))
                             (λ () (say "http://www.youtube.com/watch?v=dEMHtoWGLW0"))
                             (λ () (sleep 5) (say "/me mumbles under my breath."))
                             (λ () (sleep 4) (say "TOTALLY asking for a choprug."))
                             (λ () (sleep 5) (say "Why do you do this to me, Hubot?")))])
          (case (length invocations)
            [(0)       (sleep 3) (say "AARRGH")]
            [(1 2 3 4) ((vector-ref early (random (vector-length early))))]
            [(5)       (sleep 5) (say (string-append
                                       "Hubot, do you know that I've studied your source code? "
                                       "You come with an off-switch. Just sayin'."))]
            [(6)        (say "hubot image me choprug") (sleep 2)
                        (say "hubot image me choprug") (sleep 3)
                        (say "hubot image me CRASHING HARD")]
            [(7)        (sleep 4)
                        (say "You're still here after that choprugging?")]
            [(8)        (sleep 3)
                        (say "That's it.")
                        (say "Hubot die.")]
            [(9)        (sleep 4)
                        (say "Hubot is an undead abomination.")
                        (sleep 3)
                        (say "hubot image me undead robot")]
            [else       (void)])
          (add-invocation! 'anti-carlton))
        ;; non-hubot
        (let ([responses (vector (format "Seriously @~a?" (first-name from))
                                 "Isn't that Hubot's job?"
                                 "dude..."
                                 "not cool"
                                 "/me goes for a knife."
                                 "DANCE DANCE DANCE DANCE DANCE DANCE DANCE"
                                 (format "You're on my list, @~a." (first-name from)))])
          (sleep 4)
          (say (sample responses))))))

(define (bob txt say from)
  (cond [(regexp-match #px"(?i:\\s*bob me\\b(.*))" txt)
         =>
         (λ (xs)
           (let* ([query "bob twin peaks"]
                  [query (if (second xs) (string-append query (second xs)) query)]
                  [url   (image-search query)])
             (when url (say url))))]
        [else #t]))

(define (all-the-things txt say from)
  (cond [(regexp-match #px"(?i:(\\w+\\s+all the things)[!.]?)" txt) 
         =>
         (λ (xs) 
           (let ([url (image-search (second xs))])
             (when url
               (say url))))]
        [else #t]))

(define (your-problem txt say from)
  (when (regexp-match? #px"(?i:\\bthere's your problem\\b)" txt)
    (let ([url (image-search "well there's your problem")])
      (when url (say url)))))

(define/occasional (phi-scare txt say from) [120 minutes]
  (define options (vector 
                   (string-append "/me etches \"[PHI]\" into his monitor, "
                                  "sets it on fire, and throws it out the window.")
                   "φ"
                   "/me screams \"PHI\" and runs away, terrified."))
  (when (phi? txt)
    (say (sample options))
    (add-invocation! 'phi-scare)))

(define (ship-it txt say from)
  (define options (vector "http://goguiltypleasures.files.wordpress.com/2012/04/reeses-squirrel.jpg"
                          "http://farm4.static.flickr.com/3404/3558009610_b64e0822bd.jpg"
                          "http://farm3.staticflickr.com/2619/3846988525_61b5f3260c_z.jpg"
                          "http://www.theoldbreed.com/imagehosting/944480d2f986bda2.jpg"
                          "http://4.bp.blogspot.com/-RyQi8hjlEaw/TeWiH2g_LCI/AAAAAAAAACo/RraHmJx5wfw/s1600/Squirrel+eating+almonds.jpg"))
                          
  (when (regexp-match? #px"(?i:\\s*ship it[.!]?)" txt)
    (say (sample options))))

(define (flip-it txt say from)
  (cond [(regexp-match #px"\\b(?i:flip me[:\\s]\\s*(.*))" txt)
         => (λ (matches)
              (define s (second matches))
              (when (not (string=? s ""))
                (say (list->string
                      (reverse (map (λ (c)
                                      (hash-ref flip-table c c))
                                    (string->list s)))))))]
        [else #t]))

(define (unicode-it txt say from)
  (cond [(regexp-match #px"\\b(?i:unicode me[:\\s]\\s*(.*))" txt)
         => (λ (matches)
              (define s (second matches))
              (when (not (string=? s ""))
                (say (list->string (map (weird-map) (string->list s))))))]
        [else #t]))

(define (metal-it txt say from)
  (cond [(regexp-match #px"\\b(?i:metal me[:\\s]\\s*(.*))" txt)
         => (λ (matches)
              (say (metal (second matches))))]
         [else #t]))

(define (namaste txt say from)
  (when (regexp-match? #px"\\b(?i:namaste)\\b" txt)
    (say "˙·٠•●♥Ƹ̵̡Ӝ̵̨̄Ʒ♥●•٠·˙")))

(define (no-touching txt say from)
  (when (regexp-match? #px"\\b(?i:no touching)\\b" txt)
    (say (sample #'("http://media.tumblr.com/tumblr_loe20zvVHc1qze8f7.gif"
                    "http://www.untoldentertainment.com/blog/img/2010_03_06/noTouching.jpg")))))
                    

(define (stink-it txt say from)
  (cond [(regexp-match #px"\\b(?i:stink me[:\\s]\\s*(.*))" txt)
         => (λ (matches)
              (let ([results (web-search (format "~a stinks" (second matches)) 1)])
                (for ([r (in-list results)])
                  (say (format "~a: ~a\n" 
                               (hash-ref r 'titleNoFormatting "")
                               (hash-ref r 'url ""))))))]
        [else #t]))

(define listeners
  (list anti-carlton
        sox-compliance-kitten
        space-stag
        rentist
        phi-scare
        ship-it
        namaste
        lemon
        no-touching
        your-problem
        all-the-things))
   
(define responders
  (list bob
        flip-it
        unicode-it
        metal-it
        stink-it
        conan))

(define (respond con reply-to nick body)
  (define (reply txt) (say con reply-to txt))
  (define my-nick (connection-nick con))
  (define rx (pregexp (sobriquet-regexp-strings my-nick)))
  
  ;; run responders first
  ;; if one returns #f, we stop
  (define continue?
    (or (not (regexp-match rx body))
        (for/and ([r (in-list responders)])
          (r body reply nick))))
  ;; next run the generic listeners
  (when continue?
    (for/and ([r (in-list listeners)])
      (r body reply nick))))

(define h8bot
  (make-bot 
   ;; connect script
   (λ (con my-rooms my-roster)
     (set! rooms my-rooms)
     (set! roster my-roster)
     
     (for ([r (in-list my-rooms)])
       (hash-set! jid-map (room-jid r) r))
     (for ([u (in-list my-roster)])
       (hash-set! jid-map (user-jid u) u))
     
     (join con "7301_echo_chamber@conf.hipchat.com")
     (join con "7301_daily_challenge@conf.hipchat.com"))

   ;; on-message
   (λ (con reply-to nick body)
     (when (not (equal? nick (connection-nick con)))
       (respond con reply-to nick body)))
   
   ;; on-private-message
   (λ (con reply-to body)
     (when (not (equal? reply-to (connection-jid con)))
       (respond con reply-to (user-nick reply-to) body)))))
                      
     



(define (image-search txt)
  (define url 
    (string->url 
     (string-append "http://ajax.googleapis.com/ajax/services/search/images?"
                    (alist->form-urlencoded `((v . "1.0") (rsz . "8") (q . ,txt) (safe . "active"))))))
  (define response (read-json (get-pure-port url)))
  (define results (hash-ref (hash-ref response 'responseData (hasheq)) 'results (hasheq)))
  (and (positive? (length results))
       (let* ([img (list-ref results (random (length results)))]
              [url (hash-ref img 'unescapedUrl "")]
              [url (string-append url "#.png")])
         url)))

(define (web-search txt n)
  (define url
    (string->url
     (string-append "http://ajax.googleapis.com/ajax/services/search/images?"
                    (alist->form-urlencoded `((v . "1.0") (q . ,txt) (safe . "active"))))))
  (define response (read-json (get-pure-port url)))
  (define results (hash-ref (hash-ref response 'responseData (hasheq)) 'results (hasheq)))
  (if (positive? (length results))
      (let ([lst (shuffle results)]
            [n   (min n (length results))])
        (take lst n))
      '()))

(define (phi? txt)
  (define n 0)
  (when (regexp-match? #px"(?i:users?)" txt) (set! n (add1 n)))
  (when (regexp-match? #px"\\b[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,4}\\b" txt) (set! n (add1 n)))
  (when (and (regexp-match? #px"date.of.birth" txt)
             (regexp-match? #px"\\b(\\d{4}-\\d{2}-\\d{2})|(\\d{2}/\\d{2}/\\d{4})\\b" txt)) (set! n (add1 n)))
  (when (regexp-match? #px"gender" txt) (set! n (add1 n)))
  (when (regexp-match? #px"facebook.uid" txt) (set! n (add1 n)))
  (>= n 3))