#lang racket/base

(require racket/list
         racket/match
         xml)
(require (for-syntax racket/base))

(provide (all-defined-out))

;; structs
(struct connection (host in out to lang jid nick pass handlers) #:mutable)
(struct iq (type data) #:transparent)
(struct handle (connect message private-message))

(define iq-table (make-hash))


;; JID utilities
(define (jid-bare jid)
  (cond [(regexp-match #rx"^(.*?)/" jid) => second]
        [else jid]))

(define (jid-domain jid)
  (cond [(regexp-match #rx"@([^/]+)" jid) => second]
        [else jid]))

(define (jid-resource jid)
  (second (regexp-match #rx"^.*?/(.*)$" jid)))


;; XML utilities
(define-match-expander X
  (λ (stx)
    (syntax-case stx ()
      [(_ name attrs content) #'(element _ _ name attrs content)])))

(define (attr-ref xs key)
  (match xs
    [(cons (attribute _ _ k v) xs) (if (eq? key k) v (attr-ref xs key))]
    [_ #f]))

(define next-id
  (let ([i 0])
    (λ ()
      (begin0 (number->string i)
              (set! i (add1 i))))))

(define (children node tag)
  (match node
    [(X _ _ xs)
     (filter (λ (x)
               (match x
                 [(X (== tag eq?) _ _) x]
                 [_ #f]))
             xs)]
    [_ #f]))

(define (child node tag)
  (match node
    [(X _ _ xs) (for/first ([x (in-list xs)]
                            #:when (eq? (element-name x) tag))
                  x)]
    [_ #f]))

(define (text node)
  (match node
    [(X _ _ xs)
     (define out (open-output-string))
     (for ([x (in-list xs)])
       (write-xml/content x out))
     (get-output-string out)]
    [(pcdata _ _ txt) txt]
    [(cdata _ _ txt) txt]
    [_ #f]))

;; Null handler
(define null-handler
  (λ _ (void)))