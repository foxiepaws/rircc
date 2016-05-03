#lang racket
(require racket/tcp
         racket/async-channel
         openssl)
(provide (all-defined-out))

(define line-protocol%
  (class object%
    (init-field
     host
     port
     (ssl #f)
     (connected #f)
     (in-ch (make-async-channel))
     (handler #f))
    (define/public (connected?)
      connected)
    (define/public (hosepipe!)
      in-ch)
    (define/public (raw cmd params)
      (fprintf out "~a ~a\r\n" cmd (string-join params)))
    (super-new)
    (define-values (in out)
      (match ssl
        [#f (tcp-connect host port)]
        [#t (ssl-connect host port)]
        [_  (ssl-connect host port ssl)]))
    (file-stream-buffer-mode out 'line)
    (define conthread
      (thread
       (lambda ()
         (let run ()
           (define ev (sync in))
           (cond
             [(eof-object? (peek-char in)) (set! connected #f)]
             [else           
              (define line
                (cond
                  [(port-closed? in)
                   (set! connected #f)
                   eof]
                  [else
                   (set! connected #t)
                   (read-line in)
                   ]))
              (cond
                [(eof-object? line) #f]
                [else
                 (when (procedure? handler)
                   (handler out in-ch line))])])
           (when connected
             (run))))))))

(define irc%
  (class line-protocol%
    (inherit-field host port connected handler)
    (inherit raw)
    (init-field
     nick
     user
     (realname "irc%")
     (handlers (make-hash))
     (ready-sema (make-semaphore)))
    (super-new)

    (hash-set! handlers "ready" (lambda (msg out in-ch key)
                                  ;(displayln "in ready handler")
                                  (cond
                                    [(string=? (send msg verb) "001") (semaphore-post ready-sema)(hash-remove! handlers key)])))
    
    (set! handler (lambda (out in-ch line)
                    ;(displayln line)
                    (define msg (new irc-message% [msg line]))
                    (async-channel-put in-ch line)
                    (when (is-a? msg irc-message%)
                      (for ([kv (hash->list handlers)])
                        ((cdr kv) msg out in-ch (car kv))))
                    ))

    (define/public (msg target msg)
      (raw "PRIVMSG" (list target (string-append ":" msg))))
    (define/public (join channel)
;      (displayln (string-append "Joining " channel))
      (raw "JOIN" (list channel)))
    (define/public (part channel [msg "irc%"])
        (raw "PART" (list channel (string-append ":" msg))))
    (define/public (notice target msg)
      (raw "NOTICE" (list target (string-append ":" msg))))
    (define/public (ready?)
      ready-sema)
    (define/public (set-nick n)
      (raw "NICK" (list n)))
    (define/private (set-user-info u rn)
      (raw "USER" (list u "0" "*" (string-append ":" rn))))
    (define/public (quit [msg "irc%"])
      (raw "QUIT" (list (string-append ":" msg))))
    
    (set-nick nick)
    (set-user-info user realname)
    ))
(define irc-message%
  (class object%
    (init-field
     msg)
    (define/public (raw)
      msg)
    (define/public (verb)
      vrb)
    (define/public (prefix)
      pfix)
    (define/public (args)
      ars)
    (field [vrb ""])
    (field [pfix ""])
    (field [ars ""])
    (define (_arg-tail lst) ; group the rest of the args into a single string after :
      (memf (lambda (x) (regexp-match? (regexp "^:") x)) lst))
    (define (_arg-head lst) ; the args before :
      (take lst (- (length lst) (length (if (_arg-tail lst) (_arg-tail lst) (list ))))))
    (define (argsplit args) ; create parameter list 
      (append 
       (_arg-head args) 
       (if (_arg-tail args) 
           (list (removesep (string-join (_arg-tail args) " "))) 
           (list))))
    (define (removesep str) ; only substring if it starts with a :
      (match str
        [(pregexp "^:") (substring str 1)] 
        [_ str]))
    (super-new)
    (match (string-split msg " " #:trim? #f)
      [(list-rest p c a)
       (set! pfix (removesep p))
       (set! vrb c)
       (set! ars (argsplit a))])))
                   
  
