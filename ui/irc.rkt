#lang racket

(provide irc-output)
(provide reparse)
(provide splitmsg)

(require racket/match)
(require irc)
; structures
(struct hostmask (nick ident host server))
; identical to racket-irc's ircmessage struct.
;(struct irc-message (prefix command parameters content))

; arg split routines
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


(define (splitmsg raw) ; message splitter.
  (match (string-split raw " " #:trim? #f #:repeat? #f)
    [(or (list "PING" server) (list "PONG" server)) #f]
    [(list-rest prefix command args) (irc-message
                                      (removesep prefix) command 
                                      (argsplit args) raw)]
    [_ #f]
    )
  )

(define (split-hostmask hs) ; hostmask splitter
  (match hs
    [(pregexp "^:(.*)!(.*)@(.*)" (list _ a b c)) (hostmask a b c #f)]
    [(pregexp "(.*)!(.*)@(.*)" (list _ a b c)) (hostmask a b c #f)]
    [(pregexp "^:(.*)" (list _ s)) (hostmask #f #f #f s)]
    [(pregexp "(.*)" (list _ s)) (hostmask #f #f #f s)]
    [#f hs]
    )
)

(define (isServer? hs) ; check if user or server true on server
  (match hs
    [(hostmask _ _ _ #f) #f]
    [(hostmask #f #f #f s) #t]
    [_ #f]
    )
)

(define (reparse m) ; annoying thing to use my parser instead of racket-irc's 
  (match m
    [(struct irc-message _) (splitmsg (irc-message-content m))]
    [_ m]
    ))


(define (irc-output m) ; create output strings. 
  (match m
    [(struct irc-message _) (match (split-hostmask (irc-message-prefix m))
       [(app isServer? #f) (match (irc-message-command m)
                            ["PRIVMSG" (format "[~a] <~a> ~a" 
                                               (first (irc-message-parameters m)) 
                                               (hostmask-nick (split-hostmask (irc-message-prefix m)))
                                               (last (irc-message-parameters m))
                                    )]
                            ["NOTICE" (format "[NOTICE(~a)] * ~a ~a" 
                                               (first (irc-message-parameters m)) 
                                               (hostmask-nick (split-hostmask (irc-message-prefix m)))
                                               (last (irc-message-parameters m))
                                    )]
                            ["JOIN" (format "* ~a (~a@~a) -> ~a" 
                                            (hostmask-nick (split-hostmask (irc-message-prefix m)) )
                                            (hostmask-ident (split-hostmask (irc-message-prefix m)) )
                                            (hostmask-host (split-hostmask (irc-message-prefix m)))
                                            (first(irc-message-parameters m))
                                    )]
                            ["QUIT" (format "* ~a (~a@~a) Quits (~a)" 
                                            (hostmask-nick (split-hostmask (irc-message-prefix m))) 
                                            (hostmask-ident (split-hostmask (irc-message-prefix m))) 
                                            (hostmask-host (split-hostmask (irc-message-prefix m))) 
                                            (first(irc-message-parameters m))
                                    )]
                            ["NICK" (format "* ~a is now known as ~a" 
                                            (hostmask-nick (split-hostmask (irc-message-prefix m)) )
                                            (first(irc-message-parameters m))
                                            )]
                            ["INVITE"  (format "* ~a (~a@~a) has invited you to ~a"
                                                 (hostmask-nick (split-hostmask (irc-message-prefix m)))
                                                 (hostmask-ident (split-hostmask (irc-message-prefix m)))
                                                 (hostmask-host (split-hostmask (irc-message-prefix m)))
                                               (last(irc-message-parameters m)))]
                            ["MODE" (format "* ~a (~a@~a) set mode(s) ~a on ~a" 
                                            (hostmask-nick 
                                            (split-hostmask (irc-message-prefix m))) 
                                            (hostmask-ident (split-hostmask (irc-message-prefix m))) 
                                            (hostmask-host (split-hostmask (irc-message-prefix m)))
                                            (string-join (list-tail (irc-message-parameters m) 1) " ")
                                            (first(irc-message-parameters m))
                                    )]
                            ["PART" (if (= (length (irc-message-parameters m)) 1)
                                        (format "* ~a (~a@~a) <- ~a" 
                                                (hostmask-nick (split-hostmask (irc-message-prefix m))) 
                                                (hostmask-ident (split-hostmask (irc-message-prefix m)))
                                                (hostmask-host (split-hostmask (irc-message-prefix m))) 
                                                (first(irc-message-parameters m)))
                                        (format "* ~a (~a@~a) <- ~a (~a)" 
                                                (hostmask-nick (split-hostmask (irc-message-prefix m))) 
                                                (hostmask-ident (split-hostmask (irc-message-prefix m))) 
                                                (hostmask-host (split-hostmask (irc-message-prefix m))) 
                                                (first(irc-message-parameters m)) 
                                                (last (irc-message-parameters m) ))
                                    )]
                    [_ (irc-message-content m)])]
       [(? isServer?)
        (match (irc-message-command m)
          ["NOTICE" (format "[SERVER(~a)] * ~a: ~a"
                            (first(irc-message-parameters m))
                            (hostmask-server (split-hostmask (irc-message-prefix m)))
                            (last(irc-message-parameters m)))]
          ["MODE" (format "-*- you are now mode(s) ~a" 
                          (last(irc-message-parameters m)))]
          [(or (regexp "00[123]") (regexp "37[256]") ) (format "-*- ~a" (last (irc-message-parameters m)))] 
          [(or (regexp "00[45]") (regexp "25[01245]")) (format "-*- ~a" (string-join (list-tail (irc-message-parameters m) 1) " "))]
          [(regexp "26[56]") (format "-*- ~a" 
                                     (last(list-tail (irc-message-parameters m) 3)))]
          [_ (irc-message-content m)])
        ]
       [_ (irc-message-content m)] 
    )]
    [_ (format "~a" m)]
))
  
(define (display. str)
  (match str
    [(? string?) (displayln (string-replace (string-replace str "\r" "") "\n" ""))]
    [_ #f]
    ))

