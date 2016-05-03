#lang racket
(require "ui/curses.rkt")
(require "ui/irc-to.rkt")
(require "ui/curses/window.rkt")
(require "irc/irc.rkt")
(require racket/match)
(require racket/async-channel)





(define to-handlers
  (make-hash (list
   (cons 'default (new irc-text-object%
                       [fmt "~a"]))
   (cons 'MOTD (new irc-text-object%
                    [fmt "-!- ~a"]
                    [mat (lambda (p v)
                           (regexp-match? #rx#"37[256]" v))]
                    [app
                     (lambda (msg)
                       (list (last (send msg args))))]))
   (cons 'HIDDENHOST (new irc-text-object%
                          [fmt "-!- ~a is now your hidden host"]
                          [mat (lambda (p v)
                                 (regexp-match? #rx#"396" v))]
                          [app (lambda (msg)
                                 (list (list-ref (send msg args) 1)))]))
   (cons 'PRIVMSGNOTICE (new irc-text-object%
                      [mat (lambda (p v)
                             (or (string=? v "PRIVMSG") (string=? v "NOTICE")))]
                      [fmt "[~a] <~a> ~a"]
                      [app (lambda (msg)
                             (let ([hs (new hostmask%
                                         [prefix (send msg prefix)])])
                             (list 
                              (first (send msg args))
                              (cond
                                [(send hs isserver?) (send hs host)]
                                [else (send hs nick)])
                              (last (send msg args)))))]))
   
   (cons 'JOIN (new irc-text-object%
                 [mat (lambda (p v)
                        (string=? v "JOIN"))]
                        [fmt "* ~a (~a@~a) -> ~a"]
                        [app (lambda (msg)
                               (let ([hs (new hostmask% [prefix (send msg prefix)])])
                                 (list
                                  (send hs nick)
                                  (send hs ident)
                                  (send hs host)
                                  (first (send msg args)))))])))))


(define (irc-output msg)
  (last (sort (filter string? (for/list ([kv (hash->list to-handlers)])
                                       (send (cdr kv) parse msg)))
                     (lambda (x y)
                       (cond
                         [(and (string=? (substring x 0 1) ":") (false? (string=? (substring y 0 1) ":"))) #t]
                         [(and (string=? (substring y 0 1) ":") (false? (string=? (substring x 0 1) ":"))) #f])))))
                          

;; not using mat.
;(define (irc-ouput msg)
;  (send (hash-ref to-handlers (string->symbol (send msg verb)) (hash-ref to-handlers 'default)) parse msg))
    


(define win
  (new window-class%
       [input-raw #t]
       [input-echo #f]
       [color #t]))
(define stderr (open-output-file "/dev/stderr" #:exists 'append))
;(keypad win #t)
(init_pair 1 COLOR_BLACK COLOR_RED)
(define x (send win maxx))
(define y (send win maxy))
(define titlewin
  (new window-class%
       [parent win]
       [height 1]
       [width x]
       [y 0]
       [x 0]
       [syncup #t]))
(send titlewin attron (COLOR_PAIR 1))
(define displaywin
  (new window-class%
       [parent win]
       [height (- y 2)]
       [width x]
       [y 1]
       [x 0]
       [syncup #t]
       [scroll #t]))
(define inputwin
  (new window-class%
       [parent win]
       [height 1]
       [width x]
       [y (- y 1)]
       [x 0]
       [syncup #t]
       [input-timeout 50]))
(void (send titlewin addstr (string-append "rircc" (make-string (- x 6) #\space))))
(send win refresh)
(define s null)
(define running 1)
(define window "")

(define en (new irc%
                [host "irc.entropynet.net"]
                [port 6697]
                [ssl #t]
                [nick "AllieRacket"]
                [user "Allie"]
                [realname "Allie Fox"]))

        
(define (write_to_curses_format str)
  (let ([maxx (send displaywin maxx)])
    (let ([chars (string->list str)])
      (let ([bold #f])
        (let ([_write_formatted_char (lambda (ch)
              (match (char->integer ch)
                    [2 (cond
                         [(false? bold)
                          (set! bold #t)
                          (send displaywin attron A_BOLD)]
                         [else
                          (send displaywin attroff A_BOLD)
                          (set! bold #f)])]
                    [1 #f]
                    [13 #f]
                    [_ (send displaywin addstr (string ch))]))])
        (cond
          [(> (length chars) maxx)
           (map _write_formatted_char (take chars (- maxx 1)))
           (send displaywin addstr "\n")
           (write_to_curses_format (list->string (list-tail chars (- maxx 1))))]
          (else
           (map _write_formatted_char chars)
           (send displaywin addstr "\n")))))))
  (send displaywin attrset A_NORMAL)
  #t)
 (define (commandparse s)
  (let ([str (list->string (filter (lambda (s) (char? s)) (reverse s)))])
    (match str
      [(pregexp "^/(.*?) (.*)$" (list _ cmd rest)) (match (string-downcase cmd)
                                                     ["join" (send en join rest)]
                                                     ["win"  (set! window rest)])]
      [(pregexp "^/(.*?)$" (list _ cmd)) (match (string-downcase cmd)
                                           ["quit" (set! running -1)])]
      [_ (cond
           [(string=? window "") #f]
           [else
            (send en msg window str)
            (write_to_curses_format (irc-output (new irc-message%
                                                     [msg (string-append ":AllieRacket!fake@fake PRIVMSG " window " :" str)])))])])))   

(void (write_to_curses_format "\x02helo\x02 helo"))
(void (sync (send en ready?)))

(define ircmsgs (send en hosepipe!))
(let loop ()
  (define l (send inputwin getch))
  (define raw  (async-channel-try-get ircmsgs))
  (define msg (match raw
                [#f #f]
                [_ raw]))
  ;  (display msg
  (void (match msg
    [#f #f]
    [_ (if (string? (irc-output msg)) (write_to_curses_format (format "~a" (irc-output msg))) #f)]))
    ;[_ (waddstr displaywin (format "bleh ~a\n" msg))])
 ; (if (> l -1)
  ;    (waddstr displaywin (format "~a\n" l))
   ;   #f)
  (match l
    [-1 #f]
    [(or 263 127) (cond
                    [(pair? s) (set! s (cdr s))])]
    [10 (commandparse s)(set! s null)]
    [_ (set! s (cons (integer->char l) s))])
  (send inputwin erase)
  (send inputwin move (- y 1) 0)
  (send inputwin addstr (list->string (filter (lambda (s) (char? s)) (reverse s))))
  ; refresh our displays, with input last.
  (send win refresh)
  (if (> running 0)
        (loop)
        #f))

(void (send win end))
