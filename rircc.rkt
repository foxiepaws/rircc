#lang racket
(require "ui/curses.rkt")
(require "ui/irc.rkt")
(require irc)
(require racket/match)
(require racket/async-channel)






(define win (initscr))
(define stderr (open-output-file "/dev/stderr" #:exists 'append))
(raw)
(noecho)
(timeout 5)
(keypad win #t)
(start_color)
(init_pair 1 COLOR_BLACK COLOR_RED)
(define x (getmaxx win))
(define y (getmaxy win))
(define titlewin (subwin win 1 x 0 0))
(define displaywin (subwin win (- y 2) x 1 0))
(define inputwin (subwin win 1 x (- y 1) 0))
(syncok titlewin #t)
(wattron titlewin (COLOR_PAIR 1))
(syncok displaywin #t)
(scrollok displaywin #t)
(syncok inputwin #t)
(wcursyncup inputwin)
(wtimeout inputwin 50)
(void (waddstr titlewin (string-append "FoxIRC" (make-string (- x 6) #\space))))
(void (wrefresh win))
(define s null)
(define running 1)
(define-values (irccon ready-event) (irc-connect "irc.entropynet.net" 6667 "AllieRacket" "Allie" "Allie Fox"))
(define (commandparse s)
  (let ([str (list->string (filter (lambda (s) (char? s)) (reverse s)))])
    (match str
      [(pregexp "^/(.*?) (.*)$" (list _ cmd rest)) (match (string-downcase cmd)
                                                                           ["join" (irc-join-channel irccon rest)])]
      [(pregexp "^/(.*?)$" (list _ cmd)) (match (string-downcase cmd)
                                           ["quit" (set! running -1)])]
      [_ #f])))
(void (sync ready-event))

(define ircmsgs (irc-connection-incoming irccon))
(let loop ()
  (define l (wgetch inputwin))
 
  (define msg (async-channel-try-get ircmsgs))
;  (display msg)
  (match msg
    [#f #f]
    [_ (if (not (string=? "" (irc-output msg))) (waddstr displaywin (string-append (irc-output (reparse msg)))) #f)])
    ;[_ (waddstr displaywin (format "bleh ~a\n" msg))])
 ; (if (> l -1)
  ;    (waddstr displaywin (format "~a\n" l))
   ;   #f)
  (match l
    [-1 #f]
    [(or 263 127) (set! s (cdr s))]
    [10 (commandparse s)(set! s null)]
    [_ (set! s (cons (integer->char l) s))])
  (werase inputwin)
  (wmove inputwin (- y 1) 0)
  (waddstr inputwin (list->string (filter (lambda (s) (char? s)) (reverse s))))
  ; refresh our displays, with input last.
  (wrefresh win)
  (if (> running 0)
        (loop)
        #f))

(void (endwin))
