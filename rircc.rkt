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
(wtimeout inputwin 50)
(void (waddstr titlewin (string-append "FoxIRC" (make-string (- x 6) #\space))))
(void (wrefresh win))
(define s null)
(define running 1)
(define window "")
(define-values (irccon ready-event) (irc-connect "irc.entropynet.net" 6667 "AllieRacket" "Allie" "Allie Fox"))

        
(define (write_to_curses_format str)
  (let ([maxx (getmaxx displaywin)])
    (let ([chars (string->list str)])
      (let ([bold #f])
        (let ([_write_formatted_char (lambda (ch)
              (match (char->integer ch)
                    [2 (cond
                         [(false? bold)
                          (set! bold #t)
                          (wattron displaywin A_BOLD)]
                         [else
                          (wattrset displaywin A_NORMAL)
                          (set! bold #f)])]
                    [1 #f]
                    [13 #f]
                    [_ (waddstr displaywin (string ch))]))])
        (cond
          [(> (length chars) maxx)
           (map _write_formatted_char (take chars (- maxx 1)))
           (waddstr displaywin "\n")
           (write_to_curses_format (list->string (list-tail chars (- maxx 1))))]
          (else
           (map _write_formatted_char chars)
           (waddstr displaywin "\n")))))))
  (wattrset displaywin A_NORMAL)
  #t)
    
 (define (commandparse s)
  (let ([str (list->string (filter (lambda (s) (char? s)) (reverse s)))])
    (match str
      [(pregexp "^/(.*?) (.*)$" (list _ cmd rest)) (match (string-downcase cmd)
                                                     ["join" (irc-join-channel irccon rest)]
                                                     ["win"  (set! window rest)])]
      [(pregexp "^/(.*?)$" (list _ cmd)) (match (string-downcase cmd)
                                           ["quit" (set! running -1)])]
      [_ (cond
           [(string=? window "") #f]
           [else
            (irc-send-message irccon window str)
            (write_to_curses_format (irc-output (splitmsg (string-append ":AllieRacket!fake@fake PRIVMSG " window " :" str))))])])))   

(void (write_to_curses_format "\x02helo\x02 helo"))
(void (sync ready-event))

(define ircmsgs (irc-connection-incoming irccon))
(let loop ()
  (define l (wgetch inputwin))
 
  (define msg (async-channel-try-get ircmsgs))
;  (display msg)
  (void (match msg
    [#f #f]
    [_ (if (string? (irc-output (reparse msg))) (write_to_curses_format (string-append (irc-output (reparse msg)))) #f)]))
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
