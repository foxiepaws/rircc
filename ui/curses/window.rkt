#lang racket
(provide (all-defined-out))
(require "../curses.rkt")
(require (only-in ffi/unsafe cpointer?)) 
(define window-class%
  (class object%
    (init-field
     (parent #f)
     (width #f)
     (height #f)
     (y #f)
     (x #f)
     (input-raw #f)
     (input-timeout -1)
     (input-echo #t)
     (color #f)
     (syncup #f)
     (scroll #f)
     )
    (field (win #f))
    (define/public (maxx)
      (getmaxx win))
    (define/public (maxy)
      (getmaxy win))
    (define/public (end)
      (cond
        ((false? parent)(endwin))))
    (define/public (refresh)
      (void (wrefresh win)))
    (define/public (getch)
      (wgetch win))
    (define/public (getwin)
      win)
    (define/public (addstr str)
      (waddstr win str))
    (define/public (erase)
      (werase win))
    (define/public (move y x)
      (wmove win y x))
    (define/public (attron attr)
      (wattron win attr))
    (define/public (attroff attr)
      (wattroff win attr))
    (define/public (attrset attr)
      (wattrset win attr))
    
    (super-new)
    (cond
      [(false? parent) (set! win (initscr))]
      [(is-a? parent this%)
       (set! win (subwin (send parent getwin) height width y x))])
    (cond
      ((false? parent)
       (cond
         (input-raw (raw)))
       (cond
         ((false? input-echo) (noecho)))
       (cond
         (color (start_color)))
       ))
    (cond
      ((positive? input-timeout) (wtimeout win input-timeout)))
    (cond 
      (syncup (syncok win #t)))
     ))




