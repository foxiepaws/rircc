#lang racket
(provide (all-defined-out))
(require ffi/unsafe)
(require ffi/unsafe/define)

; FFI for curses.


(define-ffi-definer define-curses (ffi-lib "libcurses"))

(define _WINDOW-pointer (_cpointer 'WINDOW))


(define-curses initscr (_fun -> _WINDOW-pointer))
(define-curses subwin  (_fun _WINDOW-pointer _int _int _int _int -> _WINDOW-pointer))
(define-curses waddstr (_fun _WINDOW-pointer _string -> _int))
(define-curses waddch  (_fun _WINDOW-pointer _string -> _int))
(define-curses wrefresh (_fun _WINDOW-pointer -> _int))
(define-curses doupdate (_fun -> _void))
(define-curses wnoutrefresh (_fun _WINDOW-pointer -> _int))
(define-curses wcursyncup (_fun _WINDOW-pointer -> _void))
(define-curses endwin (_fun -> _int))
(define-curses getmaxx (_fun _WINDOW-pointer -> _int))
(define-curses getmaxy(_fun _WINDOW-pointer -> _int))
(define (getmaxyx win)
    (list (getmaxy win) (getmaxx win)))
(define-curses syncok (_fun _WINDOW-pointer _bool -> _int))
(define-curses getch (_fun -> _int))
(define-curses wgetch (_fun _WINDOW-pointer -> _int))
(define-curses raw (_fun -> _int))
(define-curses noraw (_fun -> _int))
(define-curses echo(_fun -> _int))
(define-curses noecho(_fun -> _int))
(define-curses cbreak(_fun -> _int))
(define-curses nocbreak(_fun -> _int))
(define-curses leaveok(_fun _WINDOW-pointer _bool -> _int))
(define-curses scrollok(_fun _WINDOW-pointer _bool -> _int))
;int halfdelay(int tenths);
;int intrflush(WINDOW *win, bool bf);
(define-curses keypad (_fun _WINDOW-pointer _bool -> _int))
;int meta(WINDOW *win, bool bf);
;int nodelay(WINDOW *win, bool bf);
;void noqiflush(void);
;void qiflush(void);
;int notimeout(WINDOW *win, bool bf);
(define-curses timeout (_fun _int -> _void))
(define-curses wtimeout (_fun _WINDOW-pointer _int -> _void))
;int typeahead(int fd);
(define-curses wmove (_fun _WINDOW-pointer _int _int -> _int))
(define-curses werase (_fun _WINDOW-pointer -> _int))

(define-curses has_colors (_fun -> _bool))
(define-curses start_color (_fun -> _int))
(define-curses init_pair (_fun _short _short _short -> _int))
(define-curses init_color (_fun _short _short _short _short -> _int))
(define-curses wattron (_fun _WINDOW-pointer _int -> _int))
(define-curses wattroff (_fun _WINDOW-pointer _int -> _int))
(define-curses wattrset (_fun _WINDOW-pointer _int -> _int))
(define-curses mvcur (_fun _int _int _int _int -> _int))
;(define-curses setsyx (_fun _int _int -> _int))

(define COLOR_BLACK     0)
(define COLOR_RED       1)
(define COLOR_GREEN     2)
(define COLOR_YELLOW    3)
(define COLOR_BLUE      4)
(define COLOR_MAGENTA   5)
(define COLOR_CYAN      6)
(define COLOR_WHITE     7)

(define A_NORMAL 0)
(define A_BOLD 2097152)
(define (COLOR_PAIR n) (arithmetic-shift n 8))





