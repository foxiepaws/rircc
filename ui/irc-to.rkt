#lang racket
(provide (all-defined-out))
(require (only-in "../irc/irc.rkt" irc-message%))


(define irc-text-object%
  (class object%
    (init-field
     (fmt "~a")
     (msg-part 'raw)
     (app #f)
     (mat #f)
     )
    (define/private (removesep str)
      (match str
        [(pregexp "^:") (substring str 1)]
        [_ str]))
    (define/private (do-parse msg)
      (cond
        [(not (procedure? app))
         (format fmt (dynamic-send msg msg-part))]
        [else
         (keyword-apply format null null fmt (app msg))]))

    (define/public (parse msg)
;      (cond
;        [(not (is-a? msg irc-message%))
;         (raise-argument-error 'parse "irc-message%" msg)])
      ; we don't have a procedure to apply first.
      (cond
        [(procedure? mat)
         (when (mat (send msg prefix) (send msg verb))
           (do-parse msg))]
        [else
         (do-parse msg)]))
         
      
      
    (super-new)
    ))



                
                 

  
