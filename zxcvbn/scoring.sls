(library (zxcvbn scoring)
  (export most-guessable-match-sequence)

  (import (chezscheme))

  (include "adjacency-graphs.scm")

  (define (nCk n k)
    (unless (and (fixnum? n) (fixnum? k))
      (assertion-violation "nCk" "n and k must both be integers"))
    (let loop ([r 1]
               [n n]
               [d 1])
      (cond [(> k n) 0]
            [(> d k) r]
            [else
             (loop (/ (* r n) d) (sub1 n) (add1 d))])))
            


  )

