(library (zxcvbn scoring)
  (export most-guessable-match-sequence)

  (import (chezscheme))

  (include "adjacency-graphs.scm")

  (define (nCk n k)
    (define (loop r n d)
      (if (> d k)
          r
          (loop (/ (* r n) d)
                (sub1 n)
                (add1 d))))
    (if (> k n) 0 (loop 1 n 1)))


  )

