(library (zxcvbn scoring)
  (export most-guessable-match-sequence)

  (import (chezscheme))

  (include "adjacency-graphs.scm")
  (define bruteforce-cardinality 10)
  (define min-guesses-before-growing-sequence 10000)
  (define min-submatch-guesses-single-char 10)
  (define min-submatch-guesses-multi-char 50)
  (define reference-year (date-year (current-date)))

  ;; '(("qwerty" '(("`" (() () () "1!" () ()))...) "keypad" '((...))))
  (define (calc-average-degree data name)
    (let* ([graph-list
            (cadr (assoc name data))]
           [neighbor-count
            (map (lambda (key-neighbors)
                   (length (filter
                            (lambda (neighbors)
                              (not (null? neighbors)))
                            (cadr key-neighbors))))
                 graph-list)])
      (/ (apply + neighbor-count) (length graph-list))))
  ;; (calc-average-degree adjacency-graphs "keypad")  
             
  (define (nCk n k)
    (define (loop r n d)
      (if (> d k)
          r
          (loop (/ (* r n) d)
                (sub1 n)
                (add1 d))))
    (if (> k n) 0 (loop 1 n 1)))

  (define (log10 x)
    (/ (log x) (log 10)))

  (define (log2 x)
    (/ (log x) (log 2)))

  (define (factorial n)
    (let loop ((i 1)
               (out 1))
      (if (> i n)
          out
          (loop (add1 i) (* out i)))))
        


  )

