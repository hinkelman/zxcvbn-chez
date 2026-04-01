(library (zxcvbn scoring)
  (export most-guessable-match-sequence)

  (import (chezscheme)
          (wak irregex))

  (include "adjacency-graphs.scm")
  (define bruteforce-cardinality 10)
  (define min-guesses-before-growing-sequence 10000)
  (define min-submatch-guesses-single-char 10)
  (define min-submatch-guesses-multi-char 50)
  (define min-year-space 20)
  (define reference-year (date-year (current-date)))

  ;; keyboard/keypad graph statistics
  (define (calc-average-degree data name)
    (let* ([graph-list (cadr (assoc name data))]
           [neighbor-count
            (map (lambda (key-neighbors)
                   (length (filter (lambda (neighbors) (not (null? neighbors)))
                                   (cadr key-neighbors))))
                 graph-list)])
      (/ (apply + neighbor-count) (length graph-list))))

  (define keyboard-average-degree (calc-average-degree adjacency-graphs "qwerty"))
  (define keypad-average-degree   (calc-average-degree adjacency-graphs "keypad"))
  (define keyboard-starting-positions (length (cadr (assoc "qwerty" adjacency-graphs))))
  (define keypad-starting-positions   (length (cadr (assoc "keypad" adjacency-graphs))))

  ;; math helpers
  (define (nCk n k)
    (define (loop r n d)
      (if (> d k)
          r
          (loop (/ (* r n) d) (sub1 n) (add1 d))))
    (if (> k n) 0 (loop 1 n 1)))

  (define (log10 x) (/ (log x) (log 10)))
  (define (log2 x)  (/ (log x) (log 2)))

  (define (factorial n)
    (let loop ([i 1] [out 1])
      (if (> i n)
          out
          (loop (add1 i) (* out i)))))

  ;; capitalization variation patterns
  (define start-upper-rx (irregex "^[A-Z][^A-Z]+$"))
  (define end-upper-rx   (irregex "^[^A-Z]+[A-Z]$"))
  (define all-upper-rx   (irregex "^[A-Z]+$"))
  (define all-lower-rx   (irregex "^[a-z]+$"))

  (define (uppercase-variations match)
    (let ([word (cdr (assoc "token" match))])
      (cond
        [(or (irregex-search all-lower-rx word)
             (string=? (string-downcase word) word))
         1]
        [(or (irregex-search start-upper-rx word)
             (irregex-search end-upper-rx word)
             (irregex-search all-upper-rx word))
         2]
        [else
         (let* ([chars (string->list word)]
                [uppers (length (filter char-upper-case? chars))]
                [lowers (length (filter char-lower-case? chars))]
                [variations 0])
           (let loop ([i 1] [variations 0])
             (if (> i (min uppers lowers))
                 variations
                 (loop (add1 i) (+ variations (nCk (+ uppers lowers) i))))))])))

  (define (l33t-variations match)
    (if (not (cdr (assoc "l33t" match)))
        1
        (let ([sub (cdr (assoc "sub" match))]
              [token (string-downcase (cdr (assoc "token" match)))])
          (let loop ([sub sub] [variations 1])
            (if (null? sub)
                variations
                (let* ([pair (car sub)]
                       [subbed-char (car pair)]
                       [unsubbed-char (cdr pair)]
                       [token-chars (string->list token)]
                       [S (length (filter (lambda (c) (char=? c subbed-char)) token-chars))]
                       [U (length (filter (lambda (c) (char=? c unsubbed-char)) token-chars))]
                       [new-variations
                        (if (or (= S 0) (= U 0))
                            (* variations 2)
                            (let inner ([i 1] [possibilities 0])
                              (if (> i (min S U))
                                  (* variations possibilities)
                                  (inner (add1 i) (+ possibilities (nCk (+ S U) i))))))])
                  (loop (cdr sub) new-variations)))))))

  ;; guess estimation functions ---------------------------------------------------

  (define (estimate-guesses match password)
    (let ([min-guesses
           (if (< (string-length (cdr (assoc "token" match)))
                  (string-length password))
               (if (= (string-length (cdr (assoc "token" match))) 1)
                   min-submatch-guesses-single-char
                   min-submatch-guesses-multi-char)
               1)])
      (let ([guesses
             (let ([pattern (cdr (assoc "pattern" match))])
               (cond
                 [(string=? pattern "bruteforce") (bruteforce-guesses match)]
                 [(string=? pattern "dictionary") (dictionary-guesses match)]
                 [(string=? pattern "spatial")    (spatial-guesses match)]
                 [(string=? pattern "repeat")     (repeat-guesses match)]
                 [(string=? pattern "sequence")   (sequence-guesses match)]
                 [(string=? pattern "regex")      (regex-guesses match)]
                 [(string=? pattern "date")       (date-guesses match)]
                 [else (error "estimate-guesses" "unknown pattern" pattern)]))])
        (max guesses min-guesses))))

  (define (bruteforce-guesses match)
    (let* ([token-len (string-length (cdr (assoc "token" match)))]
           [guesses (expt bruteforce-cardinality token-len)]
           [min-guesses (if (= token-len 1)
                            (add1 min-submatch-guesses-single-char)
                            (add1 min-submatch-guesses-multi-char))])
      (max guesses min-guesses)))

  (define (dictionary-guesses match)
    (let ([base-guesses (* (cdr (assoc "rank" match))
                           (uppercase-variations match)
                           (l33t-variations match)
                           (if (cdr (assoc "reversed" match)) 2 1))])
      base-guesses))

  (define (spatial-guesses match)
    (let* ([graph-name (cdr (assoc "graph" match))]
           [s (if (member graph-name '("qwerty" "dvorak"))
                  keyboard-starting-positions
                  keypad-starting-positions)]
           [d (if (member graph-name '("qwerty" "dvorak"))
                  keyboard-average-degree
                  keypad-average-degree)]
           [L (string-length (cdr (assoc "token" match)))]
           [t (cdr (assoc "turns" match))]
           [guesses
            (let loop ([i 2] [g 0])
              (if (> i L)
                  g
                  (let inner ([j 1] [g g])
                    (if (> j (min t (sub1 i)))
                        (loop (add1 i) g)
                        (inner (add1 j)
                               (+ g (* (nCk (sub1 i) (sub1 j)) s (expt d j))))))))]
           [shifted-count (cdr (assoc "shifted-count" match))]
           [guesses (if (> shifted-count 0)
                        (let* ([S shifted-count]
                               [U (- L S)])
                          (if (or (= S 0) (= U 0))
                              (* guesses 2)
                              (let inner ([i 1] [variations 0])
                                (if (> i (min S U))
                                    (* guesses variations)
                                    (inner (add1 i) (+ variations (nCk (+ S U) i)))))))
                        guesses)])
      guesses))

  (define (repeat-guesses match)
    (* (cdr (assoc "base-guesses" match))
       (cdr (assoc "repeat-count" match))))

  (define (sequence-guesses match)
    (let* ([token (cdr (assoc "token" match))]
           [first-chr (string-ref token 0)]
           [base-guesses
            (cond
              [(member first-chr (string->list "aAzZ019")) 4]
              [(char-numeric? first-chr) 10]
              [else 26])]
           [base-guesses (if (not (cdr (assoc "ascending" match)))
                             (* base-guesses 2)
                             base-guesses)])
      (* base-guesses (string-length token))))

  (define (regex-guesses match)
    (let ([regex-name (cdr (assoc "regex-name" match))]
          [token (cdr (assoc "token" match))])
      (let ([char-class-bases
             '(("alpha_lower" . 26) ("alpha_upper" . 26) ("alpha" . 52)
               ("alphanumeric" . 62) ("digits" . 10) ("symbols" . 33))])
        (let ([base (assoc regex-name char-class-bases)])
          (if base
              (expt (cdr base) (string-length token))
              ;; recent-year: regex-match stores the irregex match object
              (let ([year-space (max (abs (- (string->number
                                              (irregex-match-substring
                                               (cdr (assoc "regex-match" match))))
                                            reference-year))
                                     min-year-space)])
                year-space))))))

  (define (date-guesses match)
    (let* ([year-space (max (abs (- (cdr (assoc "year" match)) reference-year))
                            min-year-space)]
           [guesses (* year-space 365)]
           [separator (cdr (assoc "separator" match))]
           [guesses (if (and (string? separator) (> (string-length separator) 0))
                        (* guesses 4)
                        guesses)])
      guesses))

  ;; most-guessable-match-sequence ------------------------------------------------
  ;; Dynamic programming: find the lowest-guesses sequence of non-overlapping matches
  ;; covering the entire password.

  (define (make-bruteforce-match password i j)
    (list (cons "pattern" "bruteforce")
          (cons "token" (substring password i (add1 j)))
          (cons "i" i)
          (cons "j" j)))

  (define (most-guessable-match-sequence password matches)
    (let* ([n (string-length password)]
           ;; partition matches by ending index j
           [matches-by-j (let ([v (make-vector n '())])
                           (for-each (lambda (m)
                                       (let ([j (cdr (assoc "j" m))])
                                         (vector-set! v j (cons m (vector-ref v j)))))
                                     matches)
                           ;; sort each bucket by i
                           (let loop ([k 0])
                             (when (< k n)
                               (vector-set! v k
                                            (sort (lambda (a b)
                                                    (< (cdr (assoc "i" a))
                                                       (cdr (assoc "i" b))))
                                                  (vector-ref v k)))
                               (loop (add1 k))))
                           v)]
           ;; optimal.m[k][l]  = best match ending at k for length-l sequence
           ;; optimal.pi[k][l] = product of guesses for that sequence
           ;; optimal.g[k][l]  = overall score (factorial(l) * pi + additive)
           [opt-m  (make-vector n '())]   ; each entry is an alist keyed by l
           [opt-pi (make-vector n '())]
           [opt-g  (make-vector n '())])

      (define (update m l)
        (let* ([k (cdr (assoc "j" m))]
               [pi (estimate-guesses m password)]
               [pi (if (> l 1)
                       (let ([prev-pi (assoc (sub1 l) (vector-ref opt-pi (sub1 (cdr (assoc "i" m)))))])
                         (if prev-pi (* pi (cdr prev-pi)) pi))
                       pi)]
               [g (+ (* (factorial l) pi)
                     (expt min-guesses-before-growing-sequence (sub1 l)))])
          ;; only update if this beats all competing sequences of length <= l
          (let ([ok (let loop ([competing (vector-ref opt-g k)])
                      (if (null? competing)
                          #t
                          (let ([comp-l (caar competing)]
                                [comp-g (cdar competing)])
                            (if (and (<= comp-l l) (<= comp-g g))
                                #f
                                (loop (cdr competing))))))])
            (when ok
              (vector-set! opt-g  k (cons (cons l g)  (vector-ref opt-g  k)))
              (vector-set! opt-m  k (cons (cons l m)  (vector-ref opt-m  k)))
              (vector-set! opt-pi k (cons (cons l pi) (vector-ref opt-pi k)))))))

      (define (bruteforce-update k)
        ;; single bruteforce match spanning [0..k]
        (update (make-bruteforce-match password 0 k) 1)
        ;; bruteforce matches spanning [i..k] appended to length-l sequences ending at i-1
        (let loop ([i 1])
          (when (<= i k)
            (let ([bf-match (make-bruteforce-match password i k)])
              (for-each (lambda (lm-pair)
                          (let ([l (car lm-pair)]
                                [last-m (cdr lm-pair)])
                            ;; never two adjacent bruteforce matches
                            (unless (string=? (cdr (assoc "pattern" last-m)) "bruteforce")
                              (update bf-match (add1 l)))))
                        (vector-ref opt-m (sub1 i))))
            (loop (add1 i)))))

      (define (unwind)
        (let* ([k (sub1 n)]
               ;; find best length and score at position k
               [best-l #f]
               [best-g +inf.0])
          (for-each (lambda (lg-pair)
                      (when (< (cdr lg-pair) best-g)
                        (set! best-l (car lg-pair))
                        (set! best-g (cdr lg-pair))))
                    (vector-ref opt-g k))
          ;; walk backwards through opt-m
          (let loop ([k k] [l best-l] [seq '()])
            (if (< k 0)
                seq
                (let ([m (cdr (assoc l (vector-ref opt-m k)))])
                  (loop (sub1 (cdr (assoc "i" m)))
                        (sub1 l)
                        (cons m seq)))))))

      ;; main DP loop
      (let outer ([k 0])
        (when (< k n)
          (for-each (lambda (m)
                      (if (> (cdr (assoc "i" m)) 0)
                          (for-each (lambda (lm-pair)
                                      (update m (add1 (car lm-pair))))
                                    (vector-ref opt-m (sub1 (cdr (assoc "i" m)))))
                          (update m 1)))
                    (vector-ref matches-by-j k))
          (bruteforce-update k)
          (outer (add1 k))))

      (let* ([optimal-sequence (unwind)]
             [optimal-l (length optimal-sequence)]
             [guesses (if (= n 0)
                          1
                          (cdr (assoc optimal-l (vector-ref opt-g (sub1 n)))))])
        (list (cons "password" password)
              (cons "guesses" guesses)
              (cons "guesses-log10" (log10 guesses))
              (cons "sequence" optimal-sequence)))))

  )
