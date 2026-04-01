(library (zxcvbn matching)
  (export omnimatch
          set-user-input-dictionary)

  (import (chezscheme)
          (wak irregex)
          (zxcvbn scoring))

  ;; general helper procedures ------------------------------------------------------

  (define (build-ranked-dict freq-lists)
    (map (lambda (name)
           (let* ([lst (cadr (assoc name freq-lists))]
                  [ht (make-hashtable string-hash string=? (length lst))]
                  [ranks (map add1 (iota (length lst)))])
             (for-each (lambda (token rank)
                         (hashtable-set! ht token rank))
                       lst ranks)
             (list name ht)))
         (map car freq-lists)))

  ;; slice vector and return list
  (define (slice vec lwr upr)
    (let ([indices (map (lambda (x) (+ lwr x)) (iota (add1 (- upr lwr))))])
      (map (lambda (x) (vector-ref vec x)) indices)))

  ;; sorts output of match procs (e.g., dictionary-match) by i and then j
  (define (sort-match match)
    ;; i-vals is list of unique and sorted i values
    (let ([i-vals (sort < (remove-duplicates
                           (map (lambda (x) (cdr (assoc "i" x))) match)))])
      (apply append
             (map (lambda (i)
                    (sort (lambda (x y) (< (cdr (assoc "j" x)) (cdr (assoc "j" y))))
                          (filter (lambda (x) (= i (cdr (assoc "i" x))))
                                  match)))
                  i-vals))))

  (define (remove-duplicates ls)
    (cond [(null? ls)
           '()]
          [(member (car ls) (cdr ls))
           (remove-duplicates (cdr ls))]
          [else
           (cons (car ls) (remove-duplicates (cdr ls)))]))

  ;; prepare data and inputs -----------------------------------------------------
  
  (include "adjacency-graphs.scm")
  (include "frequency-lists.scm")
  
  (define reference-year (date-year (current-date)))
  
  (define ranked-dictionaries (build-ranked-dict frequency-lists))

  (define l33t-table
    '((#\a (#\4 #\@))
      (#\b (#\8))
      (#\c (#\( #\{ #\[ #\<))
      (#\e (#\3))
      (#\g (#\6 #\9))
      (#\i (#\1 #\! #\|))
      (#\l (#\1 #\| #\7))
      (#\o (#\0))
      (#\s (#\$ #\5))
      (#\t (#\+ #\7))
      (#\x (#\%))
      (#\z (#\2))))
    
  (define shifted-rx
    (irregex "[~!@#$%^&*()_+QWERTYUIOP{}|ASDFGHJKL:\"ZXCVBNM<>?]"))

  (define regexen
    (list (cons "recent-year" (irregex "19\\d\\d|200\\d|201\\d|202\\d"))))
  
  (define date-max-year 2050)
  (define date-min-year 1000)
  (define max-delta 5)

  ;; dictionary-match and reverse-dictionary-match ----------------------------------

  (define (dictionary-match password ranked-dict)
    (let ([dict-names (map car ranked-dict)])
      (sort-match
       (apply
        append
        (map
         (lambda (dict-name)
           (filter (lambda (x) (not (null? x)))
                   (dictionary-match-helper password dict-name ranked-dict)))
         dict-names)))))

  (define (dictionary-match-helper password dict-name ranked-dict)
    (let* ([password-lower (string-downcase password)]
           [password-vec (list->vector (string->list password-lower))]
           [password-length (vector-length password-vec)]
           [dict (cadr (assoc dict-name ranked-dict))]
           [out-vec (make-vector (/ (* password-length (add1 password-length)) 2) '())]
           [out-vec-i 0])
      (do ((i 0 (add1 i)))
          ((= i password-length))
        (do ((j i (add1 j)))
            ((= j password-length))
          (let* ([token (list->string (slice password-vec i j))]
                 [rank (hashtable-ref dict token '())])
            (when (not (null? rank))
              (vector-set! out-vec out-vec-i
                           (list (cons "pattern" "dictionary")
                                 (cons "i" i)
                                 (cons "j" j)
                                 ;; token stored as token and matched-word
                                 ;; see reverse-dictionary-match for why
                                 (cons "token" token)         
                                 (cons "matched-word" token)  
                                 (cons "rank" rank)
                                 (cons "dict-name" dict-name)
                                 (cons "reversed" #f)
                                 (cons "l33t" #f))))
            (set! out-vec-i (add1 out-vec-i)))))
      (vector->list out-vec)))
  
  (define (reverse-dictionary-match password ranked-dict)
    (let* ([password-list (string->list password)]
           [password-length (length password-list)]
           [password-reverse (list->string (reverse password-list))]
           [matches (dictionary-match password-reverse ranked-dict)])
      (sort-match
       (map
        (lambda (match)
          (map
           (lambda (match-pair)
             (let ([key (car match-pair)]
                   [val (cdr match-pair)])
               (cond [(string=? key "token")
                      (cons key (list->string (reverse (string->list val))))]
                     [(string=? key "reversed")
                      (cons key #t)]
                     [(string=? key "i")
                      (cons key (- password-length 1 (cdr (assoc "j" match))))]
                     [(string=? key "j")
                      (cons key (- password-length 1 (cdr (assoc "i" match))))]
                     [else
                      match-pair])))
           match))
        matches))))

  ;; l33t-match helpers (all defined before l33t-match to avoid forward references) ---

  ;; * relevant-l33t-subtable ----------------------

  (define (relevant-l33t-subtable password-list l33t-table)
    (let ([password-unique-chars (remove-duplicates password-list)])
      (let outer-loop ([lt l33t-table]
                       [out '()])
        (if (null? lt)
            (reverse out)
            (let ([match-chars (inner-loop (cadar lt) password-unique-chars)])
              (if (null? match-chars)
                  (outer-loop (cdr lt) out)
                  (outer-loop (cdr lt) (cons (list (caar lt) match-chars) out))))))))

  (define (inner-loop lst password-unique-chars)
    (let loop ([lst lst]
               [out '()])
      (if (null? lst)
          (reverse out)
          (if (member (car lst) password-unique-chars)
              (loop (cdr lst) (cons (car lst) out))
              (loop (cdr lst) out)))))

  ;; * enumerate-l33t-subs ----------------------

  (define (enumerate-l33t-subs subtable)
    (let* ([fliptable (flip-subtable subtable)]
           [l33t-subs (apply cartesian-product fliptable)])
      (remove-duplicates
       (sort-l33t-subs
        (apply append
               (map (lambda (sub)
                      (let ([keys (map car sub)])
                        (if (= (length keys) (length (remove-duplicates keys)))
                            (list sub)
                            (dedup sub keys))))
                    l33t-subs))))))

  ;; better description is flip and expand where necessary
  (define (flip-subtable subtable)
    (map (lambda (pair)
           (map (lambda (char)
                  (cons char (car pair)))
                (cadr pair)))
         subtable))

  (define (cartesian-product . ls)
    (fold-right product-of-two '(()) ls))

  (define (product-of-two ls1 ls2)
    (apply append
           (map (lambda (x)
                  (map (lambda (y)
                         (cons x y))
                       ls2))
                ls1)))

  ;; sort the pairs within a sub-list for remove-duplicates to work correctly
  (define (sort-l33t-subs l33t-subs)
    (map (lambda (sub)
           (sort (lambda (x y) (char<? (car x) (car y))) sub))
         l33t-subs))

  ;; * dedup ----------------------

  ;; objective of dedup is to convert, for example,
  ;; ((#\1 . #\i) (#\1 . #\l) (#\7 . #\t)) into
  ;; (((#\1 . #\l) (#\7 . #\t)) ((#\1 . #\i) (#\7 . #\t)))
  (define (dedup sub keys)
    (let loop ([iter-sub sub]
               [iter-keys keys]
               [out '()])
      (if (null? iter-sub)
          out
          (if (duplicate-key? (car iter-keys) keys)
              (loop (cdr iter-sub)
                    (cdr iter-keys)
                    (cons (filter-sub sub (car iter-sub)) out))
              (loop (cdr iter-sub) (cdr iter-keys) out)))))

  (define (duplicate-key? key keys)
    (> (length (filter (lambda (x) (char=? key x)) keys)) 1))

  ;; filter out all pairs (including focal-pair) with same key as focal-pair
  ;; cons focal-pair back on to list of any remaining pairs
  (define (filter-sub sub focal-pair)
    (cons focal-pair
          (filter (lambda (pair) (not (char=? (car focal-pair) (car pair)))) sub)))

  ;; * translate ----------------------

  (define (translate password-list sub)
    (list->string
     (map (lambda (char)
            (let ([match-char (assoc char sub)])
              (if match-char
                  (cdr match-char)
                  char)))
          password-list)))

  ;; * create-sub-display --------------

  (define (create-sub-display sub)
    (let loop ([sub sub]
               [out ""])
      (if (null? sub)
          out
          (if (string=? "" out)
              (loop (cdr sub) (csd-helper (car sub) #f))
              (loop (cdr sub)
                    (string-append out (csd-helper (car sub) #t)))))))

  (define (csd-helper pair comma?)
    (let ([str (string-append (string (car pair)) " -> " (string (cdr pair)))])
      (if comma? (string-append ", " str) str)))

  ;; * l33t-match-update --------------

  (define (l33t-match-update match token sub)
    (append
     (map (lambda (match-pair)
            (let ([key (car match-pair)]
                  [val (cdr match-pair)])
              (cond [(string=? key "l33t")
                     (cons key #t)]
                    [(string=? key "token")
                     (cons key token)]
                    [else
                     match-pair])))
          match)
     (list (cons "sub" sub)
           (cons "sub-display" (create-sub-display sub)))))

  ;; l33t-match ----------------------------------------------------------------------

  (define (l33t-match password ranked-dict l33t-table)
    (let* ([password-list (string->list password)]
           [password-vec (list->vector password-list)]
           [subtable (relevant-l33t-subtable password-list l33t-table)]
           [l33t-subs (enumerate-l33t-subs subtable)])
      (if (null? subtable)
          '()
          (sort-match
           (apply
            append
            (map
             (lambda (sub)
               (let* ([sub-password (translate password-list sub)]
                      [matches (dictionary-match sub-password ranked-dict)])
                 (let loop ([matches matches]
                            [out '()])
                   (if (null? matches)
                       out
                       (let* ([match (car matches)]
                              [i (cdr (assoc "i" match))]
                              [j (cdr (assoc "j" match))]
                              [matched-word
                               (cdr (assoc "matched-word" match))]
                              [token-list (slice password-vec i j)]
                              [token (list->string token-list)]
                              ;; filter sub to only include substitutions present in this token
                              [sub-in-token (filter (lambda (pair)
                                                      (member (car pair) token-list))
                                                    sub)])
                         (if (and (> (length token-list) 1)
                                  ;; only return matches with substitution(s)
                                  (not (null? sub-in-token)))
                             (loop
                              (cdr matches)
                              (cons (l33t-match-update match token sub-in-token) out))
                             (loop (cdr matches) out)))))))
             l33t-subs))))))

  ;; spatial-match ---------------------------------------------------------------

  (define (spatial-match password)
    (apply append
           (map (lambda (graph-pair)
                  (spatial-match-helper password (cadr graph-pair) (car graph-pair)))
                adjacency-graphs)))

  (define (spatial-match-helper password graph graph-name)
    (let ([password-vec (list->vector (string->list password))]
          [password-length (string-length password)])
      (let outer ([i 0]
                  [out '()])
        (if (>= i (sub1 password-length))
            (reverse out)
            (let inner ([j (add1 i)]
                        [last-direction #f]
                        [turns 0]
                        [shifted-count
                         (if (and (member graph-name '("qwerty" "dvorak"))
                                  (irregex-search shifted-rx (string (vector-ref password-vec i))))
                             1
                             0)])
              (let* ([prev-char (string (vector-ref password-vec (sub1 j)))]
                     [adjacents (let ([found (assoc prev-char graph)])
                                  (if found (cadr found) '()))]
                     [result (if (< j password-length)
                                 (find-adjacent (vector-ref password-vec j) adjacents)
                                 #f)])
                (if result
                    (let* ([found-direction (car result)]
                           [shifted? (cadr result)]
                           [new-shifted (if shifted? (add1 shifted-count) shifted-count)]
                           [new-turns (if (not (equal? last-direction found-direction))
                                          (add1 turns)
                                          turns)])
                      (inner (add1 j)
                             found-direction
                             new-turns
                             new-shifted))
                    (if (> (- j i) 2)
                        (outer j
                               (cons (list (cons "pattern" "spatial")
                                           (cons "i" i)
                                           (cons "j" (sub1 j))
                                           (cons "token" (substring password i j))
                                           (cons "graph" graph-name)
                                           (cons "turns" turns)
                                           (cons "shifted-count" shifted-count))
                                     out))
                        (outer j out)))))))))

  ;; returns (found-direction shifted?) if cur-char is in the adjacents list, #f otherwise
  ;; adjacents is a list of strings (2 chars: unshifted at 0, shifted at 1) or empty lists
  (define (find-adjacent cur-char adjacents)
    (let loop ([adjs adjacents]
               [direction 0])
      (if (null? adjs)
          #f
          (let ([adj (car adjs)])
            (cond
              [(not (string? adj))
               (loop (cdr adjs) (add1 direction))]
              [(char=? (string-ref adj 0) cur-char)
               (list direction #f)]
              [(and (> (string-length adj) 1)
                    (char=? (string-ref adj 1) cur-char))
               (list direction #t)]
              [else
               (loop (cdr adjs) (add1 direction))])))))

  ;; sequence-match ---------------------------------------------------------------

  (define (sequence-match password)
    (if (< (string-length password) 2)
        '()
        (let ([pv (list->vector (string->list password))]
              [n (string-length password)])
          (let loop ([k 1]
                     [run-start 0]
                     [last-delta #f]
                     [out '()])
            (if (>= k n)
                ;; emit final run
                (reverse (sequence-update pv run-start (sub1 n) last-delta out))
                (let* ([delta (- (char->integer (vector-ref pv k))
                                 (char->integer (vector-ref pv (sub1 k))))]
                       [ld (if (not last-delta) delta last-delta)])
                  (if (= delta ld)
                      (loop (add1 k) run-start ld out)
                      ;; delta changed: emit run from run-start to k-1, new run starts at k-1
                      (loop (add1 k)
                            (sub1 k)
                            delta
                            (sequence-update pv run-start (sub1 k) ld out)))))))))

  ;; emit a sequence match if conditions are met; returns updated out list
  (define (sequence-update pv i j delta out)
    (let ([abs-delta (abs delta)])
      (if (and (> abs-delta 0)
               (<= abs-delta 5)
               (or (> (- j i) 1) (= abs-delta 1)))
          (let* ([token (list->string (map (lambda (k) (vector-ref pv k))
                                           (map (lambda (x) (+ i x)) (iota (add1 (- j i))))))]
                 [chars (string->list token)])
            (let-values ([(seq-name seq-space)
                          (cond
                            [(for-all char-lower-case? chars) (values "lower" 26)]
                            [(for-all char-upper-case? chars) (values "upper" 26)]
                            [(for-all char-numeric? chars)    (values "digits" 10)]
                            [else                             (values "unicode" 26)])])
              (cons (list (cons "pattern" "sequence")
                          (cons "i" i)
                          (cons "j" j)
                          (cons "token" token)
                          (cons "sequence-name" seq-name)
                          (cons "sequence-space" seq-space)
                          (cons "ascending" (> delta 0)))
                    out)))
          out)))

  ;; repeat-match ----------------------------------------------------------------

  (define (repeat-match password)
    (let ([greedy   (irregex "(.+)\\1+")]
          [lazy     (irregex "(.+?)\\1+")]
          [lazy-anc (irregex "^(.+?)\\1+$")]
          [password-length (string-length password)])
      (let loop ([last-index 0] [out '()])
        (if (>= last-index password-length)
            (reverse out)
            (let ([greedy-m (irregex-search greedy password last-index)]
                  [lazy-m   (irregex-search lazy   password last-index)])
              (if (not greedy-m)
                  (reverse out)
                  (let* ([greedy-len (- (irregex-match-end-index greedy-m 0)
                                        (irregex-match-start-index greedy-m 0))]
                         [lazy-len   (- (irregex-match-end-index lazy-m 0)
                                        (irregex-match-start-index lazy-m 0))]
                         [use-greedy (> greedy-len lazy-len)]
                         [rx-match   (if use-greedy greedy-m lazy-m)]
                         [base-token (if use-greedy
                                         ;; greedy: extract base via lazy-anchored on full match
                                         (let ([full-match (irregex-match-substring greedy-m 0)])
                                           (irregex-match-substring
                                            (irregex-search lazy-anc full-match) 1))
                                         (irregex-match-substring lazy-m 1))]
                         [i (irregex-match-start-index rx-match 0)]
                         [j (sub1 (irregex-match-end-index rx-match 0))]
                         [full-token (irregex-match-substring rx-match 0)]
                         [base-analysis (most-guessable-match-sequence
                                         base-token
                                         (omnimatch base-token))]
                         [new-match
                          (list (cons "pattern" "repeat")
                                (cons "i" i)
                                (cons "j" j)
                                (cons "token" full-token)
                                (cons "base-token" base-token)
                                (cons "base-guesses" (cdr (assoc "guesses" base-analysis)))
                                (cons "base-matches" (cdr (assoc "sequence" base-analysis)))
                                (cons "repeat-count"
                                      (/ (string-length full-token)
                                         (string-length base-token))))])
                    (loop (add1 j) (cons new-match out)))))))))

  ;; date-match -------------------------------------------------------------------

  ;; date-splits: for unseparated date strings of length 4-8,
  ;; the possible (k l) split points where the three parts are token[0..k], token[k..l], token[l..]
  (define date-splits
    '((4 ((1 2) (2 3)))
      (5 ((1 3) (2 3)))
      (6 ((1 2) (2 4) (4 5)))
      (7 ((1 3) (2 3) (4 5) (4 6)))
      (8 ((2 4) (4 6)))))

  (define (date-match password)
    (let* ([all-matches (append (date-match-no-sep password)
                                (date-match-sep password))])
      (sort-match
       (filter (lambda (m)
                 (not (any-submatch? m all-matches)))
               all-matches))))

  (define (date-match-no-sep password)
    (let ([password-length (string-length password)]
          [maybe-date-no-sep (irregex "^\\d{4,8}$")])
      (let outer ([i 0] [out '()])
        (if (> i (- password-length 4))
            out
            (outer
             (add1 i)
             (let inner ([j (+ i 3)] [out out])
               (if (or (> j (+ i 7)) (>= j password-length))
                   out
                   (let ([token (substring password i (add1 j))])
                     (inner
                      (add1 j)
                      (if (not (irregex-search maybe-date-no-sep token))
                          out
                          (let ([best (best-dmy-for-token token)])
                            (if (not best)
                                out
                                (cons (list (cons "pattern" "date")
                                            (cons "token" token)
                                            (cons "i" i)
                                            (cons "j" j)
                                            (cons "separator" "")
                                            (cons "year" (cdr (assoc "year" best)))
                                            (cons "month" (cdr (assoc "month" best)))
                                            (cons "day" (cdr (assoc "day" best))))
                                      out)))))))))))))

  ;; for a no-separator token, find the best (closest to reference-year) dmy interpretation
  (define (best-dmy-for-token token)
    (let ([splits (cadr (assoc (string-length token) date-splits))])
      (let loop ([splits splits] [best #f] [best-dist +inf.0])
        (if (null? splits)
            best
            (let* ([kl (car splits)]
                   [k (car kl)]
                   [l (cadr kl)]
                   [dmy (map-ints-to-dmy
                         (list (string->number (substring token 0 k))
                               (string->number (substring token k l))
                               (string->number (substring token l (string-length token)))))])
              (if (not dmy)
                  (loop (cdr splits) best best-dist)
                  (let ([dist (abs (- (cdr (assoc "year" dmy)) reference-year))])
                    (if (< dist best-dist)
                        (loop (cdr splits) dmy dist)
                        (loop (cdr splits) best best-dist)))))))))

  (define (date-match-sep password)
    (let ([password-length (string-length password)]
          [maybe-date-with-sep
           (irregex "^(\\d{1,4})([\\s/\\\\_.-])(\\d{1,2})\\2(\\d{1,4})$")])
      (let outer ([i 0] [out '()])
        (if (> i (- password-length 6))
            out
            (outer
             (add1 i)
             (let inner ([j (+ i 5)] [out out])
               (if (or (> j (+ i 9)) (>= j password-length))
                   out
                   (let* ([token (substring password i (add1 j))]
                          [rx-match (irregex-search maybe-date-with-sep token)])
                     (inner
                      (add1 j)
                      (if (not rx-match)
                          out
                          (let ([dmy (map-ints-to-dmy
                                      (list (string->number (irregex-match-substring rx-match 1))
                                            (string->number (irregex-match-substring rx-match 3))
                                            (string->number (irregex-match-substring rx-match 4))))])
                            (if (not dmy)
                                out
                                (cons (list (cons "pattern" "date")
                                            (cons "token" token)
                                            (cons "i" i)
                                            (cons "j" j)
                                            (cons "separator" (irregex-match-substring rx-match 2))
                                            (cons "year" (cdr (assoc "year" dmy)))
                                            (cons "month" (cdr (assoc "month" dmy)))
                                            (cons "day" (cdr (assoc "day" dmy))))
                                      out)))))))))))))

  (define (any-submatch? m matches)
    (let ([mi (cdr (assoc "i" m))]
          [mj (cdr (assoc "j" m))])
      (let loop ([ms matches])
        (if (null? ms)
            #f
            (let ([other (car ms)])
              (if (eq? m other)
                  (loop (cdr ms))
                  (if (and (<= (cdr (assoc "i" other)) mi)
                           (>= (cdr (assoc "j" other)) mj))
                      #t
                      (loop (cdr ms)))))))))

  ;; map-ints-to-dmy: try to interpret three integers as day/month/year
  ;; returns an alist with "year" "month" "day" keys, or #f if invalid
  (define (map-ints-to-dmy ints)
    (let ([a (car ints)]
          [b (cadr ints)]
          [c (caddr ints)])
      (if (or (> b 31) (<= b 0))
          #f
          (let ([over-12 (length (filter (lambda (x) (> x 12)) ints))]
                [over-31 (length (filter (lambda (x) (> x 31)) ints))]
                [under-1 (length (filter (lambda (x) (<= x 0)) ints))])
            (if (or (>= over-31 2)
                    (= over-12 3)
                    (>= under-1 2)
                    (any (lambda (x) (and (> x 99) (< x date-min-year))) ints)
                    (any (lambda (x) (> x date-max-year)) ints))
                #f
                (try-year-splits a b c))))))

  ;; try interpreting the three ints with year as first or last element
  (define (try-year-splits a b c)
    (let try-splits ([splits (list (list c (list a b))
                                   (list a (list b c)))])
      (if (null? splits)
          ;; no 4-digit year found; try 2-digit year
          (let try-2digit ([splits (list (list c (list a b))
                                         (list a (list b c)))])
            (if (null? splits)
                #f
                (let* ([y    (car (car splits))]
                       [rest (cadr (car splits))]
                       [dm   (map-ints-to-dm rest)])
                  (if dm
                      (list (cons "year" (two-to-four-digit-year y))
                            (cons "month" (cdr (assoc "month" dm)))
                            (cons "day" (cdr (assoc "day" dm))))
                      (try-2digit (cdr splits))))))
          (let* ([y    (car (car splits))]
                 [rest (cadr (car splits))])
            (if (and (<= date-min-year y) (<= y date-max-year))
                (let ([dm (map-ints-to-dm rest)])
                  (if dm
                      (list (cons "year" y)
                            (cons "month" (cdr (assoc "month" dm)))
                            (cons "day" (cdr (assoc "day" dm))))
                      #f))
                (try-splits (cdr splits)))))))

  (define (map-ints-to-dm rest)
    (let ([d (car rest)]
          [m (cadr rest)])
      (cond
        [(and (<= 1 d 31) (<= 1 m 12)) (list (cons "day" d) (cons "month" m))]
        [(and (<= 1 m 31) (<= 1 d 12)) (list (cons "day" m) (cons "month" d))]
        [else #f])))

  (define (two-to-four-digit-year year)
    (cond
      [(> year 99) year]
      [(> year 50) (+ year 1900)]
      [else        (+ year 2000)]))

  (define (any pred lst)
    (cond
      [(null? lst) #f]
      [(pred (car lst)) #t]
      [else (any pred (cdr lst))]))

  ;; regex-match ---------------------------------------------------------------------
  ;; python version sorts the matches
  ;; but it doesn't seem necessary here
  (define (regex-match password regexen)
    (apply
     append
     (map
      (lambda (regex-pair)
        (let* ([regex-matches (re-fold (cdr regex-pair) password)]
               [tokens (map irregex-match-substring regex-matches)]
               [starts (map (lambda (x)
                              (irregex-match-start-index x 0))
                            regex-matches)]
               [ends (map (lambda (x)
                            (irregex-match-end-index x 0))
                          regex-matches)])
          (map (lambda (regex-match token start end)
                 (list (cons "pattern" "regex")
                       (cons "i" start)
                       (cons "j" (sub1 end))
                       (cons "token" token)         
                       (cons "regex-name" (car regex-pair))
                       (cons "regex-match" regex-match)))
               regex-matches tokens starts ends)))
      regexen)))

  ;; simple wrapper around irregex-fold
  ;; see http://synthcode.com/scheme/irregex/
  (define (re-fold re str)
    (irregex-fold re (lambda (i m s) (cons m s)) '() str (lambda (i s) (reverse s))))

  ;; set-user-input-dictionary ----------------------------------------------------

  (define (set-user-input-dictionary ordered-list)
    (let ([ht (make-hashtable string-hash string=? (length ordered-list))])
      (let loop ([lst ordered-list]
                 [rank 1])
        (unless (null? lst)
          (hashtable-set! ht (string-downcase (car lst)) rank)
          (loop (cdr lst) (add1 rank))))
      (set! ranked-dictionaries
            (cons (list "user_inputs" ht)
                  (filter (lambda (entry)
                            (not (string=? (car entry) "user_inputs")))
                          ranked-dictionaries)))))

  ;; omnimatch --------------------------------------------------------------------

  (define (omnimatch password)
    (sort-match
     (apply append
            (list (dictionary-match password ranked-dictionaries)
                  (reverse-dictionary-match password ranked-dictionaries)
                  (l33t-match password ranked-dictionaries l33t-table)
                  (spatial-match password)
                  (repeat-match password)
                  (sequence-match password)
                  (regex-match password regexen)
                  (date-match password)))))

  )

