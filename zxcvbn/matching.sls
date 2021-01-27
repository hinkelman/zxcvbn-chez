(library (zxcvbn matching)
  (export omnimatch
          set-user-input-dictionary)

  (import (chezscheme)
          (wak irregex))

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

  (define regexen
    (list (cons "recent-year" (irregex "19\\d\\d|200\\d|201\\d|202\\d"))))
  
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

  (define date-max-year 2050)
  (define date-min-year 1000)

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
                              [token (list->string token-list)])
                         (if (and (> (length token-list) 1)
                                  ;; only return matches with substitution(s)
                                  (not
                                   (string=? (list->string
                                              (map char-downcase token-list))
                                             matched-word)))
                             (loop
                              (cdr matches)
                              (cons (l33t-match-update match token sub) out))
                             (loop (cdr matches) out)))))))
             l33t-subs)]))))

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
              (loop (cdr sub) (create-sub-display-helper (car sub) #f))
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
     (list (cons "sub" (list sub))
           (cons "sub-display" (create-sub-display sub)))))
  
  ;; repeat-match ------------------------------------------------------------------

  

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

  
  )

