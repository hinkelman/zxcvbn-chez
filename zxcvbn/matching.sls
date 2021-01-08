(library (zxcvbn matching)
  (export omnimatch
          set-user-input-dictionary)

  (import (chezscheme)
          (wak irregex))

  (include "adjacency-graphs.scm")
  (include "frequency-lists.scm")
  (define reference-year (date-year (current-date)))

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

  (define ranked-dictionaries (build-ranked-dict frequency-lists))
  
  (define l33t-table
    '(("a" ("4" "@"))
      ("b" ("8"))
      ("c" ("(" "{" "[" "<"))
      ("e" ("3"))
      ("g" ("6" "9"))
      ("i" ("1" "!" "|"))
      ("l" ("1" "|" "7"))
      ("o" ("0"))
      ("s" ("$" "5"))
      ("t" ("+" "7"))
      ("x" ("%"))
      ("z" ("2"))))

  (define date-max-year 2050)
  (define date-min-year 1000)

  (define (dictionary-match password ranked-dict)
    (let ([dict-names (map car ranked-dict)])
      (sort-match
       (apply append
              (map (lambda (dict-name)
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
                           (create-match-element
                            ;; token stored twice as token and matched-word
                            ;; see reverse-dictionary-match for why
                            "dictionary" i j token token rank dict-name #f #f)))
            (set! out-vec-i (add1 out-vec-i)))))
      (vector->list out-vec)))
  
  ;; slice vector and return list
  (define (slice vec lwr upr)
    (let ([indices (map (lambda (x) (+ lwr x)) (iota (add1 (- upr lwr))))])
      (map (lambda (x) (vector-ref vec x)) indices)))

  (define (create-match-element pattern i j token matched-word rank dict-name reversed l33t)
    (list (cons "pattern" pattern)
          (cons "i" i)
          (cons "j" j)
          (cons "token" token)
          (cons "matched-word" matched-word)
          (cons "rank" rank)
          (cons "dict-name" dict-name)
          (cons "reversed" reversed)
          (cons "l33t" l33t)))

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

  ;; https://stackoverflow.com/questions/8382296/scheme-remove-duplicated-numbers-from-list
  (define (remove-duplicates ls)
    (cond [(null? ls)
           '()]
          [(member (car ls) (cdr ls))
           (remove-duplicates (cdr ls))]
          [else
           (cons (car ls) (remove-duplicates (cdr ls)))]))

  (define (reverse-dictionary-match password ranked-dict)
    (let* ([password-list (string->list password)]
           [password-length (length password-list)]
           [password-reverse (list->string (reverse password-list))]
           [matches (dictionary-match password-reverse ranked-dict)])
      (sort-match
       (map (lambda (match)
              (map (lambda (match-pair)
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
     

  )

