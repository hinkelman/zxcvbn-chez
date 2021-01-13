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

  (define regexen
    (list (cons "recent-year" (irregex "19\\d\\d|200\\d|201\\d|202\\d"))))
  
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

  ;; python version sorts the matches
  ;; but it doesn't seem necessary here
  (define (regex-match password regexen)
    (apply append
           (map (lambda (regex-pair)
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

