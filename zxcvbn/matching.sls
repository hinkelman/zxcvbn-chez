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
           (let ([lst (cadr (assoc name freq-lists))])
             (list name
                   (map (lambda (token rank)
                          (cons token (add1 rank)))
                        lst (iota (length lst))))))
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
  
  ;; noticeable lag for longer passwords; need to convert ranked-dict to hashtable           
  (define (dictionary-match-helper password dict-name ranked-dict)
    (let* ([password-lower (string-downcase password)]
           [password-vec (list->vector (string->list password-lower))]
           [password-length (vector-length password-vec)]
           [dict-list (cadr (assoc dict-name ranked-dict))]
           [out-vec (make-vector (/ (* password-length (add1 password-length)) 2) '())]
           [out-vec-i 0])
      (do ((i 0 (add1 i)))
          ((= i password-length))
        (do ((j i (add1 j)))
            ((= j password-length))
          (let* ([token (list->string (slice password-vec i j))]
                 [word-pair (assoc token dict-list)])
            (when word-pair
              (vector-set! out-vec out-vec-i
                           (create-match-element
                            "dictionary" i j token (car word-pair)
                            (cdr word-pair) dict-name #f #f)))
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
    (let ([i-vals (sort < (remove-duplicates (map (lambda (x) (cdr (assoc "i" x))) match)))])
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

                    
                          
    
      

  )

