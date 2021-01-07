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
    (let* ([password-lower (string-downcase password)]
           [password-list (string->list password-lower)]
           [password-length (length password-list)]
           [dict-names (map car ranked-dict)])
     ;; (sort-match
       (apply append
              (map (lambda (dict-name)
                     (let loop ([lst password-list]
                                [i 0]
                                [out '()])
                       (if (null? lst)
                           out
                           (let* ([dict-list (cadr (assoc dict-name ranked-dict))]
                                  [dict-match (dictionary-match-helper lst i dict-name dict-list)])
                             (if (null? dict-match)
                                 (loop (cdr lst) (add1 i) out)
                                 (loop (cdr lst) (add1 i) (append dict-match out)))))))
                   dict-names))))

  (define (dictionary-match-helper password-list i dict-name dict-list)
    (let loop ([lst (reverse password-list)]
               [h (sub1 (length password-list))]
               [out '()])
      (if (null? lst)
          out
          ;; dropping letters off of end of lst (via reverse)
          ;; and counting from end of lst (by setting h to length and sub1)
          (let* ([token (list->string (reverse lst))]
                 [word-pair (assoc token dict-list)])
            (if word-pair
                (loop (cdr lst) (sub1 h) (cons (create-match-element
                                                "dictionary" i (+ i h) token (car word-pair)
                                                (cdr word-pair) dict-name #f #f)
                                               out))
                (loop (cdr lst) (sub1 h) out))))))

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

