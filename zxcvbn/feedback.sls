(library (zxcvbn feedback)
  (export get-feedback)

  (import (chezscheme)
          (wak irregex)
          (zxcvbn scoring))

  (define start-upper-rx (irregex "^[A-Z][^A-Z]+$"))
  (define all-upper-rx   (irregex "^[A-Z]+$"))

  (define default-feedback
    (list (cons "warning" "")
          (cons "suggestions"
                (list "Use a few words, avoid common phrases"
                      "No need for symbols, digits, or uppercase letters"))))

  (define (get-feedback score sequence)
    (if (null? sequence)
        default-feedback
        (if (> score 2)
            (list (cons "warning" "")
                  (cons "suggestions" '()))
            (let* ([longest-match
                    (let loop ([ms (cdr sequence)] [best (car sequence)])
                      (if (null? ms)
                          best
                          (loop (cdr ms)
                                (if (> (string-length (cdr (assoc "token" (car ms))))
                                       (string-length (cdr (assoc "token" best))))
                                    (car ms)
                                    best))))]
                   [extra-feedback "Add another word or two. Uncommon words are better."]
                   [feedback (get-match-feedback longest-match (= (length sequence) 1))])
              (if feedback
                  (list (cons "warning" (cdr (assoc "warning" feedback)))
                        (cons "suggestions"
                              (cons extra-feedback (cdr (assoc "suggestions" feedback)))))
                  (list (cons "warning" "")
                        (cons "suggestions" (list extra-feedback))))))))

  (define (get-match-feedback match is-sole-match)
    (let ([pattern (cdr (assoc "pattern" match))])
      (cond
        [(string=? pattern "dictionary")
         (get-dictionary-match-feedback match is-sole-match)]
        [(string=? pattern "spatial")
         (let ([warning (if (= (cdr (assoc "turns" match)) 1)
                            "Straight rows of keys are easy to guess"
                            "Short keyboard patterns are easy to guess")])
           (list (cons "warning" warning)
                 (cons "suggestions"
                       (list "Use a longer keyboard pattern with more turns"))))]
        [(string=? pattern "repeat")
         (let ([warning (if (= (string-length (cdr (assoc "base-token" match))) 1)
                            "Repeats like \"aaa\" are easy to guess"
                            "Repeats like \"abcabcabc\" are only slightly harder to guess than \"abc\"")])
           (list (cons "warning" warning)
                 (cons "suggestions" (list "Avoid repeated words and characters"))))]
        [(string=? pattern "sequence")
         (list (cons "warning" "Sequences like abc or 6543 are easy to guess")
               (cons "suggestions" (list "Avoid sequences")))]
        [(string=? pattern "regex")
         (if (string=? (cdr (assoc "regex-name" match)) "recent-year")
             (list (cons "warning" "Recent years are easy to guess")
                   (cons "suggestions"
                         (list "Avoid recent years"
                               "Avoid years that are associated with you")))
             #f)]
        [(string=? pattern "date")
         (list (cons "warning" "Dates are often easy to guess")
               (cons "suggestions"
                     (list "Avoid dates and years that are associated with you")))]
        [else #f])))

  (define (get-dictionary-match-feedback match is-sole-match)
    (let* ([dict-name (cdr (assoc "dict-name" match))]
           [l33t      (cdr (assoc "l33t" match))]
           [reversed  (cdr (assoc "reversed" match))]
           [rank      (cdr (assoc "rank" match))]
           [token     (cdr (assoc "token" match))]
           [warning
            (cond
              [(string=? dict-name "passwords")
               (if (and is-sole-match (not l33t) (not reversed))
                   (cond
                     [(<= rank 10)  "This is a top-10 common password"]
                     [(<= rank 100) "This is a top-100 common password"]
                     [else          "This is a very common password"])
                   (if (and (assoc "guesses-log10" match)
                            (<= (cdr (assoc "guesses-log10" match)) 4))
                       "This is similar to a commonly used password"
                       ""))]
              [(string=? dict-name "english_wikipedia")
               (if is-sole-match "A word by itself is easy to guess" "")]
              [(member dict-name '("surnames" "male_names" "female_names"))
               (if is-sole-match
                   "Names and surnames by themselves are easy to guess"
                   "Common names and surnames are easy to guess")]
              [else ""])]
           [suggestions
            (let ([s '()])
              (when (irregex-search start-upper-rx token)
                (set! s (cons "Capitalization doesn't help very much" s)))
              (when (and (irregex-search all-upper-rx token)
                         (not (string=? (string-downcase token) token)))
                (set! s (cons "All-uppercase is almost as easy to guess as all-lowercase" s)))
              (when (and reversed (>= (string-length token) 4))
                (set! s (cons "Reversed words aren't much harder to guess" s)))
              (when l33t
                (set! s (cons "Predictable substitutions like '@' instead of 'a' don't help very much" s)))
              (reverse s))])
      (list (cons "warning" warning)
            (cons "suggestions" suggestions))))

  )
