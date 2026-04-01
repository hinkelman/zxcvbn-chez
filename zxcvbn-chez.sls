(library (zxcvbn-chez)
  (export zxcvbn)

  (import (chezscheme)
          (zxcvbn feedback)
          (zxcvbn matching)
          (zxcvbn scoring)
          (zxcvbn time-estimates))

  ;; Main entry point.
  ;; Returns an alist with keys:
  ;;   "password", "guesses", "guesses-log10", "sequence",
  ;;   "crack-time-seconds", "crack-time-display", "score", "feedback"
  (define (zxcvbn password . user-inputs)
    (unless (null? user-inputs)
      (set-user-input-dictionary (car user-inputs)))
    (let* ([matches (omnimatch password)]
           [result  (most-guessable-match-sequence password matches)]
           [guesses (cdr (assoc "guesses" result))]
           [sequence (cdr (assoc "sequence" result))])
      (let-values ([(crack-time-seconds crack-time-display score)
                    (estimate-attack-times guesses)])
        (list (cons "password"           password)
              (cons "guesses"            guesses)
              (cons "guesses-log10"      (cdr (assoc "guesses-log10" result)))
              (cons "sequence"           sequence)
              (cons "crack-time-seconds" crack-time-seconds)
              (cons "crack-time-display" crack-time-display)
              (cons "score"              score)
              (cons "feedback"           (get-feedback (string->number score) sequence))))))

  )
