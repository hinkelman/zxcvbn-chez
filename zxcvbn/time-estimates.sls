(library (zxcvbn time-estimates)
  (export estimate-attack-times)

  (import (chezscheme))

  (define (estimate-attack-times guesses)
    (let* ([crack-time-seconds (list (cons "online-throttling-100-per-hour" (/ guesses (/ 100 3600)))
                                     (cons "online-no-throttling-10-per-second" (/ guesses 10))
                                     (cons "offline-slow-hashing-1e4-per-second" (/ guesses 1e4))
                                     (cons "offline-fast-hashing-1e10-per-second" (/ guesses 1e10)))]
           [crack-time-display (map (lambda (x) (list (cons (car x) (display-time (cdr x))))) crack-time-seconds)])
      (values crack-time-seconds
              crack-time-display
              (guesses-to-score guesses))))

  (define (guesses-to-score guesses)
    (let ([delta 5])
      (cond [(< guesses (+ 1e3 delta)) "0"]  ;; risky password: "too guessable"
            [(< guesses (+ 1e6 delta)) "1"]  ;; modest protection from throttled online attacks: "very guessable"
            [(< guesses (+ 1e8 delta)) "2"]  ;; modest protection from unthrottled online attacks: "somewhat guessable"
            [(< guesses (+ 1e10 delta)) "3"] ;; modest protection from offline attacks: "safely unguessable"
            [else "4"])))                    ;; strong protection from offline attacks under same scenario: "very unguessable"

  (define (display-time seconds)
    (let* ([minute 60]
           [hour (* minute 60)]
           [day (* hour 24)]
           [month (* day 31)]
           [year (* month 12)]
           [century (* year 100)])
      (cond [(< seconds 1)
             "less than a second"]
            [(< seconds minute)
             (make-display-string seconds "second")]
            [(< seconds hour)
             (make-display-string (/ seconds minute) "minute")]
            [(< seconds day)
             (make-display-string (/ seconds hour) "hour")]
            [(< seconds month)
             (make-display-string (/ seconds day) "day")]
            [(< seconds year)
             (make-display-string (/ seconds month) "month")]
            [(< seconds century)
             (make-display-string (/ seconds year) "year")]
            [else "centuries"])))

  (define (make-display-string number label)
    (let* ([number-round-temp (round number)]
           [number-round (if (flonum? number-round-temp)
                             (flonum->fixnum number-round-temp)
                             number-round-temp)]
           [label-plural (if (= number-round 1)
                             label
                             (string-append label "s"))])
      (string-append (number->string number-round) " " label-plural)))
  
  )

