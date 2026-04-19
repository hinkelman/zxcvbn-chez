#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Travis Hinkelman
;; SPDX-License-Identifier: MIT
#!r6rs

(import (srfi :64 testing)
        (zxcvbn-chez))

;; Helpers --------------------------------------------------------------------

(define (get key result)
  (cdr (assoc key result)))

(define (get-warning result)
  (cdr (assoc "warning" (get "feedback" result))))

(define (get-suggestions result)
  (cdr (assoc "suggestions" (get "feedback" result))))

(define (seq-patterns result)
  (map (lambda (m) (cdr (assoc "pattern" m)))
       (get "sequence" result)))

(define (seq-tokens result)
  (map (lambda (m) (cdr (assoc "token" m)))
       (get "sequence" result)))

(define (approx=? a b epsilon)
  (< (abs (- a b)) epsilon))

;; Tests: return value structure -----------------------------------------------

(test-begin "return-value-structure")

(let ([r (zxcvbn "password")])
  (test-assert "has password key"          (assoc "password"           r))
  (test-assert "has guesses key"           (assoc "guesses"            r))
  (test-assert "has guesses-log10 key"     (assoc "guesses-log10"      r))
  (test-assert "has sequence key"          (assoc "sequence"           r))
  (test-assert "has crack-time-seconds key" (assoc "crack-time-seconds" r))
  (test-assert "has crack-time-display key" (assoc "crack-time-display" r))
  (test-assert "has score key"             (assoc "score"              r))
  (test-assert "has feedback key"          (assoc "feedback"           r))
  (test-equal  "password echoed back"      "password" (get "password" r))
  (test-assert "guesses is positive"       (> (get "guesses" r) 0))
  (test-assert "score is string 0-4"       (member (get "score" r) '("0" "1" "2" "3" "4")))
  (test-assert "sequence is a list"        (list? (get "sequence" r)))
  (test-assert "feedback has warning"      (assoc "warning"     (get "feedback" r)))
  (test-assert "feedback has suggestions"  (assoc "suggestions" (get "feedback" r)))
  (test-assert "crack-time-seconds has 4 entries"
               (= 4 (length (get "crack-time-seconds" r))))
  (test-assert "crack-time-display has 4 entries"
               (= 4 (length (get "crack-time-display" r)))))

(test-end "return-value-structure")

;; Tests: scores ---------------------------------------------------------------

(test-begin "scores")

;; score 0: very common / weak
(test-equal "score 0 - top password"   "0" (get "score" (zxcvbn "password")))
(test-equal "score 0 - common word"    "0" (get "score" (zxcvbn "monkey")))
(test-equal "score 0 - short repeat"   "0" (get "score" (zxcvbn "aaaaaa")))
(test-equal "score 0 - sequence"       "0" (get "score" (zxcvbn "abcdef")))
(test-equal "score 0 - keyboard walk"  "0" (get "score" (zxcvbn "zxcvbn")))
(test-equal "score 0 - l33t common"    "0" (get "score" (zxcvbn "p@ssw0rd")))
(test-equal "score 0 - recent year"    "0" (get "score" (zxcvbn "1988")))

;; score 1: weak
(test-equal "score 1 - date"           "1" (get "score" (zxcvbn "19870101")))
(test-equal "score 1 - spatial short"  "1" (get "score" (zxcvbn "zxcvb")))

;; score 2: fair
(test-equal "score 2 - two words"      "2" (get "score" (zxcvbn "correcthorse")))

;; score 4: strong
(test-equal "score 4 - passphrase"     "4" (get "score" (zxcvbn "correcthorsebatterystaple")))
(test-equal "score 4 - complex"        "4" (get "score" (zxcvbn "Tr0ub4dor&3")))

(test-end "scores")

;; Tests: guesses --------------------------------------------------------------

(test-begin "guesses")

;; guesses-log10 is log10 of guesses
(let* ([r (zxcvbn "password")]
       [guesses     (get "guesses" r)]
       [log10-guess (get "guesses-log10" r)])
  (test-assert "guesses-log10 consistent with guesses"
               (approx=? log10-guess
                         (/ (log guesses) (log 10))
                         1e-9)))

;; stronger passwords have strictly more guesses than weaker ones
(let ([g0 (get "guesses" (zxcvbn "password"))]
      [g1 (get "guesses" (zxcvbn "19870101"))]
      [g2 (get "guesses" (zxcvbn "correcthorse"))]
      [g4 (get "guesses" (zxcvbn "correcthorsebatterystaple"))])
  (test-assert "score-0 < score-1 guesses" (< g0 g1))
  (test-assert "score-1 < score-2 guesses" (< g1 g2))
  (test-assert "score-2 < score-4 guesses" (< g2 g4)))

;; spot-check known values
(test-equal "guesses for 'password'"               3  (get "guesses" (zxcvbn "password")))
(test-equal "guesses for 'Tr0ub4dor&3'"  100000000001 (get "guesses" (zxcvbn "Tr0ub4dor&3")))

(test-end "guesses")

;; Tests: pattern detection ----------------------------------------------------

(test-begin "pattern-detection")

;; dictionary
(test-equal "dictionary pattern - common word"
            '("dictionary") (seq-patterns (zxcvbn "password")))
(test-equal "dictionary token matches password"
            '("password") (seq-tokens (zxcvbn "password")))

;; l33t substitution (detected as dictionary with l33t=#t)
(let* ([r   (zxcvbn "p@ssw0rd")]
       [m   (car (get "sequence" r))])
  (test-equal "l33t - still dictionary pattern"  "dictionary"  (cdr (assoc "pattern" m)))
  (test-assert "l33t - flag is true"                           (cdr (assoc "l33t" m)))
  (test-equal  "l33t - matched-word is base word" "password"   (cdr (assoc "matched-word" m)))
  (test-equal  "l33t - token is original"         "p@ssw0rd"   (cdr (assoc "token" m))))

;; reversed
(let* ([r (zxcvbn "drowssap")]
       [m (car (get "sequence" r))])
  (test-equal  "reversed - dictionary pattern"    "dictionary" (cdr (assoc "pattern" m)))
  (test-assert "reversed - flag is true"                       (cdr (assoc "reversed" m)))
  (test-equal  "reversed - matched-word"          "password"   (cdr (assoc "matched-word" m))))

;; spatial
(let* ([r (zxcvbn "zxcvb")]
       [m (car (filter (lambda (m) (string=? "spatial" (cdr (assoc "pattern" m))))
                       (get "sequence" r)))])
  (test-equal "spatial - graph is qwerty" "qwerty" (cdr (assoc "graph" m)))
  (test-equal "spatial - token"           "zxcvb"  (cdr (assoc "token" m))))

;; repeat
(let* ([r (zxcvbn "abcabc")]
       [m (car (get "sequence" r))])
  (test-equal "repeat - pattern"        "repeat" (cdr (assoc "pattern" m)))
  (test-equal "repeat - base-token"     "abc"    (cdr (assoc "base-token" m)))
  (test-equal "repeat - repeat-count"   2        (cdr (assoc "repeat-count" m))))

(let* ([r (zxcvbn "aaaaaa")]
       [m (car (get "sequence" r))])
  (test-equal "single-char repeat - pattern"      "repeat" (cdr (assoc "pattern" m)))
  (test-equal "single-char repeat - base-token"   "a"      (cdr (assoc "base-token" m)))
  (test-equal "single-char repeat - repeat-count" 6        (cdr (assoc "repeat-count" m))))

;; sequence
(let* ([r (zxcvbn "abcdef")]
       [m (car (get "sequence" r))])
  (test-equal "sequence - pattern"         "sequence" (cdr (assoc "pattern" m)))
  (test-equal "sequence - sequence-name"   "lower"    (cdr (assoc "sequence-name" m)))
  (test-assert "sequence - ascending"                 (cdr (assoc "ascending" m))))

;; regex (recent year)
(let* ([r (zxcvbn "1988")]
       [m (car (get "sequence" r))])
  (test-equal "regex - pattern"      "regex"       (cdr (assoc "pattern" m)))
  (test-equal "regex - regex-name"   "recent-year" (cdr (assoc "regex-name" m)))
  (test-equal "regex - token"        "1988"        (cdr (assoc "token" m))))

;; date
(let* ([r (zxcvbn "19870101")]
       [m (car (get "sequence" r))])
  (test-equal "date - pattern"    "date"     (cdr (assoc "pattern" m)))
  (test-equal "date - year"       1987       (cdr (assoc "year" m)))
  (test-equal "date - month"      1          (cdr (assoc "month" m)))
  (test-equal "date - day"        1          (cdr (assoc "day" m)))
  (test-equal "date - separator"  ""         (cdr (assoc "separator" m))))

(test-end "pattern-detection")

;; Tests: feedback -------------------------------------------------------------

(test-begin "feedback")

;; common password warnings
(test-equal "top-10 password warning"
            "This is a top-10 common password"
            (get-warning (zxcvbn "password")))

(test-equal "top-100 password warning"
            "This is a top-100 common password"
            (get-warning (zxcvbn "zxcvbn")))

;; pattern-specific warnings
(test-equal "repeat single-char warning"
            "Repeats like \"aaa\" are easy to guess"
            (get-warning (zxcvbn "aaaaaa")))

(test-equal "repeat multi-char warning"
            "Repeats like \"abcabcabc\" are only slightly harder to guess than \"abc\""
            (get-warning (zxcvbn "abcabc")))

(test-equal "sequence warning"
            "Sequences like abc or 6543 are easy to guess"
            (get-warning (zxcvbn "abcdef")))

(test-equal "date warning"
            "Dates are often easy to guess"
            (get-warning (zxcvbn "19870101")))

;; suggestions content checks
(test-assert "common password has suggestion"
             (not (null? (get-suggestions (zxcvbn "password")))))

(test-assert "l33t match gets substitution suggestion"
             (member "Predictable substitutions like '@' instead of 'a' don't help very much"
                     (get-suggestions (zxcvbn "p@ssw0rd"))))

(test-assert "reversed match gets reversal suggestion"
             (member "Reversed words aren't much harder to guess"
                     (get-suggestions (zxcvbn "drowssap"))))

(test-assert "date match gets date suggestion"
             (member "Avoid dates and years that are associated with you"
                     (get-suggestions (zxcvbn "19870101"))))

;; strong password has no warning and no suggestions
(let ([r (zxcvbn "correcthorsebatterystaple")])
  (test-equal "strong password - no warning"       ""  (get-warning r))
  (test-equal "strong password - no suggestions"   '() (get-suggestions r)))

(test-end "feedback")

;; Tests: crack-time-seconds ---------------------------------------------------

(test-begin "crack-time-seconds")

(let ([r (zxcvbn "password")])
  (let ([cts (get "crack-time-seconds" r)])
    ;; all four scenarios present
    (test-assert "has online-throttling key"
                 (assoc "online-throttling-100-per-hour" cts))
    (test-assert "has online-no-throttling key"
                 (assoc "online-no-throttling-10-per-second" cts))
    (test-assert "has offline-slow key"
                 (assoc "offline-slow-hashing-1e4-per-second" cts))
    (test-assert "has offline-fast key"
                 (assoc "offline-fast-hashing-1e10-per-second" cts))
    ;; throttled is slowest attack, so largest crack time
    (test-assert "throttled crack time > unthrottled"
                 (> (cdr (assoc "online-throttling-100-per-hour" cts))
                    (cdr (assoc "online-no-throttling-10-per-second" cts))))
    ;; spot-check: 'password' has 3 guesses; at 100/hr = 3/(100/3600) = 108 sec
    (test-equal "online-throttling crack time for 'password'"
                108 (cdr (assoc "online-throttling-100-per-hour" cts)))))

(test-end "crack-time-seconds")

;; Tests: crack-time-display ---------------------------------------------------

(test-begin "crack-time-display")

;; crack-time-display structure: (((key . val)) ((key . val)) ...)
;; Flatten to a simple alist via (map car ...) for easy lookup.
(define (ctd-lookup k ctd)
  (cdr (assoc k (map car ctd))))

(let* ([r   (zxcvbn "password")]
       [ctd (get "crack-time-display" r)])
  (test-equal "weak password online-throttling display"
              "2 minutes"
              (ctd-lookup "online-throttling-100-per-hour" ctd))
  (test-equal "weak password unthrottled display"
              "less than a second"
              (ctd-lookup "online-no-throttling-10-per-second" ctd)))

(let* ([r   (zxcvbn "correcthorsebatterystaple")]
       [ctd (get "crack-time-display" r)])
  (test-equal "strong password throttled display"
              "centuries"
              (ctd-lookup "online-throttling-100-per-hour" ctd))
  (test-equal "strong password fast-offline display"
              "8 hours"
              (ctd-lookup "offline-fast-hashing-1e10-per-second" ctd)))

(test-end "crack-time-display")

;; Tests: user inputs ----------------------------------------------------------

(test-begin "user-inputs")

;; Providing a word as a user input makes the password weaker.
;; set-user-input-dictionary mutates ranked-dictionaries persistently, so all
;; "without inputs" values must be captured before any "with inputs" call.
;; Also use define (not let) to guarantee left-to-right evaluation order, as
;; Chez Scheme evaluates let bindings right-to-left.
(define guesses-without (get "guesses" (zxcvbn "MySecretName99")))
(define score-without   (string->number (get "score" (zxcvbn "MySecretName99"))))
(define guesses-with    (get "guesses" (zxcvbn "MySecretName99" '("MySecretName"))))
(define score-with      (string->number (get "score" (zxcvbn "MySecretName99" '("MySecretName")))))
(test-assert "user input reduces guesses" (< guesses-with guesses-without))
(test-assert "user input reduces score"   (< score-with  score-without))

;; The password itself is returned unchanged
(test-equal "password echoed with user inputs"
            "MySecretName99"
            (get "password" (zxcvbn "MySecretName99" '("MySecretName"))))

(test-end "user-inputs")

;; Exit with failure count -----------------------------------------------------

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
