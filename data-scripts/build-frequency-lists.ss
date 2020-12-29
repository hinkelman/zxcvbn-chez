#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright (c) 2020 Travis Hinkelman
;; SPDX-License-Identifier: MIT
#!r6rs

;; run this script with the following line in the terminal
;; scheme ./data-scripts/build-frequency-lists.ss

(define dictionaries
  (list (cons "us_tv_and_film" 30000)
        (cons "english_wikipedia" 30000)
        (cons "passwords" 30000)
        (cons "surnames" 10000)
        (cons "male_names" '())
        (cons "female_names" '())))

;; each line in a file has either a single token or a token and a count separated by spaces
(define (parse-tokens line)
  (let loop ([line-lst (string->list line)]
             [out '()])
    (if (or (null? line-lst) (char=? (car line-lst) #\space))
        (list->string (reverse out))
        (loop (cdr line-lst) (cons (car line-lst) out)))))

(define (rank-tokens path)
  (let ([file (open-input-file path)])
    (let loop ([line (get-line file)]
               [rank 1]
               [out '()])
      (if (eof-object? line)
          (reverse out)
          (loop (get-line file) (add1 rank) (cons (cons (parse-tokens line) rank) out))))))

;; form of output is
;; '(("passwords" '(("123456" . 1) ("password" . 2) ("12345678" . 3) ...))
;;   ("surnames" '(("smith" . 1) ("johnson" . 2) ("williams" . 3) ...)) ...)
(define (parse-frequency-lists data-dir dict)
  (let* ([dir-sep (string (directory-separator))]
         [filenames (directory-list data-dir)]
         [file-roots (map path-root filenames)]
         [dict-names (map car dict)]
         [fmt-str1 "~a in dictionaries list but not in ~a directory; excluding ~a."]
         [fmt-str2 "~a in ~a directory but not in dictionaries list; excluding ~a."])
    (for-each (lambda (x)
                (unless (member x file-roots)
                  (warning "parse-frequency-lists" (format fmt-str1 x data-dir x))))
              dict-names)
    (filter (lambda (x) (not (null? x)))
            (map (lambda (file file-root)
                   (cond [(member file-root dict-names)
                          (let ([path (string-append data-dir dir-sep file)])
                            (list file-root (rank-tokens path)))]
                         [else
                          (warning "parse-frequency-lists"
                                   (format fmt-str2 file-root data-dir file-root))
                          '()]))
                 filenames file-roots))))

(define (is-rare-and-short token rank)
  (>= rank (expt 10 (string-length token))))

(define (filter-rare-and-short freq-lists)
  (let ([freq-names (map car freq-lists)])
    (map (lambda (name)
           (list name
                 (filter (lambda (pair)
                           (not (is-rare-and-short (car pair) (cdr pair))))
                         (cadr (assoc name freq-lists)))))
         freq-names)))

;; transform pairs into lists where the last element is the dictionary name
(define (transform freq-lists name)
  (let ([lst (cadr (assoc name freq-lists))])
    (map (lambda (pair) (list (car pair) (cdr pair) name)) lst)))

;; sort by token after transforming and appending all dictionaries into one list
(define (transform-append-sort freq-lists)
  (let ([freq-names (map car freq-lists)])
    (sort (lambda (x y) (string<? (car x) (car y)))
          (apply append (map (lambda (name) (transform freq-lists name)) freq-names)))))

;; for list with same tokens sort by rank (2nd element in sub-list)
;; and choose the first element
(define (select-min-rank same-token)
  (car (sort (lambda (x y) (< (cadr x) (cadr y))) same-token)))

(define (filter-dup-tokens freq-lists-trans)
  (let loop ([lst freq-lists-trans]
             [same-token '()] ;; list for collecting sublists with same token
             [out '()])
    (if (null? (cdr lst))
        (if (null? same-token)
            (reverse (cons (car lst) out))
            (reverse (cons (select-min-rank (cons (car lst) same-token)) out)))
        (if (string=? (caar lst) (caadr lst))
            (loop (cdr lst) (cons (car lst) same-token) out)
            (if (null? same-token)
                (loop (cdr lst) '() (cons (car lst) out))
                (loop (cdr lst) '() (cons (select-min-rank (cons (car lst) same-token)) out)))))))

(define (back-transform freq-list-trans names)
  (map (lambda (name)
         (list name
               (sort (lambda (x y) (< (cdr x) (cdr y)))
                     (map (lambda (x) (cons (car x) (cadr x)))
                          (filter (lambda (x) (string=? name (caddr x))) freq-list-trans)))))
       names))

(define (filter-cutoff-limit freq-lists dict)
  (let ([freq-names (map car freq-lists)])
    (map (lambda (name)
           (list name
                 (let ([cutoff-limit (cdr (assoc name dict))])
                   (let loop ([lst (cadr (assoc name freq-lists))]
                              [count 0]
                              [out '()])
                     (if (null? cutoff-limit)
                         (map car lst)
                         (if (or (>= count cutoff-limit) (null? lst))
                             (reverse out)
                             (loop (cdr lst) (add1 count) (cons (caar lst) out))))))))
         freq-names)))

(define-syntax ->
  (syntax-rules ()
    [(-> x) x]
    [(-> x (fn . args) . rest)
     (-> (fn x . args) . rest)]
    [(-> x fn . rest)
     (-> (fn x) . rest)]))

(define frequency-lists
  (let* ([freq-lists (parse-frequency-lists "data" dictionaries)]
         [freq-names (map car freq-lists)])
    (-> freq-lists
        (filter-rare-and-short)
        (transform-append-sort)
        (filter-dup-tokens)
        (back-transform freq-names)
        (filter-cutoff-limit dictionaries))))

(with-output-to-file "frequency-lists.scm"
  (lambda () (write `(define frequency-lists ',frequency-lists))))

(exit)
