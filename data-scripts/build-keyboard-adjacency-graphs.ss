
(define qwerty
  '(("`~" "1!" "2@" "3#" "4$" "5%" "6^" "7&" "8*" "9(" "0)" "-_" "=+")
    (() "qQ" "wW" "eE" "rR" "tT" "yY" "uU" "iI" "oO" "pP" "[{" "]}" "\\|")
    (() "aA" "sS" "dD" "fF" "gG" "hH" "jJ" "kK" "lL" ";:" "'\"")
    (() "zZ" "xX" "cC" "vV" "bB" "nN" "mM" ",<" ".>" "/?")))

(define dvorak
  '(("`~" "1!" "2@" "3#" "4$" "5%" "6^" "7&" "8*" "9(" "0)" "[{" "]}")
    (() "'\"" ",<" ".>" "pP" "yY" "fF" "gG" "cC" "rR" "lL" "/?" "=+" "\\|")
    (() "aA" "oO" "eE" "uU" "iI" "dD" "hH" "tT" "nN" "sS" "-_")
    (() ";:" "qQ" "jJ" "kK" "xX" "bB" "mM" "wW" "vV" "zZ")))


(define keypad
  '((() "/" "*" "-")
    ("7" "8" "9" "+")
    ("4" "5" "6")
    ("1" "2" "3")
    (() "0" ".")))

(define keypad-mac
  '((() "=" "/" "*")
    ("7" "8" "9" "-")
    ("4" "5" "6" "+")
    ("1" "2" "3")
    (() "0" ".")))

;; loop through sub-lists in layout to build position table
(define (x-loop x-lst y)
  (let loop ([x-lst x-lst]
             [x 0]
             [out '()])
    (if (null? x-lst)
        (reverse out)
        (if (null? (car x-lst))
            (loop (cdr x-lst) (add1 x) out)  ;; count position but don't include in output
            (loop (cdr x-lst) (add1 x) (cons (list (car x-lst) (cons x y)) out))))))

;; loop through lists in layout to build position table
(define (y-loop layout)
  (let loop ([layout layout]
             [y 1]
             [out '()])
    (if (null? layout)
        (reverse out)
        (loop (cdr layout) (add1 y) (cons (x-loop (car layout) y) out)))))

(define (build-position-table layout)
  (apply append (y-loop layout)))

;; returns the six adjacent coordinates on a standard keyboard, where each row is slanted to the right from the last
;; adjacencies are clockwise, starting with key to the left, then two keys above, then right key, then two keys below
;; that is, only near-diagonal keys are adjacent, so g's coordinate is adjacent to those of t,y,b,v, but not those of r,u,n,c.)
(define (get-slanted-adjacent-coords xy)
  (let ([x (car xy)]
        [y (cdr xy)])
    (list (cons (sub1 x) y) (cons x (sub1 y)) (cons (add1 x) (sub1 y))
          (cons (add1 x) y) (cons x (add1 y)) (cons (sub1 x) (add1 y)))))

;; returns the nine clockwise adjacent coordinates on a keypad, where each row is vert aligned
(define (get-aligned-adjacent-coords xy)
  (let ([x (car xy)]
        [y (cdr xy)])
    (list (cons (sub1 x) y) (cons (sub1 x) (sub1 y)) (cons x (sub1 y)) (cons (add1 x) (sub1 y))
          (cons (add1 x) y) (cons (add1 x) (add1 y)) (cons x (add1 y)) (cons (sub1 x) (add1 y)))))

(define (get-adjacent-keys token pos-table pos-table-rev adjacency-func)
  (let* ([xy-token (cadr (assoc token pos-table))] ;; xy for token      
         [xy-adj-lst (adjacency-func xy-token)])  ;; list of xy pairs for keys adjacent to token
    (map (lambda (xy-adj)
           (let ([xy-key-adj (assoc xy-adj pos-table-rev)])
             (if xy-key-adj (cadr xy-key-adj) '())))
         xy-adj-lst)))
         
(define (build-graph-helper layout slanted)
  (let* ([pos-table (build-position-table layout)]
         [pos-table-rev (map reverse pos-table)]
         [tokens (filter (lambda (x) (not (null? x)))
                         (apply append layout))]
         [adjacency-func (if slanted
                             get-slanted-adjacent-coords
                             get-aligned-adjacent-coords)])
    (map (lambda (token)
           (list token (get-adjacent-keys token pos-table pos-table-rev adjacency-func))) tokens)))

;; split key tokens and duplicate list of adjacent keys (where necessary)
(define (build-graph layout slanted)
  (let* ([graph (build-graph-helper layout slanted)]
         [token-char0 (string->list (caar graph))]
         [token-char0-len (length token-char0)])
    (unless (for-all (lambda (x) (= token-char0-len (length (string->list (car x))))) graph)
      (assertion-violation "build-graph-dup" "not all tokens have same number of characters"))
    (cond [(= token-char0-len 1) graph]
          [(= token-char0-len 2)
           (apply append
                  (map (lambda (lst)
                         (let* ([token-char (string->list (car lst))])
                           (list (cons (string (car token-char)) (cdr lst))
                                 (cons (string (cadr token-char)) (cdr lst)))))
                       graph))]
          [else
           (assertion-violation
            "build-graph-dup"
            "token should have only 1 or 2 characters")])))

(define adjacency-graphs
  (list
   (list "qwerty" (build-graph qwerty #t))
   (list "dvorak" (build-graph dvorak #t))
   (list "keypad" (build-graph keypad #f))
   (list "keypad-mac" (build-graph keypad-mac #f))))

(with-output-to-file "adjacency-graphs.scm" (lambda () (write adjacency-graphs)))


        
    
