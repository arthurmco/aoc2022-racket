#lang racket

(define (string-split-at-char-count s num)
  (if (<= (string-length s) 3)
      (list s)
      (cons (substring s 0 num) (string-split-at-char-count (substring s (+ num 1)) num))))

(define (read-crate-stack-line cfile)
  (let ([cline (read-line cfile)])
    (if (equal? cline "")
        '()
        (cons (string-split-at-char-count cline 3)
              (read-crate-stack-line cfile)))))

(define (transform-crate-into-token crate)
  (if (and (string-prefix? crate "[") (string-suffix? crate "]"))
      (substring crate 1 2)
      #f))


(define (identify-crates crate-row transposed-dict)
  (let ([crates (map (lambda (c row)
                       (cons row (transform-crate-into-token c)))
                     crate-row (range 1 (+ (length crate-row) 1)))])
    (foldl (lambda (rowinfo dict)
             (let ([crate-info (dict-ref dict (car rowinfo) '())])
               (if (eq? (cdr rowinfo) #f)
                   dict
                   (dict-set dict (car rowinfo) (cons (cdr rowinfo) crate-info)))))
             transposed-dict crates)))

(define (initialize-crate-dictionary crate-nums)
  (let ([crate-num-list
         (map (compose string->number string-trim)
              crate-nums)])
    (make-immutable-hash (map (lambda (n) (cons (+ n 1) '())) crate-num-list))))

(define (parse-crate-stack-start crates)
  (foldl identify-crates (initialize-crate-dictionary (car crates)) (cdr crates)))


(struct instruction (move from to) #:transparent)


(define (instruction-create line)
  (let ([regex-line
         (regexp-match #px"move\\s(\\d*)\\sfrom\\s(\\d*)\\sto\\s(\\d*)" line)])
    (if (list? regex-line)
        (apply instruction
               (map string->number (cdr regex-line)))
        #f)))

(define (read-crate-instruction-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons cline (read-crate-instruction-line cfile)))))


(define (move-single-crate crate-dict from to)
  (let* ([list-from (dict-ref crate-dict from)]
         [list-to (dict-ref crate-dict to)]
         [new-from (cdr list-from)]
         [new-to (cons (car list-from) list-to)])
    (dict-set (dict-set crate-dict to new-to) from new-from)))

(define (execute-instruction instruction crate-dict)
  (let ([times (range (instruction-move instruction))])
    (foldl (lambda (t dict)
             (move-single-crate dict
                                (instruction-from instruction)
                                (instruction-to instruction)))
           crate-dict times)))

(define (open-crate-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (cons
       (parse-crate-stack-start (reverse (read-crate-stack-line cfile)))
       (map instruction-create (read-crate-instruction-line cfile))))))


(define (make-elves-crate-state-message crate-dict)
  (apply string-append
         (dict-map crate-dict
                   (lambda (k v)
                     (if (pair? v)
                         (car v)
                         "")))))


(define (run-script filename)
  (let* ([crate-info (open-crate-file filename)]
         [crate-initial-state (car crate-info)]
         [crate-instructions (cdr crate-info)]
         [crate-final-state (foldl execute-instruction crate-initial-state crate-instructions)])
    (printf "Elves crate state message: '~A'\n" (make-elves-crate-state-message crate-final-state))))
