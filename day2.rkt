#lang racket


(define (parse-strategy-line line)
  (cons (match (first line)
          ["A" 'rock]
          ["B" 'paper]
          ["C" 'scissors])

        (match (second line)
          ["X" 'rock]
          ["Y" 'paper]
          ["Z" 'scissors])))

(define (strategy-line-victory? strategy)
  (match strategy
    [(cons 'scissors 'rock) #t]
    [(cons 'paper 'scissors) #t]
    [(cons 'rock 'paper) #t]
    [_ #f]))

(define (strategy-line-draw? strategy)
  (match strategy
    [(cons 'rock 'rock) #t]
    [(cons 'scissors 'scissors) #t]
    [(cons 'paper 'paper) #t]
    [_ #f]))

(define (strategy-score strategy)
  (+
   (match (cdr strategy)
     ['rock 1]
     ['paper 2]
     ['scissors 3])
   (cond
    [(strategy-line-victory? strategy) 6]
    [(strategy-line-draw? strategy) 3]
    [else 0])))


(define (read-strategy-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons (parse-strategy-line (string-split cline " ")) (read-strategy-line cfile)))))

(define (open-strategy-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-strategy-line cfile))))


(define (run-script filename)
  (let* ([strategy (open-strategy-file filename)]
         [scores (map strategy-score strategy)]
         [sum (apply + scores)])
    (printf "Total score is ~A\n" sum)))
