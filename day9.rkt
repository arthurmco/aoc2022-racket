#lang racket

(define (parse-head-motion-line cline)
  (let ([linestr (string-split cline " ")])
    (cons
     (match (first linestr)
       ["R" 'right]
       ["U" 'up]
       ["L" 'left]
       ["D" 'down])
     (string->number (second linestr)))))

(define (read-head-motion-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons (parse-head-motion-line cline) (read-head-motion-line cfile)))))

(struct knot (head tail) #:transparent)

(define (tail-touching-head? head tail)
  (let ([xdist (abs (- (car head) (car tail)))]
        [ydist (abs (- (cdr head) (cdr tail)))])
    (and (<= xdist 1) (<= ydist 1))))

(define (move-tail-closer-to-head head tail)
  (let ([xdist (- (car head) (car tail))]
        [ydist (- (cdr head) (cdr tail))])
    (cond
      [(tail-touching-head? head tail) tail]
      [(and (= ydist 0) (<= 2 (abs xdist))) (cons (+ (car tail) (/ xdist 2)) (cdr tail))]
      [(and (= xdist 0) (<= 2 (abs ydist))) (cons (car tail) (+ (cdr tail) (/ ydist 2)))]
      [else (cons (+ (car tail) (/ xdist (abs xdist))) (+ (cdr tail) (/ ydist (abs ydist))))])))

(define (open-head-motion-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-head-motion-line cfile))))


(define (repeat n val)
  (cond
    [(<= n 0) '()]
    [(= n 1) (list val)]
    [else (cons val (repeat (- n 1) val))]))

(define (move-head knot direction)
  (let ([kheadx (car (knot-head knot))]
        [kheady (cdr (knot-head knot))])
    (cons (+ kheadx
             (match direction
               ['right 1]
               ['left -1]
               [_ 0]))
          (+ kheady
             (match direction
               ['up 1]
               ['down -1]
               [_ 0])))))


(define (decompose-motions-into-directions head-motion-info)
  (flatten
   (map (lambda (instr)          
          (repeat (cdr instr) (car instr)))
        head-motion-info )))

(define (make-knot-start)
  (knot (cons 0 0) (cons 0 0)))

(define (iterate-direction direction knot-list)
  (let* ([last-knot (car knot-list)]
         [nhead (move-head last-knot direction)]
         [ntail (move-tail-closer-to-head nhead (knot-tail last-knot))])
    (cons (knot nhead ntail) knot-list)))


(define (run-script filename)
  (let* ([directions (decompose-motions-into-directions (open-head-motion-file filename))]
         [knots (foldl iterate-direction (list (make-knot-start)) directions)]
         [tail-path (remove-duplicates (map knot-tail knots))])
    (printf "Positions the tail visited once: ~A" (length tail-path))))
