#lang racket

(define (parse-section-line cline)
  (map
   (lambda (v)
     (let ([sections (map string->number (string-split v "-"))])
       (cons (first sections) (second sections))))
   (string-split cline ",")))

(define (read-section-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons (parse-section-line cline) (read-section-line cfile)))))


(define (pairs-contain-one-another? smaller larger)
  (let ([smstart (car smaller)]
        [smend (cdr smaller)]
        [lstart (car larger)]
        [lend (cdr larger)])
    (and (<= lstart smstart) (>= lend smend))))


(define (pair-difference pair)
  (- (cdr pair) (car pair)))

(define (one-pair-fully-contains-another? section)
  (let ([first-pair (first section)]
        [second-pair (second section)])
       
    (if (>= (pair-difference first-pair) (pair-difference second-pair))
        (pairs-contain-one-another? second-pair first-pair)
        (pairs-contain-one-another? first-pair second-pair))))


(define (open-section-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-section-line cfile))))



; '((2 . 4) (6 . 8))
; 2 3 4
;       / 6 7 8
;
;
; '((5 . 7) (7 . 9))
; 5 6 7
;     7 8 9 
(define (one-pair-overlaps-another? section)
  (let ([first-pair (first section)]
        [second-pair (second section)])
    (let ([r1 (car first-pair)]
          [r2 (car second-pair)])

      (and (<= r1 (+ r2 (pair-difference second-pair)))
           (>= (+ r1 (pair-difference first-pair)) r2)))))
      



(define (run-script filename)
  (let* ([sections (open-section-file filename)]
         [pair-map (map one-pair-fully-contains-another? sections)]
         [count-fully-contains (count identity pair-map)])

    (printf "Number of sections that one pair fully contains the other: ~A\n"
            count-fully-contains)))

(define (run-script-2 filename)
  (let* ([sections (open-section-file filename)]
         [pair-map (map one-pair-overlaps-another? sections)]
         [count-overlaps (count identity pair-map)])

    (printf "Number of sections that one pair overlaps the other: ~A\n"
            count-overlaps)))
