#lang racket

(define (parse-rucksack-line cline)
  (let ([linehalf (/ (string-length cline) 2)])
    (list (substring cline 0 linehalf) (substring cline linehalf))))

(define (read-rucksack-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons (parse-rucksack-line cline) (read-rucksack-line cfile)))))

(define (open-rucksack-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-rucksack-line cfile))))


(define (get-common-item-type-on-compartments sacks)
  (set-first (apply set-intersect
                    (map (compose list->set string->list)
                         sacks))))



(define (get-item-priority item)
  (let ([charcode (char->integer item)])
    (cond
      [(and (>= charcode 65) (<= charcode 90)) (+ (- charcode 65) 27)]
      [(and (>= charcode 97) (<= charcode 122)) (- charcode 96)]
      [else 0])))


(define (run-script filename)
  (let* ([rucksacks (open-rucksack-file filename)]
         [common-items (map get-common-item-type-on-compartments rucksacks)]
         [priorities (map get-item-priority common-items)])
    (printf "Sum of priorities: ~A\n" (apply + priorities))))
