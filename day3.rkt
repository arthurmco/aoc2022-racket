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


(define (split-rucksacks-by-elf-groups sacks)
  (if (null? sacks)
      '()
      (let-values ([(group rest) (split-at sacks 3)])
        (cons group (split-rucksacks-by-elf-groups rest)))))

(define (get-common-item-type-on-compartments sacks)
  (set-first (apply set-intersect
                    (map (compose list->set string->list)
                         sacks))))


(define (get-common-item-type-on-group sack-group)
  (get-common-item-type-on-compartments
   (map (lambda (r) (apply string-append r)) sack-group)))


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

(define (run-script-2 filename)
  (let* ([rucksacks (open-rucksack-file filename)]
         [groups (split-rucksacks-by-elf-groups rucksacks)]
         [common-items (map get-common-item-type-on-group groups)]
         [priorities (map get-item-priority common-items)])
    (printf "Sum of priorities for all groups: ~A\n" (apply + priorities))))
