#lang racket


(define (read-calories-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons cline (read-calories-line cfile)))))

(define (open-calories-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-calories-line cfile))))


(define (split-calories-by-elf raw-calories)
  (list->vector
   (foldl (lambda (value calories-list)
            (if (equal? (string-trim value) "")
                (append calories-list (list '()))
                (let ([current-elf (last calories-list)]
                      [calories-before (drop-right calories-list 1)])
                  (append calories-before (list (append current-elf (list (string->number value))))))))
          (list '()) raw-calories)))



(define (sum-calories-per-elf calories-vec)
  (vector-map (lambda (v) (foldl + 0 v)) calories-vec))

(define (get-biggest-calorie-elf calories-vec)
  (let ([max-value (foldl max 0 (vector->list calories-vec))])
    (vector-member max-value calories-vec)))

(define (get-top-3-calories calories-sum)
  (vector-take (vector-sort calories-sum >) 3))

(define (run-script filename)
  (let* ([calories (split-calories-by-elf (open-calories-file filename))]
         [calories-total (sum-calories-per-elf calories)])
    (printf "Elf with most calories is ~A, with ~A\n"
            (+ 1 (get-biggest-calorie-elf calories-total))
            (vector-ref calories-total (get-biggest-calorie-elf calories-total)))))
  
(define (run-script-2 filename)
  (let* ([calories (split-calories-by-elf (open-calories-file filename))]
         [calories-total (sum-calories-per-elf calories)])
    (printf "Top 3 calories are ~A, with their sum being ~A\n"
            (get-top-3-calories calories-total)
            (foldl + 0 (vector->list (get-top-3-calories calories-total))))))
  

