#lang racket

(require json)

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

(struct rope (head middle tail) #:transparent)

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

(define (move-head rope direction)
  (let ([kheadx (car (rope-head rope))]
        [kheady (cdr (rope-head rope))])
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

(define (make-rope-start elements)
  (rope (cons 0 0) (repeat (- elements 1) (cons 0 0)) (cons 0 0)))

(define (iterate-direction direction rope-list)
  (let* ([last-rope (car rope-list)]
         [nhead (move-head last-rope direction)]
         [ntail (move-tail-closer-to-head nhead (rope-tail last-rope))])
    (cons (rope nhead '() ntail) rope-list)))

(define (run-script filename)
  (let* ([directions (decompose-motions-into-directions (open-head-motion-file filename))]
         [ropes (foldl iterate-direction (list (make-rope-start 1)) directions)]
         [tail-path (remove-duplicates (map rope-tail ropes))])
    (printf "Positions the tail visited once: ~A" (length tail-path))))


(define (rope->list rope)
  (append (cons (rope-head rope) (rope-middle rope)) (list (rope-tail rope))))

(define (list->rope l)
  (apply rope (car l) (drop-right (cdr l) 1) (take-right l 1)))

(define (move-big-rope-tail-closer-to-head head rope-remainder)
  (let* ([rope-elements (cons head rope-remainder)]
         [head-knot (take rope-elements 2)]
         [ntail (apply move-tail-closer-to-head head-knot)]
         [subropes 
          (foldl
           (lambda (rope-item rope-sliding-window)
             (let* ([last-knot (cadr (last rope-sliding-window))]
                    [ihead-knot (list last-knot rope-item)]
                    [intail (apply move-tail-closer-to-head ihead-knot)])
               (append rope-sliding-window (list (list (car ihead-knot) intail)))))
           (list (list (car head-knot) ntail)) (drop rope-elements 2))])
    (cons (caar subropes) (map cadr subropes))))

(define (json-encode-cons c)
  (list (car c) (cdr c)))

(define (rope->json r)
  (make-hash (list (cons 'head (json-encode-cons (rope-head r)))
                   (cons 'middle (map json-encode-cons (rope-middle r)))
                   (cons 'tail (json-encode-cons (rope-tail r))))))

(define (iterate-direction-big-rope direction rope-list)
  (let* ([last-rope (car rope-list)]
         [knot-list (rope->list last-rope)]
         [nhead (move-head last-rope direction)]
         [ntail (cdr knot-list)])
    (cons (list->rope (move-big-rope-tail-closer-to-head nhead ntail)) rope-list)))


(define (run-script-2 filename)
  (let* ([directions (decompose-motions-into-directions (open-head-motion-file filename))]
         [ropes (foldl iterate-direction-big-rope (list (make-rope-start 9)) directions)]
         [out (jsexpr->string (map rope->json (reverse ropes)))]
         [tail-path (remove-duplicates (map rope-tail ropes))])
    (printf "~A" out)))

(define f
  (command-line
   #:program "day9"
   #:args (filename)
   filename))

(run-script-2 f)

