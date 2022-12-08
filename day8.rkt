#lang racket

(define (read-tree-height-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons cline (read-tree-height-line cfile)))))

(define (open-tree-height-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-tree-height-line cfile))))


(define (parse-tree-height-info tree-height)
  (list->vector
   (map (lambda (line)
          (apply vector-immutable (filter identity (map string->number (string-split line "")))))
        tree-height)))


(define (get-lines-visible vector-line fold-fun)
  (let ([list-line (vector->list vector-line)])
    (fold-fun (lambda (tree index size-info)
                (if (empty? size-info)
                    (list (cons index tree))
                    (if (>= (cdar size-info) tree)
                        size-info
                        (cons (cons index tree) size-info))))
              '() list-line (range (length list-line)))))

(define (get-lines-visible-left vector-line)
  (get-lines-visible vector-line foldl))

(define (get-lines-visible-right vector-line)
  (get-lines-visible vector-line foldr))

(define (get-lines-visible-up tree-height-info column)
  (get-lines-visible-left
   (vector-map (lambda (row)
                 (vector-ref row column)) tree-height-info)))

(define (get-lines-visible-down tree-height-info column)
  (get-lines-visible-right
   (vector-map (lambda (row)
                 (vector-ref row column)) tree-height-info)))


(define (get-lines-visible-left-right tree-height-info)
  (vector->list
   (vector-map
    (lambda (vec-line idx)
      (let ([all-vec-line
             (apply set
                    (append
                     (get-lines-visible-left vec-line)
                     (get-lines-visible-right vec-line)))])

        (set-map all-vec-line (lambda (element) (cons (cons idx (car element)) (cdr element))))))
    tree-height-info
    (list->vector (range (vector-length tree-height-info))))))

(define (get-lines-visible-up-down tree-height-info)
  (vector->list
   (vector-map
    (lambda (idx)
      (let ([all-vec-line
             (apply set
                    (append
                     (get-lines-visible-up tree-height-info idx)
                     (get-lines-visible-down tree-height-info idx)))])

        (set-map all-vec-line (lambda (element) (cons (cons (car element) idx) (cdr element))))))
    (list->vector (range (vector-length (vector-ref tree-height-info 0)))))))


(define (get-trees-visible-all-directions tree-height-info)
  (let ([lines-lr (apply set-union (get-lines-visible-left-right tree-height-info))]
        [lines-ud (apply set-union (get-lines-visible-up-down tree-height-info))])
    (set-union lines-lr lines-ud)))


(define (run-script filename)
  (let* ([tree-heights (parse-tree-height-info (open-tree-height-file filename))]
         [visible-trees (get-trees-visible-all-directions tree-heights)])
    (printf "Visible trees: ~A\n" (length visible-trees))))

(define (get-closest-taller-tree-line-right coordinate-line tree-height)
  (let ([line (dropf (sort coordinate-line
                           (lambda (v1 v2) (< (cdr v1) (cdr v2))))
                    (lambda (v) (< (cdr v) tree-height)))])
    (if (empty? line)
        #f
        (car line))))

(define (get-closest-taller-tree-line-left coordinate-line tree-height)
  (let ([line (takef (sort coordinate-line
                           (lambda (v1 v2) (> (cdr v1) (cdr v2))))
                     (lambda (v) (>= (cdr v) tree-height)))])
    (if (empty? line)
        #f
        (last line))))


(define (get-maximum-viewing-left-right yx coordinate-line dimensions )
  (let-values
      ([(before after)
        (splitf-at
         (sort coordinate-line (lambda (c1 c2) (< (caar c1) (caar c2))))
         (lambda (v) (not (equal? (car v) yx))))])
    (let* ([width (cdr dimensions)]
           [pivot (car after)]
           [next-left (get-closest-taller-tree-line-left before (cdr pivot))]
           [next-right (get-closest-taller-tree-line-right (cdr after) (cdr pivot))])
      (cons
       (if (eq? next-left #f)
           (cdr yx)
           (- (cdar pivot) (cdar next-left)))
       (if (eq? next-right #f)
           (max 0 (- (cdr dimensions) (cdr yx) 1))
           (- (cdar next-right) (cdr yx)))))))


(define (get-maximum-viewing-down-up yx coordinate-line dimensions)
  (let ([height (car dimensions)])
    (get-maximum-viewing-left-right
     (cons (cdr yx) (car yx))
     (map (lambda (v) (cons (cons (cdar v) (caar v)) (cdr v))) coordinate-line)
     (cons (cdr dimensions) (car dimensions)))))


(define (get-maximum-viewing yx visibility-map dimensions)
  (let ([line
         (sort
          (filter (lambda (v) (= (caar v) (car yx))) visibility-map)
          (lambda (c1 c2) (<= (cdar c1) (cdar c2))))]
        [column
         (sort
          (filter (lambda (v) (= (cdar v) (cdr yx))) visibility-map)
          (lambda (c1 c2) (<= (caar c1) (caar c2))))]
        )
    (let ([lr (get-maximum-viewing-left-right yx line dimensions)]
          [du (get-maximum-viewing-down-up yx column dimensions)])
      (list (car lr) (cdr lr) (car du) (cdr du)))))


(define (get-tree-values tree-height)
  (let ([yx (get-dimensions tree-height)])
    (apply append
           (map (lambda (rows yidx)                  
                  (map (lambda (item xidx)                         
                         (cons (cons yidx xidx) item))                       
                       (vector->list rows) (range (cdr yx))))
                (vector->list tree-height) (range (car yx))))))


(define (get-dimensions tree-height)
  (cons
   (vector-length tree-height)
   (vector-length (vector-ref tree-height 0))))

(define (get-ideal-spot-for-tree-house visibility-map tree-height)
  (car
   (sort 
    (map
     (lambda (v)     
       (cons (car v)
             (apply * (get-maximum-viewing (car v) (get-tree-values tree-height) (get-dimensions tree-height)))))
     visibility-map)
    (lambda (v1 v2) (> (cdr v1) (cdr v2))))))

(define (run-script-2 filename)
  (let* ([tree-heights (parse-tree-height-info (open-tree-height-file filename))]
         [visible-trees (get-trees-visible-all-directions tree-heights)]
         [best-spot (get-ideal-spot-for-tree-house visible-trees tree-heights)])
    (printf "Score for the best spot: ~A\n" (cdr best-spot))))
