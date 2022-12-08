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
