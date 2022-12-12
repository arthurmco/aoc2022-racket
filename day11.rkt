(module day10 racket
  (provide run-script)
  (provide run-script-2)


  (define (read-monkey-def-line cfile)
    (let ([cline (read-line cfile)])
      (if (eof-object? cline)
          '()
          (cons cline (read-monkey-def-line cfile)))))

  (define (open-monkey-def-file filename)
    (call-with-input-file filename
      (lambda (cfile)
        (read-monkey-def-line cfile))))

  (struct monkey
    (id items operation test
        throw-at-monkey-true
        throw-at-monkey-false
        inspection-count
        divisible-value)
    #:transparent)

  (define (parse-monkey-id monkey)
    (if (string-prefix? (first monkey) "Monkey")
        (let ([monkeynum (string-trim (substring (first monkey) 6))])
          (string->number
           (substring monkeynum 0
                      (- (string-length monkeynum) 1))))
        #f))

  (define (parse-monkey-starting-items monkey)
    (if (string-prefix? (string-trim (second monkey)) "Starting items:")
        (let ([starting-str (substring (string-trim (second monkey)) 16)])
          (map (compose string->number string-trim) (string-split starting-str ",")))
        #f))

  (define (operation->function operation-terms)
    (match operation-terms
      [(list op "old" "old") (lambda (x) (op x x))]
      [(list op "old" num) (lambda (x) (op x (string->number num)))]
      [(list op num "old") (lambda (x) (op (string->number num) x))]))


  (define (parse-monkey-operation monkey)
    (let ([opstr (map string-trim (string-split (third monkey) ":"))])
      (if (equal? (car opstr) "Operation")
          (let* ([equation (map string-trim (string-split (second opstr) "="))]
                 [equation-terms (string-split (second equation) " ")])
            (operation->function
             (list
              (match (second equation-terms)
                ["+" +]
                ["-" -]
                ["*" *]
                ["/" /])
              (first equation-terms)
              (third equation-terms)))
            )
          #f)))

  (define (parse-monkey-divisible-test monkey)
    (let ([opstr (map string-trim (string-split (fourth monkey) ":"))])
      (if (equal? (car opstr) "Test")
          (let* ([terms (map string-trim (string-split (second opstr) "by"))]
                 [operation (car terms)]
                 [number (string->number (second terms))])
            (if (equal? operation "divisible")
                (lambda (x) (= (remainder x number) 0))
                #f))
          #f)))

  (define (get-monkey-divisible-value monkey)
    (let ([opstr (map string-trim (string-split (fourth monkey) ":"))])
      (if (equal? (car opstr) "Test")
          (let* ([terms (map string-trim (string-split (second opstr) "by"))]
                 [operation (car terms)]
                 [number (string->number (second terms))])
            (if (equal? operation "divisible")
                number
                #f))
          #f)))


  (define (parse-monkey-test-results monkey)
    (let ([monkey-true (string->number (substring (fifth monkey) 29))]
          [monkey-false (string->number (substring (sixth monkey) 30))])
      (cons monkey-true monkey-false)))


  (define (string-list->monkey slist)
    (let ([id (parse-monkey-id slist)]
          [starting-items (parse-monkey-starting-items slist)]
          [operation (parse-monkey-operation slist)]
          [divisible-value (get-monkey-divisible-value slist)]
          [test (parse-monkey-divisible-test slist)]
          [test-results (parse-monkey-test-results slist)])
      (if (and id starting-items operation test test-results)
          (monkey id starting-items operation test (car test-results) (cdr test-results) 0 divisible-value)
          (error "Invalid monkey data"))))


  (define (parse-monkey-info monkey-def-file-data)
    (if (null? monkey-def-file-data)
        '()
        (let-values ([(monkey-info monkey-rest)
                      (splitf-at monkey-def-file-data (compose not (curry equal? "")))])
          (cons (string-list->monkey monkey-info)
                (parse-monkey-info
                 (if (null? monkey-rest) '() (cdr monkey-rest)))))))

  (define (normal-relief v)
    (floor (/ v 3)))

  (define (get-item-worry-levels monkey given-items relief-fn)
    (map (compose relief-fn
                  (monkey-operation monkey))
         (append (monkey-items monkey) given-items)))


  (define (update-monkey-data monkey-data worry-levels interactions)
    (monkey
     (monkey-id monkey-data)
     worry-levels
     (monkey-operation monkey-data)
     (monkey-test monkey-data)
     (monkey-throw-at-monkey-true monkey-data)
     (monkey-throw-at-monkey-false monkey-data)
     (+ (monkey-inspection-count monkey-data)
        (length (monkey-items monkey-data))
        interactions)
     (monkey-divisible-value monkey-data)))

  (define (update-monkey-given-items gihash worry-levels m)
    (let ([worry-true (filter (monkey-test m) worry-levels)]
          [worry-false (filter-not (monkey-test m) worry-levels)])
      (hash-update
       (hash-update gihash (monkey-throw-at-monkey-true m) (curry append worry-true) '())
       (monkey-throw-at-monkey-false m) (curry append worry-false) '())))

  (define (less-common-multiplier monkey-vec)
    (apply * (vector->list (vector-map monkey-divisible-value monkey-vec))))

  (define (iterate-monkey-list monkey-vec relief-fn)
    (let ([new-worry-levels (foldl
                             (lambda (monkey-data mstate)
                               (let* ([given-items (hash-ref (car mstate) (monkey-id monkey-data) '())]
                                      [worry-levels (get-item-worry-levels
                                                     monkey-data given-items
                                                     relief-fn)]
                                      [items (update-monkey-given-items
                                              (hash-set (car mstate) (monkey-id monkey-data) '())
                                              worry-levels
                                              monkey-data)])
                                 (cons
                                  items
                                  (hash-set (cdr mstate) (monkey-id monkey-data)
                                            (length (hash-ref (car mstate) (monkey-id monkey-data) '()))))))
                             (cons (make-immutable-hasheq) (make-immutable-hasheq))
                             (vector->list monkey-vec))])
      (vector-map (lambda (m)
                    (update-monkey-data
                     m
                     (hash-ref (car new-worry-levels) (monkey-id m) '())
                     (hash-ref (cdr new-worry-levels) (monkey-id m) '())))
                  monkey-vec)))

  (define (iterate-monkey-list-n-times monkey-vec n relief-fn)
    (if (<= n 0)
        monkey-vec
        (iterate-monkey-list-n-times (iterate-monkey-list monkey-vec relief-fn) (- n 1) relief-fn)))

  (define (get-monkey-business monkey-list)
    (apply * (take (sort (vector->list (vector-map monkey-inspection-count monkey-list)) >) 2)))


  (define (run-script filename)
    (let* ([monkey-list (list->vector (parse-monkey-info (open-monkey-def-file filename)))]
           [monkey-after (iterate-monkey-list-n-times monkey-list 20 normal-relief)])
      (printf "Monkey business total: ~A\n" (get-monkey-business monkey-after))))

  (define (run-script-2 filename)
    (let* ([monkey-list (list->vector (parse-monkey-info (open-monkey-def-file filename)))]
           [multiplier (less-common-multiplier monkey-list)]
           [monkey-after (iterate-monkey-list-n-times monkey-list 10000 (lambda (v) (remainder v multiplier)))])
      (printf "Monkey business total (with extra worry): ~A\n" (get-monkey-business monkey-after))))

  )
