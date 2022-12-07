#lang racket

(define (parse-commands-line cline)
  (cond
    [(string-prefix? cline "$ cd /") (cons 'change-to-root-directory #t)]
    [(string-prefix? cline "$ cd ..") (cons 'back-directory #t)]
    [(string-prefix? cline "$ cd") (cons 'change-directory (substring cline 5))]
    [(string-prefix? cline "$ ls") (cons 'list-directory #t)]
    [else (cons 'output cline)]))

(define (read-commands-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons (parse-commands-line cline) (read-commands-line cfile)))))

(define (open-commands-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-commands-line cfile))))


(define (dir-list-to-name dir-list)
  (if (or (null? dir-list) (not (non-empty-string? (first dir-list))))
      "/"
      (string-join dir-list "/")))

(define (append-directory-list dir-list dir)
  (if (or (null? dir-list) (not (non-empty-string? (first dir-list))))
      (list dir)
      (append dir-list (list dir))))


(define (full-dir-list-to-name dir-list dir-name)
  (dir-list-to-name (append-directory-list dir-list dir-name)))

(define (add-element-to-dir-list dir-hashes dir-name element)
  (let ([dir-list (dict-ref dir-hashes dir-name '())])
    (dict-set dir-hashes dir-name (append dir-list (list element)))))


(define (create-history-analysis-state current-dir-path
                                       current-dir-name
                                       current-op
                                       dir-hashes)
  (list current-dir-path current-dir-name current-op dir-hashes))


(define (iterate-commands command history-analysis-state)
  (if (eq? (car command) 'change-to-root-directory)
      (create-history-analysis-state '() "" #f (make-immutable-hash '()))
      (let ([current-dir-path (first history-analysis-state)]
            [current-dir-name (second history-analysis-state)]
            [current-op (third history-analysis-state)]
            [dir-hashes (fourth history-analysis-state)])
        (match command
          [(cons 'change-directory dir)
           (let* ([current-dir (append-directory-list current-dir-path current-dir-name)]
                  [dir-name (dir-list-to-name current-dir)])
             (create-history-analysis-state current-dir dir #f dir-hashes))]
          [(cons 'back-directory #t)
           (let ([current-dir (if (empty? current-dir-path) '() (drop-right current-dir-path 1))]
                 [dir (if (empty? current-dir-path) "" (last current-dir-path))])
             (create-history-analysis-state current-dir dir #f dir-hashes))]
          [(cons 'list-directory #t)
           (create-history-analysis-state current-dir-path current-dir-name 'ls dir-hashes)]
          [(cons 'output data)
           (if (eq? current-op 'ls)
               (let ([out-list (string-split data " ")])
                 (if (equal? (first out-list) "dir")
                     (create-history-analysis-state current-dir-path current-dir-name 'ls
                                                    (add-element-to-dir-list
                                                      dir-hashes
                                                      (full-dir-list-to-name current-dir-path current-dir-name)
                                                      (list 'directory (second out-list) 0)))
                     (create-history-analysis-state current-dir-path current-dir-name 'ls
                                                    (append
                                                     (add-element-to-dir-list
                                                      dir-hashes
                                                      (full-dir-list-to-name current-dir-path current-dir-name)
                                                      (list 'file
                                                            (second out-list)
                                                            (string->number (first out-list))))))))
               history-analysis-state)]))))

(define (get-full-directory-size history-dict dir)
  (let ([element-list (dict-ref history-dict dir '())])
    (apply
     +
     (map (lambda (element)
            (match element
              [(list 'directory name _)
               (get-full-directory-size
                history-dict
                (full-dir-list-to-name (string-split dir "/") name))]

              [(list 'file _ size) size])
            ) element-list))))


(define (get-device-dirs-size history-dict)
  (dict-map history-dict (lambda (k _)
                           (get-full-directory-size history-dict k))))


(define (run-script filename)
  (let* ([commands (open-commands-file filename)]
         [tree-dict (fourth (foldl iterate-commands '() commands))]
         [dir-sizes (get-device-dirs-size tree-dict)]
         [result (apply + (filter (lambda (v) (< v 100000)) dir-sizes))])
    (printf "Total sizes of dirs < 100000 units: ~A\n" result)))
