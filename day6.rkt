#lang racket


(define (read-message-line cfile)
  (let ([cline (read-line cfile)])
    (if (eof-object? cline)
        '()
        (cons cline (read-message-line cfile)))))

(define (open-message-file filename)
  (call-with-input-file filename
    (lambda (cfile)
      (read-message-line cfile))))


(define (find-x-non-repeating-chars message start num)
  (if (< (length
          (remove-duplicates (string->list (substring message 0 num))))
         num)
      (find-x-non-repeating-chars (substring message 1) (+ start 1) num)
      (+ start num)))

(define (find-start-of-packet-marker message)
  (find-x-non-repeating-chars message 0 4))

(define (find-start-of-message-marker message)
  (find-x-non-repeating-chars message 0 14))


(define (run-script filename)
  (let* ([msg (car (open-message-file filename))]
         [start-of-packet (find-start-of-packet-marker msg)])
    (printf "Start of packet marker at char ~A\n" start-of-packet)))

(define (run-script-2 filename)
  (let* ([msg (car (open-message-file filename))]
         [start-of-message (find-start-of-message-marker msg)])
    (printf "Start of message marker at char ~A\n" start-of-message)))
