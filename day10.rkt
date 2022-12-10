(module day10 racket
  (define (parse-cpu-instruction cline)
    (let ([instr-parts (string-split cline " ")])
      (match (car instr-parts)
        ["noop" (cons 'noop #t)]
        ["addx" (cons 'addx (string->number (second instr-parts)))])))

  (define (read-cpu-instructions-line cfile)
    (let ([cline (read-line cfile)])
      (if (eof-object? cline)
          '()
          (cons (parse-cpu-instruction cline) (read-cpu-instructions-line cfile)))))

  (define (open-cpu-instructions-file filename)
    (call-with-input-file filename
      (lambda (cfile)
        (read-cpu-instructions-line cfile))))

  (struct regs (x) #:transparent)

  (define (cycle-count opcode)
    (match opcode
      ['noop 1]
      ['addx 2]))

  (define (execute-instruction instruction registers)
    (match (car instruction)
      ['noop registers]
      ['addx (regs (+ (regs-x registers) (cdr instruction)))]))


  (define (execute-for-cycles instruction-cycle-map registers cycle-count)
    (if (or (= (length instruction-cycle-map) 0) (= cycle-count 0))
        (cons instruction-cycle-map registers)
        (let* ([instruction-cycle-item (car instruction-cycle-map)]
               [remaining-inst-cycles (vector-ref instruction-cycle-item 0)]
               [instruction (vector-ref instruction-cycle-item 1)])
          (if (> remaining-inst-cycles 1)
              (execute-for-cycles
               (cons
                (vector (- remaining-inst-cycles 1) instruction)
                (cdr instruction-cycle-map))
               registers
               (- cycle-count 1))
              (execute-for-cycles
               (cdr instruction-cycle-map)
               (execute-instruction instruction registers)
               (- cycle-count 1))))))

  (define (execute-until-start-of-cycle instruction-cycle-map registers cycle-count)
    (execute-for-cycles instruction-cycle-map registers (max 0 (- cycle-count 1))))

  (define (run-script filename)
    (let* ([cpu-instrs (open-cpu-instructions-file filename)]
           [instr-cycle-map (map (lambda (v) (vector (cycle-count (car v)) v)) cpu-instrs)]
           [signal-strengths
            (map
             (lambda (cycle) (* cycle (regs-x (cdr (execute-until-start-of-cycle instr-cycle-map (regs 1) cycle)))))
             '(20 60 100 140 180 220))])
      (printf "Signal strengths are ~A, sum is ~A\n" signal-strengths (apply + signal-strengths)))))
