(module day10 racket
  (provide run-script)
  (provide run-script-2)
  
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

  (define (sprite-region registers)
    (list (- (regs-x registers) 1) (regs-x registers) (+ (regs-x registers) 1)))
  
  (define (update-screen screen sprite-index cycle-num registers)
    (if (or (eq? screen #f) (>= cycle-num (vector-length screen)))
        screen               
        (vector-map (lambda (pixel index)
                      (let ([probable-sprite (map (curry max 0) (sprite-region registers))])
                        (if (and (= index cycle-num)
                                 (list? (member (remainder index 40) probable-sprite)))
                              #t
                              pixel)))
                    screen
                    (list->vector (range (vector-length screen))))))


  (define (execute-for-cycles instruction-cycle-map registers cycle-count cycle-total screen)
      (if (or (= (length instruction-cycle-map) 0) (= cycle-count 0))
          (list instruction-cycle-map registers screen)
          (let* ([instruction-cycle-item (car instruction-cycle-map)]
                 [remaining-inst-cycles (vector-ref instruction-cycle-item 0)]
                 [instruction (vector-ref instruction-cycle-item 1)]
                 [new-screen (update-screen screen 0 (- cycle-total cycle-count) registers)])
            (if (> remaining-inst-cycles 1)
                (execute-for-cycles
                 (cons
                  (vector (- remaining-inst-cycles 1) instruction)
                  (cdr instruction-cycle-map))
                 registers
                 (- cycle-count 1)
                 cycle-total
                 new-screen)
                (execute-for-cycles
                 (cdr instruction-cycle-map)
                 (execute-instruction instruction registers)
                 (- cycle-count 1)
                 cycle-total
                 new-screen)))))

    (define (execute-until-start-of-cycle instruction-cycle-map registers cycle-count screen)
      (execute-for-cycles instruction-cycle-map registers (max 0 (- cycle-count 1))                          
                          (max 0 (- cycle-count 1)) screen))

    (define (run-script filename)
      (let* ([cpu-instrs (open-cpu-instructions-file filename)]
             [instr-cycle-map (map (lambda (v) (vector (cycle-count (car v)) v)) cpu-instrs)]
             [signal-strengths
              (map
               (lambda (cycle) (* cycle
                                  (regs-x (second (execute-until-start-of-cycle instr-cycle-map (regs 1) cycle #f)))))
               '(20 60 100 140 180 220))])
        (printf "Signal strengths are ~A, sum is ~A\n" signal-strengths (apply + signal-strengths))))

    (define (create-screen)
      (vector->immutable-vector (make-vector 240 #f)))


    (define (screen-line->string screen-line)
      (list->string (vector->list screen-line)))
    
    (define (create-screen-lines screen)
      (if (<= (vector-length screen) 40)
          (list (screen-line->string screen))
          (let-values ([(screen-line screen-remain)
                        (vector-split-at screen 40)])
            (cons (screen-line->string screen-line)                  
                  (create-screen-lines screen-remain)))))
    
    (define (draw-screen screen)
      (let ([screen-data (vector-map (lambda (pixel)                                
                                (if pixel #\# #\.)) screen)])
        (string-join (create-screen-lines screen-data) "\n")))

    (define (run-script-2 filename)
      (let* ([cpu-instrs (open-cpu-instructions-file filename)]
             [instr-cycle-map (map (lambda (v) (vector (cycle-count (car v)) v)) cpu-instrs)]
             [screen (create-screen)]
             [data (execute-until-start-of-cycle instr-cycle-map (regs 1) 241 screen)]
             [screen-contents (draw-screen (third data))])
        (printf "Screen content:\n~A\n" screen-contents))))
