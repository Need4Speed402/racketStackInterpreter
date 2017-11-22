;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname test2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define-struct machine (programCounter stack memory input output))
(define-struct memory (a b))

(define BYTE1 (expt 2 8))
(define BYTE2 (expt 2 16))
(define BYTE3 (expt 2 24))
(define BYTE4 (expt 2 32))
(define BYTE5 (expt 2 40))
(define BYTE6 (expt 2 48))
(define BYTE7 (expt 2 56))
(define BYTE8 (expt 2 64))

(define (setPC machine pc)
  (make-machine
   pc
   (machine-stack machine)
   (machine-memory machine)
   (machine-input machine)
   (machine-output machine)))

(define (setStack machine index value)
  (local [
          (define (c stack index value)
            (cond
              [(zero? index) (cons value (rest stack))]
              [else (cons (first stack )(c (rest stack) (sub1 index) value))]))]
    (make-machine
     (machine-programCounter machine)
     (c (machine-stack machine) index value)
     (machine-memory machine)
     (machine-input machine)
     (machine-output machine))))

(define (getMemory machine index)
  (local [
          (define (c memory index)
            (cond
              [(number? memory)
               (cond
                 [(zero? index) memory]
                 [else 0])]
              [(even? index) (c (memory-a memory) (quotient index 2))]
              [else (c (memory-b memory) (quotient index 2))]))]
   (c (machine-memory machine) index)))

(define (setMemory machine index value)
  (local [
          (define (c memory index)
            (cond
              [(number? memory)
               (cond
                 [(zero? index) value]
                 [(odd? index) (make-memory memory (c 0 (quotient index 2)))]
                 [else (make-memory (c memory (quotient index 2)) 0)])]
              [(odd? index) (make-memory (memory-a memory) (c (memory-b memory) (quotient index 2)))]
              [else (make-memory (c (memory-a memory) (quotient index 2)) (memory-b memory))]))]
    (make-machine
     (machine-programCounter machine)
     (machine-stack machine)
     (c (machine-memory machine) index)
     (machine-input machine)
     (machine-output machine))))

(define (byteCompliment b)
  (cond
    [(< b 0) (+ b 256)]
    [(< b 128) b]
    [else (- b 256)]))

(define (shortCompliment b)
  (cond
    [(< b 0) (+ b 65536)]
    [(< b 32768) b]
    [else (- b 65536)]))

(define (intCompliment b)
  (cond
    [(< b 0) (+ b 4294967296)]
    [(< b 2147483648) b]
    [else (- b 4294967296)]))

(define (longCompliment b)
  (cond
    [(< b 0) (+ b 18446744073709551616)]
    [(< b 9223372036854775808) b]
    [else (- b 18446744073709551616)]))

;; memory in little-endian
(define (getByte machine index) (byteCompliment (getMemory machine index)))
(define (getShort machine index) (shortCompliment (+
        (getMemory machine index)
        (* (getMemory machine (+ index 1)) BYTE1))))
(define (getInt machine index) (intCompliment (+
        (getMemory machine index)
        (* (getMemory machine (+ index 1)) BYTE1)
        (* (getMemory machine (+ index 2)) BYTE2)
        (* (getMemory machine (+ index 3)) BYTE3))))
(define (getLong machine index) (longCompliment (+
        (getMemory machine index)
        (* (getMemory machine (+ index 1)) BYTE1)
        (* (getMemory machine (+ index 2)) BYTE2)
        (* (getMemory machine (+ index 3)) BYTE3)
        (* (getMemory machine (+ index 4)) BYTE4)
        (* (getMemory machine (+ index 5)) BYTE5)
        (* (getMemory machine (+ index 6)) BYTE6)
        (* (getMemory machine (+ index 7)) BYTE7))))

(define (setByte machine index value) (setMemory machine index (byteCompliment value)))
(define (setShort machine index value)
  (local [(define 2s (shortCompliment value))]
    (setMemory (setMemory machine
               index (remainder 2s BYTE1))
               (+ index 1) (remainder (quotient 2s BYTE1) BYTE1))))
(define (setInt machine index value)
  (local [(define 2s (intCompliment value))]
    (setMemory (setMemory (setMemory (setMemory machine
               index (remainder (2s BYTE1)))
               (+ index 1) (remainder (quotient 2s BYTE1) BYTE1))
               (+ index 2) (remainder (quotient 2s BYTE2) BYTE1))
               (+ index 3) (remainder (quotient 2s BYTE3) BYTE1))))
(define (setLong machine index value)
  (local [(define 2s (longCompliment value))]
    (setMemory (setMemory (setMemory (setMemory (setMemory (setMemory (setMemory (setMemory machine
               index (remainder 2s BYTE1))
               (+ index 1) (remainder (quotient 2s BYTE1) BYTE1))
               (+ index 2) (remainder (quotient 2s BYTE2) BYTE1))
               (+ index 3) (remainder (quotient 2s BYTE3) BYTE1))
               (+ index 4) (remainder (quotient 2s BYTE4) BYTE1))
               (+ index 5) (remainder (quotient 2s BYTE5) BYTE1))
               (+ index 6) (remainder (quotient 2s BYTE6) BYTE1))
               (+ index 7) (remainder (quotient 2s BYTE7) BYTE1))))

(define (bitwise v1 v2 operation)
  (local [
          (define (r count v1 v2)
            (cond
              [(< count 64) (+ (if (operation (odd? (quotient v1 (expt 2 count))) (odd? (quotient v2 (expt 2 count)))) (expt 2 count) 0) (r (add1 count) v1 v2))]
              [else 0]))]
    (longCompliment (r 0 (longCompliment v1) (longCompliment v2)))))

(define (run machine)
  (local [
          (define op (getMemory machine (machine-programCounter machine)))

          (define (mask i)
            (cond
              [(>= i 9223372036854775808) (- -1 (remainder i 9223372036854775808))]
              [(< i -9223372036854775808) (+ 9223372036854775807 (remainder i 9223372036854775808))]
              [else i]))
          
          (define (push inc run)
            (make-machine (+ (machine-programCounter machine) inc)
                          (cons (run machine (add1 (machine-programCounter machine))) (machine-stack machine))
                          (machine-memory machine)
                          (machine-input machine)
                          (machine-output machine)))
          (define (change-stack run)
            (make-machine (add1 (machine-programCounter machine))
                          (run (machine-stack machine))
                          (machine-memory machine)
                          (machine-input machine)
                          (machine-output machine)))
          (define (pop2 run)
            (run (change-stack (lambda (s) (rest (rest s))))
                 (first (machine-stack machine))
                 (second (machine-stack machine))))
          (define (pop1 run)
            (run (change-stack rest)
                 (first (machine-stack machine))))
          (define (pop2-push run)
            (change-stack (lambda (s) (cons (run machine (first s) (second s)) (rest (rest s))))))
          (define (pop1-push run)
            (change-stack (lambda (s) (cons (run machine (first s)) (rest s)))))]
    (if (= op 255) machine (run (cond
           [(= op 00) (change-stack identity)] ; nop
           [(= op 01) (push 2 getByte)] ; push1
           [(= op 02) (push 3 getShort)] ; push2
           [(= op 03) (push 5 getInt)] ; push4
           [(= op 04) (push 9 getLong)] ; push8
           [(= op 05) (pop2-push (lambda (m a b) (getByte m (+ a b))))] ; adv1
           [(= op 06) (pop2-push (lambda (m a b) (getShort m (+ a (* b 2)))))] ; adv2
           [(= op 07) (pop2-push (lambda (m a b) (getInt m (+ a (* b 4)))))] ; adv4
           [(= op 08) (pop2-push (lambda (m a b) (getLong m (+ a (* b 8)))))] ; adv8
           [(= op 09) (pop1-push getByte)] ; get1
           [(= op 10) (pop1-push getShort)] ; get2
           [(= op 11) (pop1-push getInt)] ; get4
           [(= op 12) (pop1-push getLong)] ; get8
           [(= op 13) (pop2      setByte)] ; put1
           [(= op 14) (pop2      setShort)] ; put2
           [(= op 15) (pop2      setInt)] ; put4
           [(= op 16) (pop2      setLong)] ; put8
           [(= op 17) (pop1 setPC)] ; jump
           [(= op 18) (change-stack rest)] ; pop
           [(= op 19) (change-stack (lambda (s) (cons (first s) s)))] ; dup
           [(= op 20) (pop1-push (lambda (m a) (list-ref (machine-stack machine) a)))] ; down
           [(= op 21) (pop2      (lambda (m a b) (setStack m (sub1 a) b)))] ; up
           [(= op 22) (change-stack (lambda (s) (cons (second s) (cons (first s) (rest (rest s))))))] ; swap
           [(= op 23) (pop2-push (lambda (m a b) (bitwise a b (lambda (v1 v2) (and v1 v2)))))] ; and
           [(= op 24) (pop2-push (lambda (m a b) (bitwise a b (lambda (v1 v2) (or v1 v2)))))] ; or
           [(= op 25) (pop2-push (lambda (m a b) (bitwise a b (lambda (v1 v2) (not (boolean=? v1 v2))))))] ; xor
           [(= op 26) (pop2-push (lambda (m a b) (longCompliment (quotient (longCompliment b) (expt 2 a)))))] ; shr
           [(= op 27) (pop2-push (lambda (m a b) (longCompliment (+ (quotient (longCompliment b) (expt 2 a)) (* (- (expt 2 a) 1) (expt 2 (- 64 a)))))))] ; shsr
           [(= op 28) (pop2-push (lambda (m a b) (longCompliment (remainder (* (longCompliment b) (expt 2 a)) BYTE8))))] ; shl
           [(= op 29) (pop1-push (lambda (m a) (bitwise a 0 (lambda (v1 v2) (not v1)))))] ; inv
           [(= op 30) (pop1-push (lambda (m a) (if (odd? (longCompliment a)) (longCompliment (sub1 (longCompliment a))) a)))] ; binv
           [(= op 31) (pop1-push (lambda (m a) (mask (add1 a))))] ; iinc
           [(= op 32) (pop1-push (lambda (m a) (mask (sub1 a))))] ; idec
           [(= op 33) (pop2-push (lambda (m a b) (mask (+ b a))))] ; iadd
           [(= op 34) (pop2-push (lambda (m a b) (mask (- b a))))] ; isub
           [(= op 35) (pop2-push (lambda (m a b) (mask (* b a))))] ; imul
           [(= op 36) (pop2-push (lambda (m a b) (quotient b a)))] ; idiv
           [(= op 37) (pop2-push (lambda (m a b) (mask (- b (* (floor (/ b a)) a)))))] ; imod
           [(= op 38) (pop1-push (lambda (m a) (mask (- a))))] ; ineg
           [(= op 39) (pop2-push (lambda (m a b) (sgn (- b a))))] ; icmp
           ; floating point add [(= op 40)
           ; floating point subtract [(= op 41)
           ; floating point multiply [(= op 42)
           ; floating point divide [(= op 43)
           ; floating point modulous [(= op 44)
           ; floating point negation [(= op 45)
           ; floating point comparison [(= op 46)
           ; convert float to int [(= op 47)
           ; convert int to float [(= op 48)
           [(= op 49) (make-machine
                       (add1 (machine-programCounter machine))
                       (cons (if (empty? (machine-input machine)) 0 (first (machine-input machine))) (machine-stack machine))
                       (machine-memory machine)
                       (if (empty? (machine-input machine)) empty (rest (machine-input machine)))
                       (machine-output machine))] ; get - input of the turing machine
           [(= op 50) (make-machine
                       (add1 (machine-programCounter machine))
                       (rest (machine-stack machine))
                       (machine-memory machine)
                       (machine-input machine)
                       (cons (first (machine-stack machine)) (machine-output machine)))] ; put - output of the turing machine
           [else (error op)])))))

(define (parseData data)
  (local [
          (define (charValue i)
            (cond
              [(char=? i #\A) 00] [(char=? i #\B) 32] [(char=? i #\C) 16] [(char=? i #\D) 48] 
              [(char=? i #\E) 08] [(char=? i #\F) 40] [(char=? i #\G) 24] [(char=? i #\H) 56] 
              [(char=? i #\I) 04] [(char=? i #\J) 36] [(char=? i #\K) 20] [(char=? i #\L) 52] 
              [(char=? i #\M) 12] [(char=? i #\N) 44] [(char=? i #\O) 28] [(char=? i #\P) 60] 
              [(char=? i #\Q) 02] [(char=? i #\R) 34] [(char=? i #\S) 18] [(char=? i #\T) 50] 
              [(char=? i #\U) 10] [(char=? i #\V) 42] [(char=? i #\W) 26] [(char=? i #\X) 58] 
              [(char=? i #\Y) 06] [(char=? i #\Z) 38] [(char=? i #\a) 22] [(char=? i #\b) 54] 
              [(char=? i #\c) 14] [(char=? i #\d) 46] [(char=? i #\e) 30] [(char=? i #\f) 62] 
              [(char=? i #\g) 01] [(char=? i #\h) 33] [(char=? i #\i) 17] [(char=? i #\j) 49] 
              [(char=? i #\k) 09] [(char=? i #\l) 41] [(char=? i #\m) 25] [(char=? i #\n) 57] 
              [(char=? i #\o) 05] [(char=? i #\p) 37] [(char=? i #\q) 21] [(char=? i #\r) 53] 
              [(char=? i #\s) 13] [(char=? i #\t) 45] [(char=? i #\u) 29] [(char=? i #\v) 61] 
              [(char=? i #\w) 03] [(char=? i #\x) 35] [(char=? i #\y) 19] [(char=? i #\z) 51] 
              [(char=? i #\0) 11] [(char=? i #\1) 43] [(char=? i #\2) 27] [(char=? i #\3) 59] 
              [(char=? i #\4) 07] [(char=? i #\5) 39] [(char=? i #\6) 23] [(char=? i #\7) 55] 
              [(char=? i #\8) 15] [(char=? i #\9) 47] [(char=? i #\+) 31] [(char=? i #\/) 63]
              [else -1]))

          (define (reverse4 i)
            (cond
              [(= i 00) 00] [(= i 01) 08] [(= i 02) 04] [(= i 03) 12] 
              [(= i 04) 02] [(= i 05) 10] [(= i 06) 06] [(= i 07) 14] 
              [(= i 08) 01] [(= i 09) 09] [(= i 10) 05] [(= i 11) 13] 
              [(= i 12) 03] [(= i 13) 11] [(= i 14) 07] [(= i 15) 15]))

          (define (reverseBits i)
            (+ (reverse4 (quotient i 16)) (* (reverse4 (remainder i 16)) 16)))
          
          (define (getBytes i)
            (cond
              [(zero? i) empty]
              [else (cons (reverseBits (remainder i 256)) (getBytes (quotient i 256)))]))

          (define (decodeBase64 l)
            (cond
              [(empty? l) 0]
              [(= (charValue (first l)) -1) (decodeBase64 (rest l))]
              [else (+ (charValue (first l)) (* (decodeBase64 (rest l)) 64))]))

          (define (alternating data f)
            (cond
              [(empty? data) empty]
              [f (cons (first data) (alternating (rest data) (not f)))]
              [else (alternating (rest data) (not f))]))
          
          (define (treeMemory data)
            (cond
              [(empty? data) 0]
              [(empty? (rest data)) (first data)]
              [else (make-memory (treeMemory (alternating data true)) (treeMemory (alternating data false)))]))]
    (treeMemory (getBytes (decodeBase64 (string->list data))))))

(define (runData data input)
  (cond
    [(string? input) (runData data (map char->integer (string->list input)))]
    [else (machine-output (run (make-machine 0 empty (parseData data) input empty)))]))

(define (print l)
  (local [
          (define (split chars currentChars lines)
            (cond
              [(empty? chars) (cons (list->string currentChars) lines)]
              [(= (first chars) 13) (split (rest chars) currentChars lines)]
              [(= (first chars) 10) (split (rest chars) empty (cons (list->string currentChars) lines))]
              [else (split (rest chars) (cons (integer->char (first chars)) currentChars) lines)]))]
    (split l empty empty)))
