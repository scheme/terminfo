;;;
;;; utilities.scm
;;;

(define (char->digit c)
  (if (char-digit? c)
      (- (char->integer c) (char->integer #\0))
      (error c "This is not a digit")))

(define (digit->char c)
  (cond
   ((not (number? c))
    (error c "This is not a number"))
   ((or (zero? c)
        (and (positive? c) (< c 10)))
    (ascii->char (+ c (char->ascii #\0))))
   (else (error c "This is not a digit"))))

(define (letter->number c)
  (if (char-alphabetic? c)
      (1+ (- (char->ascii (char-upcase c)) (char->ascii #\A)))
      (error c "This is not a letter")))

(define (number->letter n)
  (if (or (negative? n)
          (> n 26))
      (error n "This is not between 1 and 26")
      (ascii->char (+ ))))

(define (1+ digit)
  (if (number? digit)
      (+ 1 digit)
      (error digit "This is not a number")))

(define push cons)
(define pop car)

(define (char->procedure c)
  (case c
    ((#\+) +)
    ((#\-) -)
    ((#\*) *)
    ((#\/) /)
    ((#\m) modulo)
    ((#\&) bitwise-and)
    ((#\|) bitwise-ior)
    ((#\^) bitwise-xor)
    ((#\=) =)
    ((#\>) >)
    ((#\<) <)
    (else (error c "This is not recognized."))))
