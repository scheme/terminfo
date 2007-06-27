;;;
;;; utilities.scm
;;;

(define (char->digit c)
  (if (char-digit? c)
      (- (char->integer c) (char->integer #\0))
      -1))

(define (letter->number c)
  (if (char-alphabetic? c)
      (1+ (- (char->ascii (char-upcase c)) (char->ascii #\A)))
      (error c "This is not a letter")))

(define (1+ digit)
  (if (number? digit)
      (+ 1 digit)
      (error digit " is not a number")))