;;;
;;; utilities.scm
;;;

(define (char->digit c)
  (if (char-digit? c)
      (- (char->integer c) (char->integer #\0))
      -1))

(define (1+ digit)
  (if (number? digit)
      (+ 1 digit)
      (error digit " is not a number")))