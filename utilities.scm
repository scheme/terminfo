;;;
;;; utilities.scm
;;;

(define (char->digit c)
  (if (char-digit? c)
      (- (char->integer c) (char->integer #\0))
      -1))