;;; -*- Mode: Scheme; scheme48-package: terminfo -*-
;;;
;;; utilities.scm
;;;

(define (char-digit? c)
  (if (char-set-contains? char-set:digit c)
      (char->digit c)
      #f))

(define (char->digit c)
  (if (char-set-contains? char-set:digit c)
      (- (char->integer c) (char->integer #\0))
      (error c "This is not a digit")))

(define (letter->number c)
  (if (char-alphabetic? c)
      (+ 1 (- (char->integer (char-upcase c)) (char->integer #\A)))
      (error c "This is not a letter")))

(define (number->letter n)
  (if (or (negative? n)
          (> n 26))
      (error n "This is not between 1 and 26")
      (integer->char (+ (- n 1) (char->integer #\a)))))

(define push cons)
(define (pop stack)
  (if (null? stack)
      (error "The stack is empty")
      (car stack)))

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

(define (member? atom list)
  (cond ((null? list) #f)
        ((eq? atom (car list)) #t)
        (else (member? atom (cdr list)))))
