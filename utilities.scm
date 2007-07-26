;;; -*- Mode: Scheme; scheme48-package: terminfo -*-
;;;
;;; utilities.scm
;;;

(define (char-digit? c)
  (if (r5rs:char-digit? c)
      (char->digit c)
      #f))

(define (char->digit c)
  (if (r5rs:char-digit? c)
      (- (char->integer c) (char->integer #\0))
      (error c "This is not a digit")))

(define (letter->number c)
  (if (char-alphabetic? c)
      (1+ (- (char->ascii (char-upcase c)) (char->ascii #\A)))
      (error c "This is not a letter")))

(define (number->letter n)
  (if (or (negative? n)
          (> n 26))
      (error n "This is not between 1 and 26")
      (ascii->char (+ (1- n) (char->ascii #\a)))))

(define (1+ n)
  (if (number? n)
      (+ 1 n)
      (error n "This is not a number")))

(define (1- n)
  (if (number? n)
      (- 1 n)
      (error n "This is not a number")))

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
