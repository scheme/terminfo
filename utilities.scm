;;;
;;; utilities.scm
;;;

(define-syntax process-state
  (syntax-rules (accept ->)
    ((_ accept)
     (lambda (stream) (null? stream)))
    ((_ (label -> target) ...)
     (lambda (stream)
       (if
        (null? stream) #f
        (case (car stream)
          ((label) (target (cdr stream)))
          ...
          (else #f)))))))

(define-syntax automaton
  (syntax-rules (:)
    ((_ init-state
        (state : response ...)
        ...)
     (letrec ((state
               (process-state response ...))
              ...)
       init-state))))

(define-syntax push
  (syntax-rules ()
    ((_ item list)
     (set! list (cons item list)))))

(define-syntax pop
  (syntax-rules ()
    ((_ list)
     (let ((value (car list)))
       (set! list (cdr list))
       value))))

(define (char->digit c)
  (if (char-digit? c)
      (- (char->integer c) (char->integer #\0))
      -1))