;;; Tests the terminfo library
;;;
;;; Based on: http://uw713doc.sco.com/en/SDK_charm/_An_Example_terminfo_Program.html
;;;
;;; ,open terminfo
;;;

(define text "This is UunderlinedU. #This is BboldB.#")
(define underlined? #f)
(define bold? #f)

(define (toggle flag)
  (case flag
    ((underline)
     (set! underlined? (not underlined?))
     (tputs (if underlined?
                (enter-underline-mode)
                (exit-underline-mode))))
    ((bold)
     (set! bold? (not bold?))
     (tputs (if bold?
                (enter-bold-mode)
                (exit-attribute-mode))))))

(setup-terminal)
(string-for-each
 (lambda (c)
   (cond
    ((char=? #\U c) (toggle 'underline))
    ((char=? #\B c) (toggle 'bold))
    ((char=? #\# c) (newline))
    (else (display c))))
 text)