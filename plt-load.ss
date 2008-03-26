(require (lib "1.ss"  "srfi"))
(require (lib "6.ss"  "srfi"))
(require (lib "9.ss"  "srfi"))
(require (lib "11.ss" "srfi"))
(require (lib "13.ss" "srfi"))
(require (lib "14.ss" "srfi"))
(require (lib "23.ss" "srfi"))
(require (lib "60.ss" "srfi"))
(require (lib "69.ss" "srfi"))
(require (lib "optional.ss" "srfi"))
(define-syntax let-optionals
  (syntax-rules ()
    ((let-optionals body ...)
     (let-optionals* body ...))))

(define ignore void)
(define (tty-info port) ignore)
(define (uname) ignore)

;; Modify as necessary
(define (uname:os-name uname)
  (let ((type (system-type 'os)))
    (case type
      ((macosx) "Darwin")
      (else (symbol->string type)))))

(define (tty-info:output-speed ttyinfo) 'extb)

(define (path-list->file-name path-list)
  (string-join path-list "/"))

(define (infix-splitter delimiter)
  (lambda (input-string)
    (string-tokenize input-string
                     (char-set-complement (string->char-set delimiter)))))

(define (file-readable? filename)
  (member 'read (file-or-directory-permissions filename)))

(define (file-not-exists? filename)
  (not (file-exists? filename)))

(define-syntax with-current-input-port
  (syntax-rules ()
    ((with-current-input-port port body ...)
     (let ((old-port (current-input-port)))
       (dynamic-wind
         (lambda () (current-input-port port))
         (lambda () body ...)
         (lambda () (current-input-port old-port)))))))

(define-syntax with-current-output-port
  (syntax-rules ()
    ((with-current-output-port port body ...)
     (let ((old-port (current-output-port)))
       (dynamic-wind
         (lambda () (current-output-port port))
         (lambda () body ...)
         (lambda () (current-output-port old-port)))))))

(load "terminfo.scm")
(load "terminfo-capabilities.scm")
(load "utilities.scm")