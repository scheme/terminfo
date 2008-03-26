;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure terminfo terminfo-interface
  (open (modify ascii (rename (ascii->char integer->char)
                              (char->ascii char->integer)))
        bitwise
        i/o
        let-opt
        (modify scheme (hide integer->char char->integer))
        signals
        support
        tables
        threads
        srfi-1 srfi-6 srfi-9 srfi-11 srfi-13 srfi-14)
  (files terminfo
         terminfo-capabilities
         utilities))

(define-structure support (export path-list->file-name
                                  infix-splitter
                                  file-readable?
                                  file-not-exists?
                                  getenv
                                  tty-info tty-info:output-speed
                                  uname uname:os-name
                                  (with-current-input-port  :syntax)
                                  (with-current-output-port :syntax))
  (open i/o-internal scheme posix srfi-13 srfi-14 util)
  (for-syntax (open scheme i/o-internal))
  (begin
    (define ignore (unspecific))
    (define (tty-info port) ignore)
    (define (uname) ignore)

    ;; Modify as necessary
    (define (uname:os-name uname) (os-name))
    (define (tty-info:output-speed ttyinfo) 'extb)

    (define (getenv variable) (lookup-environment-variable variable))

    (define (path-list->file-name path-list)
      (string-join path-list "/"))

    (define (infix-splitter delimiter)
      (lambda (input-string)
        (string-tokenize input-string (string->char-set delimiter))))

    (define (file-readable? filename)
      (accessible? filename (access-mode read)))

    (define (file-not-exists? filename)
      (not (accessible? filename (access-mode exists))))

    (define-syntax with-current-input-port
      (syntax-rules ()
        ((with-current-input-port port body ...)
         (call-with-current-input-port port
           (lambda () body ...)))))

    (define-syntax with-current-output-port
      (syntax-rules ()
        ((with-current-output-port port body ...)
         (call-with-current-output-port port
           (lambda () body ...)))))
    ))