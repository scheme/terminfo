;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure terminfo terminfo-interface
  (open ascii
        let-opt
        scheme
        support
        (subset threads (sleep))
        srfi-1 srfi-6 srfi-9 srfi-11 srfi-13 srfi-14 srfi-23 srfi-60 srfi-69)
  (files terminfo
         terminfo-capabilities
         utilities))

(define-structure srfi-60 (export bitwise-and
                                  bitwise-ior
                                  bitwise-xor)
  (open scheme bitwise))

(define-structure srfi-69 (export make-hash-table
                                  hash-table-ref
                                  hash-table-set!)
  (open scheme
        (modify tables (rename (make-table make-hash-table)
                               (table-ref  hash-table-ref)
                               (table-set! hash-table-set!)))))

(define-structure support (export path-list->file-name
                                  infix-splitter
                                  file-readable?
                                  file-not-exists?
                                  getenv
                                  read-byte
                                  tty-info tty-info:output-speed
                                  uname uname:os-name
                                  (when   :syntax)
                                  (unless :syntax)
                                  (with-current-input-port  :syntax)
                                  (with-current-output-port :syntax))
  (open ascii
        i/o-internal
        let-opt
        scheme
        posix
        (subset signals (error))
        srfi-13
        srfi-14
        (subset util (unspecific)))
  (for-syntax (open scheme i/o-internal))
  (begin

    (define-syntax unless
      (syntax-rules ()
        ((unless predicate action0 . actions)
         (if predicate
             #f
             (begin action0 . actions)))))

    (define-syntax when
      (syntax-rules ()
        ((when predicate action0 . actions)
         (if predicate
             (begin action0 . actions)
             #f))))

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
        (string-tokenize input-string
                         (char-set-complement (string->char-set delimiter)))))

    (define (file-readable? filename)
      (accessible? filename (access-mode read)))

    (define (file-not-exists? filename)
      (not (accessible? filename (access-mode exists))))

    (define (read-byte . args)
      (let-optionals args ((s (current-input-port)))
        (let ((value (read-char s)))
          (if (eof-object? value)
              (error "invalid data")
              (char->ascii value)))))

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