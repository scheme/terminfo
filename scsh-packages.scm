;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure terminfo terminfo-interface
  (open
   ascii
   srfi-1 srfi-6 srfi-9 srfi-11 srfi-13 srfi-14
   scheme
   error-package
   field-reader-package
   scsh-level-0
   i/o let-opt
   (modify tables (rename (make-table make-hash-table)
                          (table-ref  hash-table-ref)
                          (table-set! hash-table-set!)))
   threads)
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

    (define (read-byte . args)
      (let-optionals args ((s (current-input-port)))
        (let ((value (read-char s)))
          (if (eof-object? value)
              (error "invalid data")
              (char->ascii value))))) )
  (files terminfo
         terminfo-capabilities
         utilities))
