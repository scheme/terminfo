(define-interface terminfo-interface
  (export *terminfo* *terminfo-directories* set-terminal terminfo-capability))

(define-structure terminfo terminfo-interface
  (open scheme scheme-with-scsh srfi-6 srfi-9 srfi-13 tables)
  (files terminfo terminfo-capabilities))


