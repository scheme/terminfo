(define-interface terminfo-core-interface 
  (export *terminfo* *terminfo-directories* set-terminal))

(define-interface terminfo-capabilities-interface
  (export terminfo-capability))

(define-interface terminfo-interface
  (compound-interface terminfo-core-interface terminfo-capabilities-interface))

(define-structure terminfo-core terminfo-core-interface
  (open scheme-with-scsh
        srfi-6 srfi-9 srfi-13 srfi-16)
  (files terminfo))

(define-structure terminfo-capabilities terminfo-capabilities-interface
  (open scheme-with-scsh tables terminfo-core)
  (files terminfo-capabilities))

(define-structure terminfo terminfo-interface
  (open terminfo-core terminfo-capabilities))

