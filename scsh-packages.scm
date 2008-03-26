;;; -*- Mode: Scheme; scheme48-package: (config) -*-

(define-structure terminfo terminfo-interface
  (open
   (modify ascii (rename (ascii->char integer->char)
                         (char->ascii char->integer)))
   srfi-1 srfi-6 srfi-9 srfi-11 srfi-13 srfi-14
   (modify scheme (hide char->integer integer->char ))
   error-package
   field-reader-package
   scsh-level-0
   i/o let-opt tables threads)
  (files terminfo
         terminfo-capabilities
         utilities))
