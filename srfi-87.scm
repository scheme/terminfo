(define-syntax srfi-case
  (syntax-rules (else =>)
    ((srfi-case (key ...)
       clauses ...)
     (let ((atom-key (key ...)))
       (srfi-case atom-key clauses ...)))
    ((srfi-case key
       (else => result))
     (result key))
    ((srfi-case key
       ((atoms ...) => result))
     (if (memv key '(atoms ...))
         (result key)))
    ((srfi-case key
       ((atoms ...) => result)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (result key)
         (srfi-case key clause clauses ...)))
    ((srfi-case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((srfi-case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))
    ((srfi-case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (srfi-case key clause clauses ...)))))