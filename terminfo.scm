;;; -*- Mode: Scheme; scheme48-package: terminfo -*-
;;;
;;; terminfo.scm: Terminfo API for SCSH
;;;
;;; Copyright © 2007 Duncan Mak <duncan@ccs.neu.edu>
;;;

(define *current-terminal*     '())
(define *terminfo-directories* '("/usr/share/terminfo"
                                 "/usr/share/misc/terminfo"))

(define-record-type terminal
  (make-terminal names booleans numbers strings)
  terminal?
  (names    terminal:names)
  (booleans terminal:booleans)
  (numbers  terminal:numbers)
  (strings  terminal:strings))

(define (terminfo-filename name)
  (let loop ((dirs *terminfo-directories*))
    (if (not (null? dirs))
        (let* ((basedir (car dirs))
               (initial (string-take name 1))
               (file    (path-list->file-name (list basedir initial name))))
          (cond ((file-not-exists? file)
                 (error "Cannot find terminfo named " name))
                ; TODO: returns only the relative path
                ((file-symlink? file) (read-symlink file))
                ((file-readable? file) file)
                (else (loop (cdr dirs))))))))

(define (read-byte . args)
  (let-optionals args ((s (current-input-port)))
    (let ((value (read-char s)))
      (if (eof-object? value)
          (error "invalid data")
          (char->ascii value)))))

(define (read-short . args)
  (let-optionals args ((s (current-input-port)))
    (let ((n (+ (read-byte s)
                (* 256 (read-byte s)))))
      (if (> n 32767)
          (- n 65536)
          n))))

(define (read-strings-and-split . args)
  (let-optionals args ((s (current-input-port)))
    (let loop ((char   (read-char s))
               (result '()))
      (if (or (eof-object? char) (zero? (char->ascii char)))
          ((infix-splitter "|") (reverse-list->string result))
          (loop (read-char s)
                (cons char result))))))

;;; Supports exactly *one* decimal point
(define (read-number s i)
  (let loop ((j i))
    (let ((char (string-ref s j)))
      (cond
       ((char=? #\. char)
        (values (string->number (substring s i (+ 2 j)))
                (+ 2 j)))
       ((not (char-digit? char))
        (values (string->number (substring s i j))
                (1+ j)))
       (else (loop (1+ j)))))))

(define (read-padding s lines)
  (if (not (and (char=? #\$ (string-ref s 0))
                (char=? #\< (string-ref s 1))))
      (error "Invalid input"))
  (let loop ((time  0)
             (force #f)
             (i     2))
    (case (string-ref s i)
      ((#\>) (values time force))
      ((#\*) (loop (* time lines) force (1+ i)))
      ((#\/) (loop time           #t    (1+ i)))
      (else
       (if (not (zero? time))
           (error s "This is not well-formed.")
           (let ((time j (read-number s i)))
             (loop time force j)))))))

(define (baud-rate port)
  (let* ((info (tty-info port))
         (rate (tty-info:output-speed info)))
    (case rate
      ((exta) 19200)
      ((extb) 38400)
      (else rate))))

(define (tputs s . args)
  (let-optionals args ((lines-affected 1)
                       (output-port (current-output-port)))
    (let loop ((i 0)
               (len (string-length s)))
      (if (< i len)
          (case (string-ref s i)
            ((#\$)
             (let* ((substr (substring s i (1+ (string-index s #\>))))
                    (time
                     forced (read-padding substr lines-affected))
                    (rate   (baud-rate output-port)))
               (if (or force (eq? #t (xon-xoff)))
                   (if (eq? #t (no-pad-char))
                       (sleep (/ time 10000.0))
                       (do ((i 0 (+ i 1))) ((>= i (ceiling (/ (* rate time) 100000))))
                         (write-char (char-padding) output-port)
                         (loop (1+ i) len)))
                   (loop (1+ i) len))))
            (else => (lambda (x)
                       (write-char x)
                       (loop (1+ i) len))))))))

;;;
;;; See Table 7.3, _Unix_Curses_Explained_, p.101
;;;
(define (write-escaped-character c . args)
  (let-optionals args ((output-port (current-output-port)))
    (with-current-output-port output-port
        (case c
          ((#\E) (write (ascii->char 27)))
          ((#\e) (write (ascii->char 27)))
          ((#\n) (newline))
          ((#\l) (write (ascii->char 10)))
          ((#\r) (write (ascii->char 13)))
          ((#\t) (write (ascii->char 9)))
          ((#\b) (write (ascii->char 8)))
          ((#\f) (write (ascii->char 12)))
          ((#\s) (write #\space))
          ((#\,) (write #\,))
          ((#\0) (write (ascii->char 0)))
          ((#\\) (write #\\))
          ((#\^) (write #\^))
          ((#\:) (write #\:))
          (else  (error c "This is an invalid escape character."))))))

(define (write-control-character c . args)
  (let-optionals args ((output-port (current-output-port)))
    (with-current-output-port output-port
        (case c
          ((#\@) (write (ascii->char 0)))   ; Null character
          ((#\[) (write (ascii->char 33)))  ; Escape
          ((#\\) (write (ascii->char 34)))  ; File separator
          ((#\]) (write (ascii->char 35)))  ; Group separator
          ((#\^) (write (ascii->char 36)))  ; Record separator
          ((#\_) (write (ascii->char 37)))  ; Unit separator
          ((#\?) (write (ascii->char 177))) ; Delete
          (else  (write (ascii->char (letter->number c))))))))

(define (write-param-capability s i stack svars dvars params . args)
  (let-optionals args ((output-port (current-output-port)))
    (with-current-output-port output-port
        (case (string-ref s i)
          ((#\%) ; %% -> outputs `%'
           (write-char #\%)
           (values (1+ i) stack svars dvars))

          ((#\c) ; %c -> print pop() like %c in printf
           (let* ((v   (pop stack))
                  (val (if (number? v) (ascii->char v) v)))
             (write-char val)
             (values (1+ i) (cdr stack) svars dvars)))

          ((#\s) ; %s -> print pop() like %s in printf
           (display (pop stack))
           (values (1+ i) (cdr stack) svars dvars))

          ((#\p) ; %p[1-9] -> push i-th parameter
           (let* ((idx   (char->digit (string-ref s (1+ i))))
                  (param (list-ref params (- 1 idx))))
             (values (+ 2 i) (push param stack) svars dvars)))

          ((#\P) ; %P[a-z] -> set variable [a-z] to pop()
           (let* ((c   (string-ref s (1+ i)))
                  (idx (letter->number c))
                  (var (if (char-upper-case? c) svars dvars)))
             (vector-set! var idx (pop stack))
             (values (+ 2 i) (cdr stack) svars dvars)))

          ((#\g) ; %g[a-z] -> get variable [a-z] and push it
           (let* ((c   (string-ref s (1+ i)))
                  (idx (letter->number c))
                  (var (if (char-upper-case? c) svars dvars))
                  (val (vector-ref var idx)))
             (values (+ 2 i) (push val stack) svars dvars)))

          ((#\') ; %'c' -> push char constant c
           (let ((c (string-ref s (1+ i))))
             (values (+ 3 i) (push c stack) svars dvars)))

          ((#\{) ; %{nn} -> push integer constant nn
           (let* ((end (string-index s #\} i))
                  (str (substring s (1+ i) end))
                  (nn  (string->number str)))
             (values (1+ end) (push nn stack) svars dvars)))

          ((#\l) ; %l -> push strlen (pop)
           (let ((val (digit->char (string-length (pop stack)))))
             (values (1+ i) (push val (cdr stack)) svars dvars)))

          ; %op -> push (pop() op pop())
          ((#\+ #\- #\* #\/ #\m #\& #\| #\^ #\= #\> #\<)
           (let* ((c   (string-ref s i))
                  (op  (char->procedure c))
                  (x   (first stack))
                  (y   (second stack))
                  (val (op x y)))
             (values (1+ i) (push val (cddr stack)) svars dvars)))
          ; %i   add 1 to first two parameters (for ANSI terminals)
          ((#\i)
           (let ((incr (lambda (v)
                         (cond
                          ((null? v) v)
                          ((null? (cdr v))
                           (cons (1+ (first v))
                                 (cdr v)))
                          (else
                           (cons (1+ (first v))
                                 (cons (1+ (second v))
                                       (cddr v))))))))
             (values (1+ i) stack svars dvars (incr params))))

          ;; %[[:]flags][width[.precision]][doxXs]
          ;;  as in printf, flags are [-+#] and space
          (else
           (letrec ((char-specifier?
                     (lambda (c)
                       (case c
                         ((#\d #\o #\x #\X #\s) #t)
                         (else #f))))
                    (char-flag?
                     (lambda (c)
                       (case c
                         ((#\+ #\- #\# #\space) #t)
                         (else #f))))
                    (->number
                     (lambda (x)
                       (cond ((char? x) (char->ascii x))
                             ((number? x) x)
                             (else (error x "This is invalid.")))))
                    (incr (lambda (v i) (+ (* 10 v) i)))
                    (print
                     (lambda (flags width precision specifier)
                       (let* ((value  (pop stack))
                              (nvalue (->number value))
                              (svalue ""))
                         (set! svalue
                               (case specifier
                                 ((#\d) (number->string nvalue 10))
                                 ((#\o) (number->string nvalue 8))
                                 ((#\x) (number->string nvalue 16))
                                 ((#\X) (string-upcase
                                         (number->string nvalue 16)))
                                 ((#\s) value)))
                         (if (not (zero? precision))
                             )
                         (case flag
                           ((#\+)
                            (set! svalue
                                  (string-append
                                   (if (positive? nvalue) "+" "-")
                                   svalue)))
                           ((#\-)
                            (set! svalue (string-pad svalue width)))
                           ((#\space)
                            (let ((prefix (string-take svalue)))
                              (if (not (or (char=? prefix #\+)
                                           (char=? prefix #\-)))
                                  (set! svalue (string-append " " svalue)))))
                           ((#\#)
                            (set! svalue
                                  (string-append
                                   (case specifier
                                     ((#\o) "0")
                                     ((#\x) "0x")
                                     ((#\X) "0X")
                                     (else
                                      (error "Not applicable for specifier:") specifier))
                                   svalue))))
                         (display svalue))))
                    (loop
                     (lambda (i flags width precision saw-colon saw-decimal)
                       (let ((c (string-ref s i)))
                         (cond
                          ((char=? c #\:)
                           (loop (1+ i) flags width precision #t saw-decimal))
                          ((and (char-flag? c) saw-colon)
                           (loop (1+ i)
                                 (cons c flags)
                                 width precision saw-colon saw-decimal))
                          ((char=? c #\.)
                           (loop (1+ i) flags width precision saw-colon #t))
                          ((char-digit? c)
                           (loop (1+ i) flags
                                 (if saw-decimal
                                     width
                                     (incr width (char->digit c)))
                                 (if saw-decimal
                                     (incr precision (char->digit c))
                                     precision)
                                 saw-colon
                                 saw-decimal))
                          ((char-specifier? c)
                           (print flags width precision c)
                           (values (1+ i) (cdr stack) svars dvars))
                          (else (error c "This is invalid.")))))))
             (loop i '() 0 0 #f #f)))))))

(define (tparm s . params)
  (with-current-output-port (open-output-string)
      (let loop ((i     0)
                 (stack '())
                 (svars (make-vector 26 0))
                 (dvars (make-vector 26 0))
                 (len   (string-length s)))
        (if (>= i len)
            (get-output-string (current-output-port))
            (case (string-ref s i)
              ((#\\)
               (write-escaped-character (string-ref s (1+ i)))
               (loop (1+ i) stack svars dvars len))
              ((#\^)
               (write-control-character (string-ref s (1+ i)))
               (loop (1+ i) stack svars dvars len))
              ((#\%) (let* ((i
                             stack
                             svars
                             dvars (write-param-capability
                                    s (1+ i) stack svars dvars params)))
                       (loop i stack svars dvars len)))
              (else => (lambda (x)
                         (write-char x)
                         (loop (1+ i) stack svars dvars len))))))))

(define (load-terminfo name)
  (with-input-from-file name
    (lambda ()
      (let* ((magic         (read-short))
             (sznames       (read-short))
             (szbooleans    (read-short))
             (sznumbers     (read-short))
             (szstrings     (read-short))
             (szstringtable (read-short))
             (names         (read-strings-and-split))
             (booleans      (make-vector szbooleans #f))
             (numbers       (make-vector sznumbers -1))
             (strings       (make-vector szstrings -1))
             (stringtable   (make-string szstringtable)))
        (if (not (= magic #o432))
            (error name "This is not well-formed"))
        (do ((i 0 (+ i 1))) ((>= i szbooleans))
          (vector-set! booleans i (not (zero? (read-byte)))))
        (if (odd? (+ sznames szbooleans))
            (read-byte))
        (do ((i 0 (+ i 1))) ((>= i sznumbers))
          (vector-set! numbers i (read-short)))
        (do ((i 0 (+ i 1))) ((>= i szstrings))
          (vector-set! strings i (read-short)))
        (do ((i 0 (+ i 1))) ((>= i szstringtable))
          (string-set! stringtable i (read-char)))
        (do ((i 0 (+ i 1))) ((>= i szstrings))
          (if (positive? (vector-ref strings i))
              (let* ((start  (vector-ref strings i))
                     (end    (string-index stringtable (ascii->char 0)
                                           start szstringtable))
                     (substr (substring stringtable start end)))
                (vector-set! strings i substr))))
        (make-terminal names booleans numbers strings)))))

(define (setup-terminal . args)
  (let-optionals args ((term (getenv "TERM")))
    (let ((filename (terminfo-filename term)))
      (set! *current-terminal* (load-terminfo filename))
      *current-terminal*)))