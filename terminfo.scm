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
      (if (<= i len)
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
            (else (write-char (string-ref s i))
                  (loop (1+ i) len)))))))

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

(define (letter-position c)
  (if (char-alphabetic? c)
      (1+ (- (char->ascii (char-upcase c)) (char->ascii #\A)))
      (error c "This is not a letter")))

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
          (else  (write (ascii->char (letter-position c))))))))

(define (tparm s . args)
  (with-current-output-port (open-output-string)
      (let loop ((i   0)
                 (len (string-length s)))
        (if (>= i len)
            (get-output-string (current-output-port))
            (begin
              (case (string-ref s i)
                ((#\\) (write-escaped-character (string-ref s (1+ i))))
                ((#\^) (write-control-character (string-ref s (1+ i))))
                ((#\%) (write-param-capability (substring s (1+ i) len) args))
                (else  (write-char (string-ref s i))))
              (loop (1+ i) len))))))

(define (write-param-capability s . args) #t)
;; (define (write-parameterized-capability args)
;;   (if (not (char=? #\% (read-char))) (error "Invalid string"))
;;   (letrec ((loop (lambda (c stk)
;;                    (case c
;;                      ((#\%) (write #\%)
;;                       (loop (read-char) stk))
;;                      ((#\+ #\- #\* #\% #\m)
;;                       (loop (read-char) (arithmatic c stk)))
;;                      ((#\{)
;;                       (loop (read-char) (push-integer-constant c stk)))
;;                      ((#\l)
;;                       (loop (read-char) (pop-string-addr+len c stk)))
;;                      ((#\i)
;;                       )

;; )))

;;            (arithmatic (lambda (c stk)
;;                          ()))
;;            (push-integer-constant (lambda (c stk)
;;                                )))
;;     (loop (read-char) '()))
;;   )

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
              (let* ((start     (vector-ref strings i))
                     (end       (string-index stringtable (ascii->char 0)
                                              start szstringtable))
                     (new-value (substring stringtable start end)))
                (vector-set! strings i new-value))))
        (make-terminal names booleans numbers strings)))))

(define (setup-terminal . args)
  (let-optionals args ((term (getenv "TERM")))
    (let ((filename (terminfo-filename term)))
      (set! *current-terminal* (load-terminfo filename))
      *current-terminal*)))