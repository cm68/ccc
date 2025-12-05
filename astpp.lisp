#!/usr/bin/sbcl --script
;;;; AST Pretty Printer for ccc compiler - paren-free hex format
;;;;
;;;; Format:
;;;;   Names: 2-hex-len + hex-bytes (e.g., 03666f6f = "foo")
;;;;   Numbers: hex digits terminated by '.' (e.g., 1a. = 26)
;;;;   Top-level: F (function), Z (global), U (string)
;;;;   Statements: B (block), I (if), E (expr), R (return), etc.
;;;;   Expressions: operator + width suffix + operands

(defvar *input* nil)
(defvar *pos* 0)
(defvar *indent* 0)

;;; Low-level input
(defun cur ()
  (if (< *pos* (length *input*))
      (char *input* *pos*)
      nil))

(defun advance ()
  (when (< *pos* (length *input*))
    (incf *pos*)))

(defun peek-char-at (n)
  (let ((p (+ *pos* n)))
    (if (< p (length *input*))
        (char *input* p)
        nil)))

(defun skip-whitespace ()
  (loop while (and (cur) (member (cur) '(#\Space #\Tab #\Newline #\Return)))
        do (advance)))

(defun hex-digit-p (c)
  (and c (or (digit-char-p c 16))))

(defun width-char-p (c)
  "Check if character is a type width suffix: b/B (byte), s/S (short), l/L (long), p (ptr), f (float), d (double), v (void)"
  (and c (member c '(#\b #\B #\s #\S #\l #\L #\p #\f #\d #\v))))

(defun digit-operator-p (c)
  "Check if character is one of the digit operators: 0=<<=, 1=|=, 2=/=, 6=>>="
  (and c (member c '(#\0 #\1 #\2 #\6))))

(defun hex-val (c)
  (cond ((char<= #\0 c #\9) (- (char-code c) (char-code #\0)))
        ((char<= #\a c #\f) (+ 10 (- (char-code c) (char-code #\a))))
        ((char<= #\A c #\F) (+ 10 (- (char-code c) (char-code #\A))))
        (t 0)))

;;; Parsing primitives
(defun read-hex2 ()
  "Read 2 hex chars as a byte"
  (let ((h (hex-val (cur))))
    (advance)
    (let ((l (hex-val (cur))))
      (advance)
      (+ (* h 16) l))))

(defun read-num ()
  "Read hex number terminated by '.'"
  (let ((neg nil) (v 0))
    (when (eql (cur) #\-)
      (setf neg t)
      (advance))
    (loop while (and (cur) (not (eql (cur) #\.)))
          do (setf v (+ (* v 16) (hex-val (cur))))
             (advance))
    (when (eql (cur) #\.)
      (advance))
    (if neg (- v) v)))

(defun read-name ()
  "Read hex-length-prefixed name"
  (let* ((len (read-hex2))
         (chars (loop repeat len collect (code-char (read-hex2)))))
    (coerce chars 'string)))

(defun read-str ()
  "Read hex-length-prefixed string"
  (read-name))

;;; Pretty printing helpers
(defun indent-str ()
  (make-string (* *indent* 2) :initial-element #\Space))

(defun pr (fmt &rest args)
  (apply #'format t fmt args))

(defun prln (fmt &rest args)
  (format t "~A" (indent-str))
  (apply #'format t fmt args)
  (terpri))

(defun width-name (c)
  (case c
    (#\b "byte")
    (#\B "ubyte")
    (#\s "short")
    (#\S "ushort")
    (#\l "long")
    (#\L "ulong")
    (#\p "ptr")
    (#\f "float")
    (#\d "double")
    (#\v "void")
    (t (string c))))

(defun op-name (c)
  (case c
    (#\M "DEREF")
    (#\= "ASSIGN")
    (#\+ "ADD")
    (#\- "SUB")
    (#\* "MUL")
    (#\/ "DIV")
    (#\% "MOD")
    (#\& "AND")
    (#\| "OR")
    (#\^ "XOR")
    (#\~ "NOT")
    (#\y "LSHIFT")
    (#\w "RSHIFT")
    (#\< "LT")
    (#\> "GT")
    (#\L "LE")
    (#\g "GE")
    (#\Q "EQ")
    (#\n "NE")
    (#\j "LAND")
    (#\h "LOR")
    (#\! "LNOT")
    (#\N "NARROW")
    (#\W "WIDEN")
    (#\X "SEXT")
    (#\? "TERNARY")
    (#\@ "CALL")
    (#\Y "COPY")
    (#\\ "NEG")
    (#\' "ADDR")
    (#\P "+=")
    (#\T "*=")
    (#\0 "<<=")
    (#\1 "|=")
    (#\2 "/=")
    (#\6 ">>=")
    (t (format nil "~A" c))))

;;; Expression parsing and printing
(defun parse-print-expr ()
  "Parse and print an expression, return string representation"
  (skip-whitespace)
  (let ((c (cur)))
    (cond
      ;; Null expression
      ((eql c #\_)
       (advance)
       "()")

      ;; Symbol reference
      ((eql c #\$)
       (advance)
       (let ((name (read-name)))
         (format nil "$~A" name)))

      ;; Stack offset
      ((eql c #\S)
       (advance)
       (let ((off (read-num)))
         (format nil "SP[~A]" off)))

      ;; Numeric constant (hex digits, or - followed by hex digit)
      ;; BUT NOT operators when followed by a width char:
      ;; - digit operators (0,1,2,6) followed by width
      ;; - minus (-) when followed by width (it's subtraction, not negative number)
      ((or (and (hex-digit-p c)
                (not (and (digit-operator-p c)
                          (width-char-p (peek-char-at 1)))))
           (and (eql c #\-)
                (let ((next (peek-char-at 1)))
                  (and next (hex-digit-p next)
                       (not (width-char-p next))))))
       (let ((v (read-num)))
         (format nil "~A" v)))

      ;; Operator
      (t
       (advance)
       (let ((op-char c))
         ;; Special cases
         (cond
           ;; Call: @ argc. func args...
           ((eql op-char #\@)
            (let* ((argc (read-num))
                   (func (parse-print-expr))
                   (args (loop repeat argc collect (parse-print-expr))))
              (format nil "(CALL ~A~{ ~A~})" func args)))

           ;; Ternary: ? width cond then else
           ((eql op-char #\?)
            (let* ((w (cur))
                   (_ (advance))
                   (cond-e (parse-print-expr))
                   (then-e (parse-print-expr))
                   (else-e (parse-print-expr)))
              (declare (ignore _))
              (format nil "(?:~A ~A ~A ~A)" (width-name w) cond-e then-e else-e)))

           ;; Copy: Y size. dest src
           ((eql op-char #\Y)
            (let* ((sz (read-num))
                   (dst (parse-print-expr))
                   (src (parse-print-expr)))
              (format nil "(COPY:~A ~A ~A)" sz dst src)))

           ;; Inc/Dec: op width expr delta.
           ((member (char-code op-char) '(#xcf #xef #xd6 #xf6))
            (let* ((w (cur))
                   (_ (advance))
                   (e (parse-print-expr))
                   (delta (read-num))
                   (op-name (case (char-code op-char)
                              (#xcf "PREINC")
                              (#xef "POSTINC")
                              (#xd6 "PREDEC")
                              (#xf6 "POSTDEC"))))
              (declare (ignore _))
              (format nil "(~A:~A ~A ~A)" op-name (width-name w) e delta)))

           ;; Bitfield extract: 0xa7 off. wid. expr
           ((eql (char-code op-char) #xa7)
            (let* ((off (read-num))
                   (wid (read-num))
                   (e (parse-print-expr)))
              (format nil "(BFEXT ~A:~A ~A)" off wid e)))

           ;; Bitfield assign: 0xdd off. wid. dest val
           ((eql (char-code op-char) #xdd)
            (let* ((off (read-num))
                   (wid (read-num))
                   (dst (parse-print-expr))
                   (val (parse-print-expr)))
              (format nil "(BFSET ~A:~A ~A ~A)" off wid dst val)))

           ;; Regular operator with width suffix
           (t
            (let* ((w (cur))
                   (_ (advance))
                   (arity (op-arity op-char)))
              (declare (ignore _))
              (case arity
                (1 (let ((e1 (parse-print-expr)))
                     (format nil "(~A:~A ~A)" (op-name op-char) (width-name w) e1)))
                (2 (let ((e1 (parse-print-expr))
                         (e2 (parse-print-expr)))
                     (format nil "(~A:~A ~A ~A)" (op-name op-char) (width-name w) e1 e2)))
                (t (format nil "(~A:~A ???)" (op-name op-char) (width-name w))))))))))))

(defun op-arity (c)
  "Return arity of operator"
  (cond
    ;; Unary
    ((member c '(#\M #\N #\W #\! #\~ #\\ #\')) 1)
    ((eql (char-code c) #xab) 1)  ; sign extend
    ;; Binary
    ((member c '(#\= #\+ #\- #\* #\/ #\% #\& #\| #\^ #\< #\> #\Q #\n #\L #\g #\y #\w #\: #\h #\j)) 2)
    ((member c '(#\P #\T #\2 #\1 #\X #\0 #\6)) 2)
    ((member (char-code c) '(#xdf #xfe #xc6)) 2)
    ;; Default to binary
    (t 2)))

;;; Statement parsing and printing
(defun parse-print-stmt ()
  "Parse and print a statement"
  (skip-whitespace)
  (let ((c (cur)))
    (when (null c) (return-from parse-print-stmt))
    (advance)
    (case c
      ;; Block: B decl_count. stmt_count. decls... stmts...
      (#\B
       (let* ((decl-count (read-num))
              (stmt-count (read-num)))
         (prln "BLOCK {")
         (incf *indent*)
         ;; Declarations
         (dotimes (i decl-count)
           (skip-whitespace)
           (when (eql (cur) #\d)
             (advance)
             (let* ((type-char (cur))
                    (_ (advance))
                    (name (read-name)))
               (declare (ignore _))
               (prln "DECL ~A : ~A" name (width-name type-char)))))
         ;; Statements
         (dotimes (i stmt-count)
           (parse-print-stmt))
         (decf *indent*)
         (prln "}")))

      ;; If: I has_else. cond then [else]
      (#\I
       (let* ((has-else (read-num))
              (cond-e (parse-print-expr)))
         (prln "IF (~A)" cond-e)
         (incf *indent*)
         (parse-print-stmt)
         (decf *indent*)
         (when (= has-else 1)
           (prln "ELSE")
           (incf *indent*)
           (parse-print-stmt)
           (decf *indent*))))

      ;; Expression: E expr
      (#\E
       (let ((e (parse-print-expr)))
         (prln "EXPR ~A" e)))

      ;; Return: R has_value. [expr]
      (#\R
       (let ((has-val (read-num)))
         (if (= has-val 1)
             (prln "RETURN ~A" (parse-print-expr))
             (prln "RETURN"))))

      ;; Label: L hexname
      (#\L
       (let ((name (read-name)))
         (prln "LABEL ~A:" name)))

      ;; Goto: G hexname
      (#\G
       (let ((name (read-name)))
         (prln "GOTO ~A" name)))

      ;; Switch: S has_label. [hexlabel] case_count. expr cases...
      (#\S
       (let* ((has-label (read-num))
              (label (when (= has-label 1) (read-name)))
              (case-count (read-num))
              (expr (parse-print-expr)))
         (if label
             (prln "SWITCH [~A] (~A) {" label expr)
             (prln "SWITCH (~A) {" expr))
         (incf *indent*)
         (dotimes (i case-count)
           (parse-print-stmt))
         (decf *indent*)
         (prln "}")))

      ;; Case: C stmt_count. value stmts...
      (#\C
       (let* ((stmt-count (read-num))
              (value (parse-print-expr)))
         (prln "CASE ~A:" value)
         (incf *indent*)
         (dotimes (i stmt-count)
           (parse-print-stmt))
         (decf *indent*)))

      ;; Default: O stmt_count. stmts...
      (#\O
       (let ((stmt-count (read-num)))
         (prln "DEFAULT:")
         (incf *indent*)
         (dotimes (i stmt-count)
           (parse-print-stmt))
         (decf *indent*)))

      ;; Asm: A len hexdata
      (#\A
       (let ((asm-str (read-str)))
         (prln "ASM { ~A }" asm-str)))

      ;; Empty statement
      (#\;
       (prln ";"))

      ;; Break
      (#\K
       (prln "BREAK"))

      ;; Continue
      (#\N
       (prln "CONTINUE"))

      ;; While (unlabeled)
      (#\W
       (let ((cond-e (parse-print-expr)))
         (prln "WHILE (~A)" cond-e)
         (incf *indent*)
         (parse-print-stmt)
         (decf *indent*)))

      ;; Do (unlabeled)
      (#\D
       (prln "DO")
       (incf *indent*)
       (parse-print-stmt)
       (decf *indent*)
       (let ((cond-e (parse-print-expr)))
         (prln "WHILE (~A)" cond-e)))

      ;; For (unlabeled)
      (#\F
       (let ((init (parse-print-expr))
             (cond-e (parse-print-expr))
             (incr (parse-print-expr)))
         (prln "FOR (~A; ~A; ~A)" init cond-e incr)
         (incf *indent*)
         (parse-print-stmt)
         (decf *indent*)))

      (t
       (prln "??? ~A" c)))))

;;; Top-level parsing
(defun parse-print-function ()
  "Parse and print function: F rettype hexname param_count. params... body"
  (let* ((ret-type (cur))
         (_ (advance))
         (name (read-name))
         (param-count (read-num))
         (params nil))
    (declare (ignore _))
    ;; Read parameters
    (dotimes (i param-count)
      (skip-whitespace)
      (when (eql (cur) #\d)
        (advance)
        (let* ((ptype (cur))
               (_ (advance))
               (pname (read-name)))
          (declare (ignore _))
          (push (cons pname ptype) params))))
    (setf params (nreverse params))
    ;; Print function header
    (format t "~%FUNCTION ~A(" name)
    (loop for (pname . ptype) in params
          for i from 0
          do (when (> i 0) (format t ", "))
             (format t "~A:~A" pname (width-name ptype)))
    (format t ") -> ~A~%" (width-name ret-type))
    (format t "{~%")
    ;; Parse body
    (let ((*indent* 1))
      (skip-whitespace)
      (parse-print-stmt))
    (format t "}~%")))

(defun parse-print-global ()
  "Parse and print global: Z $hexname type has_init. [init]"
  (skip-whitespace)
  (when (eql (cur) #\$) (advance))
  (let* ((name (read-name))
         (type-char (cur))
         (_ (advance)))
    (declare (ignore _))
    (cond
      ;; Array: a count. elemsize. has_init. [init]
      ((eql type-char #\a)
       (let* ((count (read-num))
              (elemsize (read-num))
              (has-init (read-num)))
         (format t "GLOBAL ~A : array[~A] of ~A-byte~%" name count elemsize)
         (when (= has-init 1)
           ;; Skip initializer - it starts with '['
           (skip-whitespace)
           (when (eql (cur) #\[)
             (advance)
             (let ((elem-type (cur)))
               (declare (ignore elem-type))
               (advance)
               (let ((init-count (read-num))
                     (printed-ellipsis nil))
                 (format t "  = { ")
                 (dotimes (i init-count)
                   (let ((val (read-num)))
                     (cond
                       ((< i 10)
                        (when (> i 0) (format t ", "))
                        (format t "~A" val))
                       ((not printed-ellipsis)
                        (format t ", ...")
                        (setf printed-ellipsis t)))))
                 (format t " }~%")))))))

      ;; Struct: r size. has_init. [init]
      ((eql type-char #\r)
       (let* ((size (read-num))
              (has-init (read-num)))
         (format t "GLOBAL ~A : struct[~A]~%" name size)
         (when (= has-init 1)
           (parse-print-expr))))

      ;; Pointer or primitive: type has_init. [init]
      (t
       (let ((has-init (read-num)))
         (format t "GLOBAL ~A : ~A" name (width-name type-char))
         (when (= has-init 1)
           (format t " = ~A" (parse-print-expr)))
         (terpri))))))

(defun parse-print-string ()
  "Parse and print string literal: U hexname hexdata"
  (let* ((name (read-name))
         (data (read-str)))
    (format t "STRING _~A = ~S~%" name data)))

(defun parse-print-toplevel ()
  "Parse and print one top-level item"
  (skip-whitespace)
  (let ((c (cur)))
    (when (null c) (return-from parse-print-toplevel nil))
    (advance)
    (case c
      (#\F (parse-print-function))
      (#\Z (parse-print-global))
      (#\U (parse-print-string))
      (#\Newline nil)  ; skip blank lines
      (t (format t "??? top-level: ~A~%" c)))
    t))

(defun parse-print-ast ()
  "Parse and print entire AST"
  (format t "========================================~%")
  (format t "AST Pretty Printer Output~%")
  (format t "========================================~%")
  (loop while (parse-print-toplevel))
  (format t "~%========================================~%"))

;;; Main entry point
(defun main ()
  (let* ((args (cdr sb-ext:*posix-argv*))
         (filename (first args)))
    (setf *input*
          (if filename
              (with-open-file (s filename :external-format :latin-1)
                (let ((content (make-string (file-length s))))
                  (read-sequence content s)
                  content))
              (with-output-to-string (str)
                (loop for line = (read-line *standard-input* nil nil)
                      while line
                      do (write-line line str)))))
    (setf *pos* 0)
    (parse-print-ast)))

(main)
