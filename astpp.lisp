#!/usr/bin/sbcl --script
;;;; AST Pretty Printer for ccc compiler output
;;;; Formats S-expression AST in human-readable form

(defvar *indent-level* 0)
(defvar *indent-string* "  ")
(defvar *raw-mode* nil)  ; When true, don't translate operators

(defun indent ()
  "Return current indentation string"
  (make-string (* *indent-level* (length *indent-string*))
               :initial-element #\Space))

(defun parse-width-annotation (sym)
  "Parse a symbol with width annotation (e.g., M_s) into (operator . width)"
  (let ((name (if (symbolp sym) (symbol-name sym) (string sym))))
    (let ((underscore-pos (position #\_ name :from-end t)))
      (if (and underscore-pos (> (length name) 2))
          (cons (intern (subseq name 0 underscore-pos))
                (intern (subseq name (1+ underscore-pos))))
          (cons (intern name) nil)))))

(defun width-name (width)
  "Get human-readable name for width annotation"
  (when width
    (let ((width-str (string width)))
      (cond
        ((string= width-str "b") "byte")
        ((string= width-str "s") "short")
        ((string= width-str "l") "long")
        ((string= width-str "p") "ptr")
        ((string= width-str "f") "float")
        ((string= width-str "d") "double")
        (t width-str)))))

(defun operator-name (op)
  "Get human-readable name for operator (or raw operator if *raw-mode*)"
  (let ((op-str (string op)))
    (if *raw-mode*
        op-str
        (cond
          ((string= op-str "M") "DEREF")
          ((string= op-str "=") "ASSIGN")
          ((string= op-str "+") "ADD")
          ((string= op-str "-") "SUB")
          ((string= op-str "*") "MUL")
          ((string= op-str "/") "DIV")
          ((string= op-str "%") "MOD")
          ((string= op-str "&") "AND")
          ((string= op-str "|") "OR")
          ((string= op-str "^") "XOR")
          ((string= op-str "~") "NOT")
          ((string= op-str "y") "LSHIFT")
          ((string= op-str "w") "RSHIFT")
          ((string= op-str "<") "LT")
          ((string= op-str ">") "GT")
          ((string= op-str "L") "LE")
          ((string= op-str "g") "GE")
          ((string= op-str "Q") "EQ")
          ((string= op-str "n") "NE")
          ((string= op-str "j") "LAND")
          ((string= op-str "h") "LOR")
          ((string= op-str "!") "LNOT")
          ((string= op-str "N") "NARROW")
          ((string= op-str "«") "SEXT")
          ((string= op-str "W") "WIDEN")
          ((string= op-str "X") "XOREQ")
          ((string= op-str "?") "TERNARY")
          ((string= op-str "@") "CALL")
          ((string= op-str "NEG") "NEGATE")
          ;; Increment/decrement operators
          ((string= op-str "Ï") "PREINC")
          ((string= op-str "ï") "POSTINC")
          ((string= op-str "Ö") "PREDEC")
          ((string= op-str "ö") "POSTDEC")
          (t op-str)))))

(defun statement-name (stmt)
  "Get human-readable name for statement type (or raw statement if *raw-mode*)"
  (let ((stmt-str (string stmt)))
    (if *raw-mode*
        stmt-str
        (cond
          ((string= stmt-str "B") "BLOCK")
          ((string= stmt-str "E") "EXPR")
          ((string= stmt-str "R") "RETURN")
          ((string= stmt-str "I") "IF")
          ((string= stmt-str "W") "WHILE")
          ((string= stmt-str "F") "FOR")
          ((string= stmt-str "D") "DO-WHILE")
          ((string= stmt-str "S") "SWITCH")
          ((string= stmt-str "C") "CASE")
          ((string= stmt-str "O") "DEFAULT")
          ((string= stmt-str "K") "BREAK")
          ((string= stmt-str "N") "CONTINUE")
          ((string= stmt-str "G") "GOTO")
          ((string= stmt-str "L") "LABEL")
          ((string= stmt-str "d") "DECL")
          (t stmt-str)))))

(defun pp-expr (expr &optional (inline nil))
  "Pretty print an expression"
  (cond
    ;; Numbers
    ((numberp expr)
     (format nil "~A" expr))

    ;; Symbols (variables)
    ((symbolp expr)
     (format nil "~A" expr))

    ;; Lists (operators)
    ((listp expr)
     (let* ((op-with-width (first expr))
            (parsed (parse-width-annotation op-with-width))
            (op (car parsed))
            (width (cdr parsed))
            (op-name (operator-name op))
            (width-str (if width (format nil ":~A" (width-name width)) "")))

       (case (length expr)
         ;; Nullary/special
         (1 (format nil "(~A)" op))

         ;; Unary
         (2 (if inline
                (format nil "(~A~A ~A)" op-name width-str (pp-expr (second expr) t))
                (format nil "(~A~A~%~A  ~A)"
                        op-name width-str
                        (indent)
                        (pp-expr (second expr) t))))

         ;; Binary
         (3 (if inline
                (format nil "(~A~A ~A ~A)"
                        op-name width-str
                        (pp-expr (second expr) t)
                        (pp-expr (third expr) t))
                (format nil "(~A~A~%~A  ~A~%~A  ~A)"
                        op-name width-str
                        (indent) (pp-expr (second expr) t)
                        (indent) (pp-expr (third expr) t))))

         ;; Ternary or function call
         (otherwise
          (if (string= (string op) "@")
              ;; Function call
              (format nil "(CALL ~A~{ ~A~})"
                      (pp-expr (second expr) t)
                      (mapcar (lambda (arg) (pp-expr arg t)) (cddr expr)))
              ;; Other multi-arg
              (format nil "(~A~A~{ ~A~})"
                      op-name width-str
                      (mapcar (lambda (arg) (pp-expr arg t)) (rest expr))))))))

    (t (format nil "~A" expr))))

(defun pp-statement (stmt depth)
  "Pretty print a statement with given depth"
  (let ((*indent-level* depth))
    (cond
      ;; Null statement
      ((null stmt)
       (format t "~A(NULL)~%" (indent)))

      ;; Not a list
      ((not (listp stmt))
       (format t "~A~A~%" (indent) stmt))

      ;; List - check statement type
      (t
       (let* ((stmt-type (first stmt))
              (stmt-name (statement-name stmt-type)))

         (cond
           ;; Expression statement
           ((or (eq stmt-type '|E|) (eq stmt-type 'E))
            (format t "~A~A:~%" (indent) stmt-name)
            (let ((*indent-level* (1+ depth)))
              (format t "~A~A~%" (indent) (pp-expr (second stmt)))))

           ;; Return statement
           ((or (eq stmt-type '|R|) (eq stmt-type 'R))
            (if (second stmt)
                (format t "~A~A ~A~%" (indent) stmt-name (pp-expr (second stmt) t))
                (format t "~A~A~%" (indent) stmt-name)))

           ;; Block statement
           ((or (eq stmt-type '|B|) (eq stmt-type 'B))
            (format t "~A~A {~%" (indent) stmt-name)
            (dolist (s (rest stmt))
              (pp-statement s (1+ depth)))
            (format t "~A}~%" (indent)))

           ;; Declaration - new format: (d:type name) -> (d_type name) after preprocessing
           ((or (eq stmt-type '|d|) (eq stmt-type 'd)
                (let ((tag-str (symbol-name stmt-type)))
                  (and (> (length tag-str) 1)
                       (char= (char tag-str 0) #\d)
                       (char= (char tag-str 1) #\_))))
            (let* ((parsed (parse-decl-tag stmt-type))
                   (type (cdr parsed))
                   (name (second stmt)))
              (if type
                  ;; New format: type in tag
                  (format t "~ADECL ~A : ~A~%" (indent) name type)
                  ;; Old format: (d name type)
                  (format t "~ADECL ~A : ~A~%" (indent) (second stmt) (third stmt)))))

           ;; If statement
           ((or (eq stmt-type '|I|) (eq stmt-type 'I))
            (format t "~A~A (~A)~%" (indent) stmt-name (pp-expr (second stmt) t))
            (pp-statement (third stmt) (1+ depth))
            (when (fourth stmt)
              (format t "~AELSE~%" (indent))
              (pp-statement (fourth stmt) (1+ depth))))

           ;; While loop
           ((or (eq stmt-type '|W|) (eq stmt-type 'W))
            (format t "~A~A (~A)~%" (indent) stmt-name (pp-expr (second stmt) t))
            (pp-statement (third stmt) (1+ depth)))

           ;; For loop
           ((or (eq stmt-type '|F|) (eq stmt-type 'F))
            (format t "~A~A (init: ~A; cond: ~A; incr: ~A)~%"
                    (indent) stmt-name
                    (pp-expr (second stmt) t)
                    (if (third stmt) (pp-expr (third stmt) t) "")
                    (if (fourth stmt) (pp-expr (fourth stmt) t) ""))
            (pp-statement (fifth stmt) (1+ depth)))

           ;; Do-while loop
           ((or (eq stmt-type '|D|) (eq stmt-type 'D))
            (format t "~A~A~%" (indent) stmt-name)
            (pp-statement (second stmt) (1+ depth))
            (format t "~AWHILE (~A)~%" (indent) (pp-expr (third stmt) t)))

           ;; Switch statement
           ((or (eq stmt-type '|S|) (eq stmt-type 'S))
            (format t "~A~A (~A) {~%" (indent) stmt-name (pp-expr (second stmt) t))
            (dolist (item (cddr stmt))
              (pp-statement item (1+ depth)))
            (format t "~A}~%" (indent)))

           ;; Case label
           ((or (eq stmt-type '|C|) (eq stmt-type 'C))
            (format t "~A~A ~A:~%" (indent) stmt-name (pp-expr (second stmt) t)))

           ;; Default label
           ((or (eq stmt-type '|O|) (eq stmt-type 'O))
            (format t "~A~A:~%" (indent) stmt-name))

           ;; Break
           ((or (eq stmt-type '|K|) (eq stmt-type 'K))
            (format t "~A~A~%" (indent) stmt-name))

           ;; Continue
           ((or (eq stmt-type '|N|) (eq stmt-type 'N))
            (format t "~A~A~%" (indent) stmt-name))

           ;; Goto
           ((or (eq stmt-type '|G|) (eq stmt-type 'G))
            (format t "~A~A ~A~%" (indent) stmt-name (second stmt)))

           ;; Label
           ((or (eq stmt-type '|L|) (eq stmt-type 'L))
            (format t "~A~A ~A:~%" (indent) stmt-name (second stmt)))

           ;; Unknown
           (t
            (format t "~A??? ~A~%" (indent) stmt))))))))

(defun parse-func-tag (tag)
  "Parse function tag like f_v into (f . v) for return type"
  (let ((name (if (symbolp tag) (symbol-name tag) (string tag))))
    (let ((underscore-pos (position #\_ name)))
      (if underscore-pos
          (cons (subseq name 0 underscore-pos)
                (subseq name (1+ underscore-pos)))
          (cons name nil)))))

(defun parse-decl-tag (tag)
  "Parse declaration tag like d_b into (d . b) for type"
  (let ((name (if (symbolp tag) (symbol-name tag) (string tag))))
    (let ((underscore-pos (position #\_ name)))
      (if underscore-pos
          (cons (subseq name 0 underscore-pos)
                (subseq name (1+ underscore-pos)))
          (cons name nil)))))

(defun pp-function (func-def return-type)
  "Pretty print a function definition.
   func-def is (name (params...) body) where params are (d:type name) forms.
   return-type is extracted from f:type tag."
  (let* ((name (first func-def))
         (params (second func-def))
         (body (third func-def)))

    (format t "~%FUNCTION ~A(" name)
    ;; Print parameters - now in (d:type name) format
    (loop for param in params
          for i from 0
          do (when (> i 0) (format t ", "))
             (if (and (listp param)
                      (let ((tag (symbol-name (first param))))
                        (and (> (length tag) 1)
                             (char= (char tag 0) #\d)
                             (char= (char tag 1) #\_))))
                 ;; New format: (d_type name)
                 (let* ((parsed (parse-decl-tag (first param)))
                        (type (cdr parsed))
                        (pname (second param)))
                   (format t "~A : ~A" pname type))
                 ;; Old format or unknown
                 (format t "~A" param)))
    (format t ") -> ~A~%" (or return-type "?"))
    (format t "{~%")

    ;; Print body - declarations are now inline in the block
    (when body
      (pp-statement body 1))

    (format t "}~%")))

(defun pp-global (name type init)
  "Pretty print a global variable"
  (if init
      (format t "GLOBAL ~A : ~A = ~A~%" name type (pp-expr init t))
      (format t "GLOBAL ~A : ~A~%" name type)))

(defun pp-string (name value)
  "Pretty print a string literal"
  (format t "STRING ~A = ~S~%" name value))

(defun symbol-name-only (sym)
  "Extract just the name part of a symbol, ignoring package prefix"
  (let ((name (if (symbolp sym) (symbol-name sym) (string sym))))
    (let ((colon-pos (position #\: name :from-end t)))
      (if colon-pos
          (subseq name (1+ colon-pos))
          name))))

;;; Raw mode - output valid S-expressions
(defun pp-sexp (sexp depth)
  "Pretty print S-expression with indentation (valid AST output)"
  (cond
    ;; Empty list - must check before atom since NIL is both
    ((null sexp)
     (format t "()"))

    ;; String - print with quotes
    ((stringp sexp)
     (format t "~S" sexp))

    ;; Other atoms (number, symbol)
    ((atom sexp)
     (format t "~A" sexp))

    ;; List
    (t
     (let ((first-elem (first sexp)))
       ;; Check if this is a comment
       (when (and (symbolp first-elem)
                  (char= (char (symbol-name first-elem) 0) #\;))
         (format t "~%; ~A~%" (subseq (symbol-name first-elem) 1))
         (return-from pp-sexp))

       ;; Regular list
       (format t "(~A" first-elem)
       (dolist (elem (rest sexp))
         (cond
           ;; If element is empty list, print () - must check before atom
           ((null elem)
            (format t " ()"))

           ;; If element is a string, print with quotes
           ((stringp elem)
            (format t " ~S" elem))

           ;; If element is a simple atom, print on same line
           ((atom elem)
            (format t " ~A" elem))

           ;; If element is a small list, print on same line
           ((and (listp elem) (< (length elem) 4) (every #'atom elem))
            (format t " ")
            (pp-sexp elem depth))

           ;; Otherwise, put on new line with indentation
           (t
            (format t "~%~A  " (make-string (* depth 2) :initial-element #\Space))
            (pp-sexp elem (1+ depth)))))
       (format t ")")))))

(defun pp-ast-raw (ast)
  "Pretty print AST in raw mode (valid S-expressions)"
  (dolist (item ast)
    (cond
      ;; Handle comments
      ((and (listp item) (symbolp (first item))
            (> (length (symbol-name (first item))) 0)
            (char= (char (symbol-name (first item)) 0) #\;))
       (format t "~%~A~%" (symbol-name (first item))))

      ;; Regular AST nodes
      ((listp item)
       (format t "~%")
       (pp-sexp item 0)
       (format t "~%"))

      ;; Atoms (shouldn't happen at top level, but handle it)
      (t
       (format t "~A~%" item))))
  (format t "~%"))

(defun pp-ast (ast)
  "Pretty print complete AST"
  (if *raw-mode*
      (pp-ast-raw ast)
      (progn
        (format t "========================================~%")
        (format t "AST Pretty Printer Output~%")
        (format t "========================================~%")

        (dolist (item ast)
    (when (listp item)
      (let ((tag (first item)))
        (cond
          ;; Global variable (Z <name> <type> [<init>])
          ((or (eq tag 'Z) (eq tag '|Z|))
           (pp-global (symbol-name-only (second item))
                     (third item)
                     (fourth item)))

          ;; String literal (s <name> "<value>")
          ((or (eq tag 's) (eq tag '|s|))
           (pp-string (symbol-name-only (second item))
                     (third item)))

          ;; Literals section (L ...)
          ((or (eq tag '|L|) (eq tag 'L))
           (format t "~%STRING LITERALS:~%")
           (dolist (lit (rest item))
             (when (and (listp lit)
                       (or (eq (first lit) 's) (eq (first lit) '|s|)))
               (pp-string (symbol-name-only (second lit))
                         (third lit)))))

          ;; Function definition (f:type <name> <params> <body>)
          ;; Tag is f_type after preprocessing (colon -> underscore)
          ((or (eq tag 'f) (eq tag '|f|)
               (let ((tag-str (symbol-name tag)))
                 (and (> (length tag-str) 1)
                      (char= (char tag-str 0) #\f)
                      (char= (char tag-str 1) #\_))))
           (let* ((parsed (parse-func-tag tag))
                  (return-type (cdr parsed)))
             (pp-function (rest item) return-type)))))))))) ; cond, let, when, dolist, progn, if, defun

(defun setup-reader ()
  "Configure the Lisp reader to handle AST symbols"
  (setf (readtable-case *readtable*) :preserve))

(defun preprocess-ast-text (text)
  "Preprocess AST text, handling special characters"
  ;; Escape special characters for the Lisp reader
  ;; C and Lisp use similar string escape conventions, so we mainly need to
  ;; handle vertical bars which have special meaning in Lisp
  (setf text (loop with result = (make-array (length text) :element-type 'character
                                              :fill-pointer 0 :adjustable t)
                   with in-string = nil
                   with prev-ch = nil
                   for i from 0 below (length text)
                   for ch = (char text i)
                   do (cond
                        ;; Track string state - quote not preceded by backslash
                        ((and (char= ch #\")
                              (not (and prev-ch (char= prev-ch #\\))))
                         (vector-push-extend ch result)
                         (setf in-string (not in-string)))
                        ;; Vertical bar outside string - escape it for Lisp reader
                        ((and (not in-string) (char= ch #\|))
                         (vector-push-extend #\\ result)
                         (vector-push-extend ch result))
                        ;; Comma outside string - escape it for Lisp reader
                        ;; (comma is the C comma operator in AST, but Lisp unquote operator)
                        ((and (not in-string) (char= ch #\,))
                         (vector-push-extend #\\ result)
                         (vector-push-extend ch result))
                        ;; Semicolon outside string - escape it for Lisp reader
                        ;; (semicolon is a statement in AST, but Lisp comment starter)
                        ((and (not in-string) (char= ch #\;))
                         (vector-push-extend #\\ result)
                         (vector-push-extend ch result))
                        ;; Backslash outside string - double it to prevent escaping next char
                        ;; (backslash is the NEGATE operator in AST, but Lisp escape char)
                        ((and (not in-string) (char= ch #\\))
                         (vector-push-extend #\\ result)
                         (vector-push-extend ch result))
                        ;; Everything else - pass through as-is
                        ;; C and Lisp escape sequences are compatible (\n, \\, \", etc.)
                        (t
                         (vector-push-extend ch result)))
                      (setf prev-ch ch)
                   finally (return result)))

  ;; Replace conversion operators with type suffix (W:s -> W_s, N:b -> N_b, X:l -> X_l)
  ;; These are widen, narrow, and sign-extend with target type annotation
  ;; Must be done before the blanket colon replacement below
  (dolist (prefix '("W" "N" "X"))
    (let ((old (concatenate 'string prefix ":"))
          (new (concatenate 'string prefix "_")))
      (loop for pos = (search old text)
            while pos
            do (setf text (concatenate 'string
                                       (subseq text 0 pos)
                                       new
                                       (subseq text (+ pos 2)))))))

  ;; Replace remaining colons with underscores (for width annotations like =:s -> =_s)
  (setf text (substitute #\_ #\: text))

  ;; Read all forms
  (with-input-from-string (stream text)
    (loop for form = (read stream nil :eof)
          until (eq form :eof)
          collect form)))

(defun preprocess-ast-file (filename)
  "Read and preprocess AST file, handling special characters"
  (let ((text (with-open-file (stream filename :external-format :latin-1)
                (let ((content (make-string (file-length stream))))
                  (read-sequence content stream)
                  content))))
    (preprocess-ast-text text)))

(defun preprocess-ast-stdin ()
  "Read and preprocess AST from stdin"
  ;; Read stdin as Latin-1 by wrapping it in a new stream with correct encoding
  ;; We can't change stdin's encoding, but we can create a new stream from it
  (let ((text (with-output-to-string (str)
                ;; Read all bytes from stdin
                (let ((bytes (make-array 0 :element-type '(unsigned-byte 8)
                                          :adjustable t :fill-pointer 0)))
                  (loop for byte = (read-byte *standard-input* nil nil)
                        while byte
                        do (vector-push-extend byte bytes))
                  ;; Convert bytes to string using Latin-1 encoding
                  (write-string (sb-ext:octets-to-string bytes :external-format :latin-1) str)))))
    (preprocess-ast-text text)))

(defun usage ()
  "Print usage message"
  (format t "Usage: astpp.lisp [options] [<ast-file>]~%")
  (format t "~%")
  (format t "Pretty print ccc compiler AST in human-readable format.~%")
  (format t "If no file is specified, reads from stdin (acts as a filter).~%")
  (format t "~%")
  (format t "Options:~%")
  (format t "  -r, --raw       Raw mode: keep original operators (M, =, +, etc.)~%")
  (format t "                  Only reindent, don't translate to readable names~%")
  (format t "  -h, --help      Show this help message~%")
  (format t "~%")
  (format t "Examples:~%")
  (format t "  # Read from file:~%")
  (format t "  ./cc1 -E program.c > program.ast~%")
  (format t "  ./astpp.lisp program.ast~%")
  (format t "~%")
  (format t "  # Use as filter (read from stdin):~%")
  (format t "  ./cc1 -E program.c | ./astpp.lisp~%")
  (format t "  ./cc1 -E program.c 2>&1 | ./astpp.lisp --raw~%")
  (format t "~%")
  (format t "Raw mode example:~%")
  (format t "  ./astpp.lisp --raw program.ast~%")
  (format t "  # Shows 'M' instead of 'DEREF', '=' instead of 'ASSIGN', etc.~%")
  (format t "~%")
  (format t "Or use with ccc driver:~%")
  (format t "  ./ccc -E program.c  # generates program.ast~%")
  (format t "  ./astpp.lisp program.ast~%")
  (sb-ext:exit :code 1))

(defun main (filename)
  "Main entry point - if filename is nil, read from stdin"
  (setup-reader)
  (let ((ast (if filename
                 (preprocess-ast-file filename)
                 (preprocess-ast-stdin))))
    (pp-ast ast))
  (unless *raw-mode*
    (format t "~%========================================~%")))

;; Parse command-line arguments
(defun parse-args (args)
  "Parse command-line arguments, return (filename . raw-mode-flag)"
  (let ((filename nil)
        (raw-mode nil))
    (loop for arg in args
          do (cond
               ((or (string= arg "-h") (string= arg "--help") (string= arg "help"))
                (usage))
               ((or (string= arg "-r") (string= arg "--raw"))
                (setf raw-mode t))
               ((and (not filename) (not (char= (char arg 0) #\-)))
                (setf filename arg))
               (t
                (format t "Unknown option: ~A~%" arg)
                (usage))))
    ;; filename can be nil - means read from stdin
    (cons filename raw-mode)))

;; Run if called as script
(when (and *posix-argv* (>= (length *posix-argv*) 1))
  (let* ((parsed (parse-args (cdr *posix-argv*)))
         (filename (car parsed))
         (raw-mode (cdr parsed)))
    (setf *raw-mode* raw-mode)
    (main filename)))
