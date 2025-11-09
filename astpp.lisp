#!/usr/bin/sbcl --script
;;;; AST Pretty Printer for ccc compiler output
;;;; Formats S-expression AST in human-readable form

(defvar *indent-level* 0)
(defvar *indent-string* "  ")

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
  "Get human-readable name for operator"
  (let ((op-str (string op)))
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
      ((string= op-str "X") "SEXT")
      ((string= op-str "W") "WIDEN")
      ((string= op-str "?") "TERNARY")
      ((string= op-str "@") "CALL")
      ((string= op-str "NEG") "NEGATE")
      (t op-str))))

(defun statement-name (stmt)
  "Get human-readable name for statement type"
  (let ((stmt-str (string stmt)))
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
      (t stmt-str))))

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

           ;; Declaration
           ((or (eq stmt-type '|d|) (eq stmt-type 'd))
            (format t "~A~A ~A : ~A~%" (indent) stmt-name (second stmt) (third stmt)))

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

(defun pp-function (func-def)
  "Pretty print a function definition"
  (let* ((name (first func-def))
         (params (second func-def))
         (return-type (third func-def))
         (body (fourth func-def)))

    (format t "~%FUNCTION ~A(" name)
    (loop for param in params
          for i from 0
          do (when (> i 0) (format t ", "))
             (format t "~A" param))
    (format t ") -> ~A~%" return-type)
    (format t "{~%")
    (pp-statement body 1)
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

(defun pp-ast (ast)
  "Pretty print complete AST"
  (format t "========================================~%")
  (format t "AST Pretty Printer Output~%")
  (format t "========================================~%")

  (dolist (item ast)
    (when (listp item)
      (let ((tag (first item)))
        (cond
          ;; Global variable (g <name> <type> [<init>])
          ((or (eq tag 'g) (eq tag '|g|))
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

          ;; Function definition (f <name> <params> <type> <body>)
          ((or (eq tag 'f) (eq tag '|f|))
           (pp-function (rest item))))))))

(defun setup-reader ()
  "Configure the Lisp reader to handle AST symbols"
  (setf (readtable-case *readtable*) :preserve))

(defun preprocess-ast-file (filename)
  "Read and preprocess AST file, handling special characters"
  (let ((text (with-open-file (stream filename)
                (let ((content (make-string (file-length stream))))
                  (read-sequence content stream)
                  content))))

    ;; Escape special characters for the Lisp reader
    (setf text (loop with result = (make-array (length text) :element-type 'character
                                                :fill-pointer 0 :adjustable t)
                     with in-string = nil
                     for i from 0 below (length text)
                     for ch = (char text i)
                     for next-ch = (if (< (1+ i) (length text)) (char text (1+ i)) nil)
                     do (cond
                          ;; Track string state
                          ((and (char= ch #\")
                                (or (= i 0)
                                    (char/= (char text (1- i)) #\\)))
                           (vector-push-extend ch result)
                           (setf in-string (not in-string)))
                          ;; Backslash in string NOT followed by quote - double it
                          ((and in-string (char= ch #\\)
                                (not (and next-ch (char= next-ch #\"))))
                           (vector-push-extend ch result)
                           (vector-push-extend ch result))
                          ;; Vertical bar outside string - escape it
                          ((and (not in-string) (char= ch #\|))
                           (vector-push-extend #\\ result)
                           (vector-push-extend ch result))
                          ;; Everything else
                          (t
                           (vector-push-extend ch result)))
                     finally (return result)))

    ;; Replace colons with underscores (for width annotations like =:s -> =_s)
    (setf text (substitute #\_ #\: text))

    ;; Read all forms
    (with-input-from-string (stream text)
      (loop for form = (read stream nil :eof)
            until (eq form :eof)
            collect form))))

(defun usage ()
  "Print usage message"
  (format t "Usage: astpp.lisp <ast-file>~%")
  (format t "~%")
  (format t "Pretty print ccc compiler AST in human-readable format.~%")
  (format t "~%")
  (format t "Example:~%")
  (format t "  ./cc1 -E program.c > program.ast~%")
  (format t "  ./astpp.lisp program.ast~%")
  (format t "~%")
  (format t "Or use with ccc driver:~%")
  (format t "  ./ccc -E program.c  # generates program.ast~%")
  (format t "  ./astpp.lisp program.ast~%")
  (sb-ext:exit :code 1))

(defun main (filename)
  "Main entry point"
  (setup-reader)
  (let ((ast (preprocess-ast-file filename)))
    (pp-ast ast))
  (format t "~%========================================~%"))

;; Run if called as script
(cond
  ((not *posix-argv*)
   (usage))
  ((< (length *posix-argv*) 2)
   (usage))
  ((member (second *posix-argv*) '("-h" "--help" "help") :test #'string=)
   (usage))
  (t
   (main (second *posix-argv*))))
