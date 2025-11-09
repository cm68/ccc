#!/usr/bin/sbcl --script
;;;; AST Interpreter for ccc compiler output
;;;; Reads S-expression AST and executes it as a virtual machine
;;;; Primary: SBCL (Steel Bank Common Lisp)
;;;; Also works with CLISP

(defpackage :ccc-interp
  (:use :cl)
  (:export :run-program :interpret-file))

(in-package :ccc-interp)

;;; Memory model - uses hash tables for simplicity
(defvar *globals* (make-hash-table :test 'equal))
(defvar *locals* nil)  ; Stack of local variable frames
(defvar *strings* (make-hash-table :test 'equal))  ; String literals
(defvar *functions* (make-hash-table :test 'equal))  ; Function definitions
(defvar *return-value* nil)
(defvar *break-flag* nil)
(defvar *continue-flag* nil)

;;; Type widths in bytes
(defun type-width (type-suffix)
  "Return byte width for type suffix (:b :s :l :p :f :d)"
  (case type-suffix
    ((:b) 1)   ; byte (char)
    ((:s) 2)   ; short (int)
    ((:l) 4)   ; long
    ((:p) 2)   ; pointer
    ((:f) 4)   ; float
    ((:d) 8)   ; double
    (t 2)))    ; default to short

;;; Utility functions
(defun parse-width-annotation (op)
  "Parse operator with width annotation like =_s or M_b (colon replaced), returns (op . width)"
  (let ((str (string op)))
    (if (and (find #\_ str) (> (length str) 2))
        (let ((pos (position #\_ str :from-end t)))  ; Find last underscore for width annotation
          (cons (intern (subseq str 0 pos) :keyword)
                (intern (format nil ":~A" (subseq str (1+ pos))) :keyword)))
        (cons (intern str :keyword) nil))))

(defun symbol-name-only (sym)
  "Extract variable name from $-prefixed symbol"
  (let ((name (string sym)))
    (cond
      ((and (>= (length name) 3)
            (char= (char name 0) #\$)
            (char= (char name 1) #\_))
       (subseq name 2))  ; Global: $_name -> name
      ((and (>= (length name) 3)
            (char= (char name 0) #\$)
            (char= (char name 1) #\A))
       (subseq name 2))  ; Argument: $Aname -> name
      ((and (>= (length name) 2)
            (char= (char name 0) #\$))
       (subseq name 1))  ; Local: $name -> name
      (t name))))

;;; Memory operations
(defun get-variable (sym)
  "Get variable value from locals or globals"
  (let ((name (symbol-name-only sym)))
    (cond
      ;; Check local frames (most recent first)
      ((loop for frame in *locals*
             for val = (gethash name frame)
             when val return val))
      ;; Check globals
      ((gethash name *globals*))
      ;; Check strings
      ((gethash name *strings*))
      ;; Not found - return 0
      (t 0))))

(defun set-variable (sym value)
  "Set variable value in locals or globals"
  (let ((name (symbol-name-only sym)))
    (cond
      ;; Try to update in local frames
      ((loop for frame in *locals*
             when (nth-value 1 (gethash name frame))
             do (setf (gethash name frame) value)
             and return t))
      ;; Otherwise set in globals
      (t (setf (gethash name *globals*) value)))
    value))

(defun narrow (value width)
  "Narrow value to width (truncate)"
  (let ((bits (* 8 (type-width width))))
    (logand value (1- (ash 1 bits)))))

(defun sign-extend (value from-width to-width)
  "Sign extend value from smaller to larger width"
  (let* ((from-bits (* 8 (type-width from-width)))
         (sign-bit (ash 1 (1- from-bits)))
         (is-negative (not (zerop (logand value sign-bit)))))
    (if is-negative
        (logior value (lognot (1- (ash 1 from-bits))))
        value)))

(defun zero-extend (value from-width to-width)
  "Zero extend (just mask to ensure clean value)"
  (let ((from-bits (* 8 (type-width from-width))))
    (logand value (1- (ash 1 from-bits)))))

;;; Expression evaluation
(defun eval-expr (expr)
  "Evaluate an expression and return its value"
  (cond
    ;; Numbers
    ((numberp expr) expr)

    ;; Symbols (variables)
    ((symbolp expr)
     (get-variable expr))

    ;; Lists (operators)
    ((listp expr)
     (let* ((op-with-width (first expr))
            (parsed (parse-width-annotation op-with-width))
            (op (car parsed))
            (op-str (string op))
            (width (cdr parsed)))

       (cond
         ;; Use string comparison to avoid case sensitivity issues
         ((string= op-str "M")  ; Dereference
          (eval-expr (second expr)))

         ;; Arithmetic
         ((string= op-str "+") (+ (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "-") (- (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "*") (* (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "/") (floor (eval-expr (second expr)) (max 1 (eval-expr (third expr)))))
         ((string= op-str "%") (mod (eval-expr (second expr)) (max 1 (eval-expr (third expr)))))

         ;; Bitwise
         ((string= op-str "&") (logand (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "|") (logior (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "^") (logxor (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "~") (lognot (eval-expr (second expr))))
         ((string= op-str "y") (ash (eval-expr (second expr)) (eval-expr (third expr))))
         ((string= op-str "w") (ash (eval-expr (second expr)) (- (eval-expr (third expr)))))

         ;; Comparison
         ((string= op-str "<") (if (< (eval-expr (second expr)) (eval-expr (third expr))) 1 0))
         ((string= op-str ">") (if (> (eval-expr (second expr)) (eval-expr (third expr))) 1 0))
         ((string= op-str "L") (if (<= (eval-expr (second expr)) (eval-expr (third expr))) 1 0))
         ((string= op-str "g") (if (>= (eval-expr (second expr)) (eval-expr (third expr))) 1 0))
         ((string= op-str "Q") (if (= (eval-expr (second expr)) (eval-expr (third expr))) 1 0))
         ((string= op-str "n") (if (/= (eval-expr (second expr)) (eval-expr (third expr))) 1 0))

         ;; Logical
         ((string= op-str "j") (if (and (not (zerop (eval-expr (second expr))))
                                        (not (zerop (eval-expr (third expr))))) 1 0))
         ((string= op-str "h") (if (or (not (zerop (eval-expr (second expr))))
                                       (not (zerop (eval-expr (third expr))))) 1 0))
         ((string= op-str "!") (if (zerop (eval-expr (second expr))) 1 0))

         ;; Unary operators
         ((string= op-str "NEG") (- (eval-expr (second expr))))

         ;; Assignment
         ((string= op-str "=")
          (let ((lval (second expr))
                (rval (eval-expr (third expr))))
            (if (and (listp lval) (eq (car lval) '$))
                (set-variable (second lval) rval)
                (set-variable lval rval))
            rval))

         ;; Type conversions
         ((string= op-str "N") (eval-expr (second expr)))
         ((string= op-str "X") (eval-expr (second expr)))
         ((string= op-str "W") (eval-expr (second expr)))

         ;; Address-of
         ((string= op-str "&") (eval-expr (second expr)))

         ;; Ternary conditional
         ((string= op-str "?") (if (not (zerop (eval-expr (second expr))))
                                   (eval-expr (second (third expr)))
                                   (eval-expr (third (third expr)))))

         ;; Compound assignments
         ((string= op-str "P") (let* ((lval (second expr))
                                      (old (eval-expr lval))
                                      (new (+ old (eval-expr (third expr)))))
                                 (set-variable lval new)))

         ;; Function call
         ((string= op-str "@") (funcall-ast (second expr) (cddr expr)))

         (t (format t "Unknown operator: ~S (string ~S)~%" op op-str)
            0))))

    (t 0)))

;;; Function calls
(defun extract-param-name (param-spec)
  "Extract parameter name from param_type specification (colon replaced with underscore)"
  (if (symbolp param-spec)
      (let ((str (string param-spec)))
        (if (find #\_ str)
            (subseq str 0 (position #\_ str))
            str))
      (string param-spec)))

(defun process-escape-sequences (str)
  "Process C escape sequences in a string"
  (let ((result (make-array (length str) :element-type 'character :fill-pointer 0))
        (i 0))
    (loop while (< i (length str))
          do (let ((ch (char str i)))
               (cond
                 ((and (char= ch #\\) (< (1+ i) (length str)))
                  (let ((next (char str (1+ i))))
                    (case next
                      (#\n (vector-push-extend #\Newline result))
                      (#\t (vector-push-extend #\Tab result))
                      (#\r (vector-push-extend #\Return result))
                      (#\\ (vector-push-extend #\\ result))
                      (#\" (vector-push-extend #\" result))
                      (t (vector-push-extend next result)))
                    (incf i 2)))
                 (t
                  (vector-push-extend ch result)
                  (incf i)))))
    result))

(defun builtin-printf (args)
  "Builtin printf implementation - prints first argument as string"
  (let ((fmt-arg (first args)))
    (cond
      ;; String literal reference
      ((and (symbolp fmt-arg)
            (gethash (symbol-name-only fmt-arg) *strings*))
       (let ((str (gethash (symbol-name-only fmt-arg) *strings*)))
         ;; Escape sequences were processed when loading
         (format t "~A" str)
         (length str)))
      ;; Direct string value
      ((stringp fmt-arg)
       (format t "~A" fmt-arg)
       (length fmt-arg))
      ;; Number
      (t
       (format t "~A" (eval-expr fmt-arg))
       1))))

(defun funcall-ast (func-expr args)
  "Call a function with arguments"
  (let* ((func-name (string-downcase (symbol-name-only func-expr)))
         (func-def (gethash func-name *functions*)))
    (cond
      ;; Check for builtin functions
      ((string= func-name "printf")
       (builtin-printf args))

      ;; User-defined function
      (func-def
       (let* ((params (second func-def))
              (body (car (last func-def)))
              (new-frame (make-hash-table :test 'equal)))

         ;; Bind parameters
         (loop for param in params
               for arg in args
               do (setf (gethash (extract-param-name param) new-frame)
                       (eval-expr arg)))

         ;; Push frame and execute
         (push new-frame *locals*)
         (setf *return-value* nil)
         (eval-statement body)
         (pop *locals*)

         (or *return-value* 0)))

      ;; Undefined function
      (t
       (format t "Warning: undefined function ~A~%" func-name)
       0))))

;;; Statement execution
(defun eval-statement (stmt)
  "Execute a statement"
  (when (or *return-value* *break-flag* *continue-flag*)
    (return-from eval-statement nil))

  (when (null stmt)
    (return-from eval-statement nil))

  (cond
    ;; Expression statement
    ((and (listp stmt) (eq (first stmt) '|E|))
     (eval-expr (second stmt)))

    ;; Return statement
    ((and (listp stmt) (eq (first stmt) '|R|))
     (setf *return-value* (if (second stmt)
                              (eval-expr (second stmt))
                              0)))

    ;; Block statement
    ((and (listp stmt) (eq (first stmt) '|B|))
     (let ((new-frame (make-hash-table :test 'equal)))
       (push new-frame *locals*)
       ;; Process declarations and statements
       (dolist (s (rest stmt))
         (when (or *return-value* *break-flag* *continue-flag*)
           (pop *locals*)
           (return-from eval-statement nil))
         (eval-statement s))
       (pop *locals*)))

    ;; Declaration
    ((and (listp stmt) (eq (first stmt) 'd))
     (let ((var-name (symbol-name-only (second stmt))))
       (setf (gethash var-name (first *locals*)) 0)))

    ;; If statement
    ((and (listp stmt) (eq (first stmt) '|I|))
     (if (not (zerop (eval-expr (second stmt))))
         (eval-statement (third stmt))
         (when (fourth stmt)
           (eval-statement (fourth stmt)))))

    ;; While loop
    ((and (listp stmt) (eq (first stmt) '|W|))
     (loop
       (when (or *return-value* *break-flag*)
         (setf *break-flag* nil)
         (return))
       (when (zerop (eval-expr (second stmt)))
         (return))
       (setf *continue-flag* nil)
       (eval-statement (third stmt))))

    ;; For loop
    ((and (listp stmt) (eq (first stmt) '|F|))
     (eval-expr (second stmt))  ; init
     (loop
       (when (or *return-value* *break-flag*)
         (setf *break-flag* nil)
         (return))
       (when (and (third stmt) (zerop (eval-expr (third stmt))))  ; condition
         (return))
       (setf *continue-flag* nil)
       (eval-statement (fifth stmt))  ; body
       (when (fourth stmt)
         (eval-expr (fourth stmt)))))  ; increment

    ;; Break
    ((and (listp stmt) (eq (first stmt) '|b|))
     (setf *break-flag* t))

    ;; Continue
    ((and (listp stmt) (eq (first stmt) '|N|))
     (setf *continue-flag* t))

    ;; Do-while loop
    ((and (listp stmt) (eq (first stmt) '|D|))
     (loop
       (setf *continue-flag* nil)
       (eval-statement (third stmt))  ; body
       (when (or *return-value* *break-flag*)
         (setf *break-flag* nil)
         (return))
       (when (zerop (eval-expr (second stmt)))  ; condition
         (return))))

    ;; Switch statement (simplified - just execute body for now)
    ((and (listp stmt) (eq (first stmt) '|S|))
     (eval-statement (third stmt)))

    (t nil)))

;;; Reader configuration
(defun setup-reader ()
  "Configure the Lisp reader to handle AST symbols"
  ;; Preserve case for symbols
  (setf (readtable-case *readtable*) :preserve)
  ;; Create dummy packages for type annotations
  (dolist (pkg '("n" "result" "i" "x" "y" "a" "b" "c" "d" "p" "q" "r" "s" "t" "v"))
    (unless (find-package pkg)
      (make-package pkg :use nil))))

;;; Program loading
(defun load-ast (ast)
  "Load AST into the interpreter"
  (dolist (item ast)
    (when (listp item)
      (let ((tag (first item)))
        (cond
          ;; Global variable
          ((or (eq tag 'g) (eq tag '|g|))
           (let ((name (symbol-name-only (second item))))
             (setf (gethash name *globals*)
                   (if (fourth item)
                       (eval-expr (fourth item))
                       0))))

          ;; String literal (s name "value")
          ((or (eq tag 's) (eq tag '|s|))
           (let ((name (symbol-name-only (second item)))
                 (value (third item)))
             (setf (gethash name *strings*) (process-escape-sequences value))))

          ;; Literals section (L (s name "value") ...)
          ((or (eq tag '|L|) (eq tag 'L))
           (dolist (lit (rest item))
             (when (and (listp lit)
                        (or (eq (first lit) 's) (eq (first lit) '|s|)))
               (let* ((name (symbol-name-only (second lit)))
                      (value (third lit))
                      (processed (process-escape-sequences value)))
                 (setf (gethash name *strings*) processed)))))

          ;; Function definition
          ((or (eq tag 'f) (eq tag '|f|))
           (let ((name (symbol-name-only (second item))))
             (setf (gethash name *functions*) (rest item)))))))))

;;; Main entry points
(defun interpret-file (filename)
  "Read and interpret an AST file"
  (setup-reader)
  (clrhash *globals*)
  (clrhash *functions*)
  (clrhash *strings*)
  (setf *locals* nil)

  ;; Read file as text, preprocess to handle colons in type specs
  (let ((text (with-open-file (stream filename)
                (let ((content (make-string (file-length stream))))
                  (read-sequence content stream)
                  content))))

    ;; Escape special characters for the Lisp reader
    ;; Strategy: Double backslashes EXCEPT when escaping a quote
    ;; - \n becomes \\n (reader produces \n, then process-escape-sequences makes newline)
    ;; - \" stays as \" (reader produces quote in the string)
    ;; - \\ becomes \\\\ (reader produces \\, then process-escape-sequences makes \)
    ;; - | outside strings becomes \| (reader produces symbol |)
    (setf text (loop with result = (make-array (length text) :element-type 'character
                                                :fill-pointer 0 :adjustable t)
                     with in-string = nil
                     for i from 0 below (length text)
                     for ch = (char text i)
                     for next-ch = (if (< (1+ i) (length text)) (char text (1+ i)) nil)
                     do (cond
                          ;; Track string state (quote toggles unless escaped)
                          ((and (char= ch #\")
                                (or (= i 0)
                                    (char/= (char text (1- i)) #\\)))
                           (vector-push-extend ch result)
                           (setf in-string (not in-string)))
                          ;; Backslash in string NOT followed by quote - double it
                          ((and in-string (char= ch #\\) (not (and next-ch (char= next-ch #\"))))
                           (vector-push-extend ch result)
                           (vector-push-extend ch result))
                          ;; Vertical bar outside string - escape it
                          ((and (not in-string) (char= ch #\|))
                           (vector-push-extend #\\ result)
                           (vector-push-extend ch result))
                          ;; Everything else passes through unchanged
                          (t
                           (vector-push-extend ch result)))
                     finally (return result)))

    ;; Simple replacement: change param:type to param_type
    ;; This avoids package qualification issues
    (setf text (substitute #\_ #\: text))

    (with-input-from-string (stream text)
      (let ((ast nil))
        ;; Read all S-expressions
        (loop for expr = (read stream nil nil)
              while expr
              do (push expr ast))

        ;; Load the program
        (load-ast (nreverse ast))

        ;; Find and call main
        (let ((main-func (gethash "main" *functions*)))
          (if main-func
              (let ((result (funcall-ast '$_main nil)))
                (format t "Program exited with code: ~A~%" result)
                result)
              (format t "No main function found~%")))))))

(defun run-program (ast-string)
  "Interpret AST from a string"
  (clrhash *globals*)
  (clrhash *functions*)
  (clrhash *strings*)
  (setf *locals* nil)

  (with-input-from-string (stream ast-string)
    (let ((ast nil))
      (loop for expr = (read stream nil nil)
            while expr
            do (push expr ast))

      (load-ast (nreverse ast))

      (let ((main-func (gethash "main" *functions*)))
        (if main-func
            (funcall-ast '$_main nil)
            (format t "No main function found~%"))))))

;;; Command-line interface
(defun get-command-line-args ()
  "Get command-line arguments in a portable way"
  #+sbcl sb-ext:*posix-argv*
  #+clisp ext:*args*
  #+ccl ccl:*command-line-argument-list*
  #-(or sbcl clisp ccl) nil)

(defun main ()
  (let ((args (get-command-line-args)))
    (if (and args (>= (length args) 1))
        (interpret-file (if (member :clisp *features*)
                           (first args)   ; clisp: first arg is filename
                           (second args))) ; sbcl: second arg is filename (first is script name)
        (format t "Usage: interp.lisp <ast-file>~%"))))

;; Run if executed as script
#+(or sbcl clisp)
(main)
