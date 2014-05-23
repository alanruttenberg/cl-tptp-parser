;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: MONTY.PARSER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; TPTP language lexer.

(in-package :cl-tptp-parser.parser)

(defclass filepos ()
  ((file :initform ""
         :initarg :file
         :accessor pos-file
         :documentation "File name")
   (line :initform -1
         :initarg :line
         :accessor pos-line
         :documentation "Line number")
   (col  :initform -1
         :initarg :col
         :accessor pos-col
         :documentation "Column number"))
  (:documentation "Position of a token in a file"))

(defun make-pos (file line col)
  (make-instance 'filepos
                 :file file
                 :line line
                 :col col))

(defclass token ()
  ((terminal :initform nil
             :initarg :terminal
             :accessor token-terminal
             :documentation "Token terminal")
   (text     :initform ""
             :initarg :text
             :accessor token-text
             :documentation "Contained text")
   (value    :initform nil
             :initarg :value
             :accessor token-value
             :documentation "Numeric value")
   (pos      :initform nil
             :initarg :pos
             :documentation "Position in file"))
  (:documentation "Token"))

(defun make-token (terminal text pos)
  (make-instance 'token
                 :terminal terminal
                 :text text
                 :pos pos))

(defun make-token-with-value (terminal value text pos)
  (make-instance 'token
                 :terminal terminal
                 :value value
                 :text text
                 :pos pos))

(defmethod print-object ((tkn token) stream)
  (format stream "<Token ~s ~s>" (token-terminal tkn) (token-text tkn)))


(defmacro define-keyword-lookup-table (keywords)
  (let ((set-code (append '(progn) (loop for keyword in keywords
                                      collecting (let ((sym (read-from-string keyword)))
                                                   `(setf (gethash ,keyword table) ',sym))))))
    `(let ((table (make-hash-table :test 'equal)))
       ,set-code
       table)))

(defparameter *keywords* (define-keyword-lookup-table (;; Formula types
                                                       "thf" "tff" "fof" "cnf" "tpi"

                                                       ;; Formula roles
                                                       "axiom" "hypothesis" "lemma" "conjecture"

                                                       ;; System
                                                       "include"
                                                   
                                                       ;; Boolean
                                                       "$true" "$false"
                                                       "$distinct"
                                                       
                                                       ;; Arithmetic
                                                       "$int" "$rat" "$real"
                                                       "$less" "$lesseq" "$greater" "$greatereq"
                                                       "$uminus" "$sum" "$difference" "$product"
                                                       "$quotient_e" "$quotient_t" "$quotient_f"
                                                       "$remainder_e" "$remainder_t" "$remainder_f"
                                                       "$floor" "$ceiling" "$truncate" "$round"
                                                       "$is_int" "$is_rat" "$to_int" "$to_rat" "$to_real")))


(defparameter *delimiters-operators* (stable-sort '(;; Punctuation
                                                    (|(|   1 (#\())
                                                    (|)|   1 (#\)))
                                                    (|,|   1 (#\,))
                                                    (|.|   1 (#\.))
                                                    (|[|   1 (#\[))
                                                    (|]|   1 (#\]))
                                                    (|:|   1 (#\:))

                                                    ;; Operators
                                                    (|!|   1 (#\!))
                                                    (|?|   1 (#\?))
                                                    (|~|   1 (#\~))
                                                    (|\||  1 (#\|))
                                                    (|&|   1 (#\&))
                                                    (|<=>| 3 (#\< #\= #\>))
                                                    (|<=|  2 (#\< #\=))
                                                    (|>=|  2 (#\> #\=))
                                                    (|<~>| 3 (#\< #\~ #\>))
                                                    (|~\|| 2 (#\~ #\|))     ;; Watch for this
                                                    (|~&|  2 (#\~ #\&))
                                                    (|*|   1 (#\*))
                                                    (|+|   1 (#\+))
                                                    (|-->| 3 (#\- #\- #\>))

                                                    ;; Predicates
                                                    (|=|   1 (#\=))
                                                    (|!=|  2 (#\! #\=)))
                                                  (lambda (x y) (>= (cadr x) (cadr y)))))

                                                                       

(defmacro match-regex (regex)
  `(multiple-value-bind (s e) (cl-ppcre:scan ,regex line)
     (if (and (not (null s)) (zerop s))
         (progn
           (setf start s
                 end e
                 token-text (subseq line start end)
                 line (subseq line end))
           t)
         nil)))

(defun read-delim-op (line pos)
  (let ((delim-op-list *delimiters-operators*)
        (token nil)
        (len nil))
    (loop while (and (not token) delim-op-list) do
         (let* ((row (car delim-op-list))
                (terminal (car row))
                (len1 (cadr row))
                (chars (caddr row))
                (c0 (car chars))
                (c1 (if (>= len1 2) (cadr chars) #\space))
                (c2 (if (>= len1 3) (caddr chars) #\space))
                (len2 (length line))
                (t0 (char line 0))
                (t1 (if (>= len2 2) (char line 1) #\space))
                (t2 (if (>= len2 3) (char line 2) #\space)))
           (if (or (and (= len1 1) (>= len2 1) (char= c0 t0))
                   (and (= len1 2) (>= len2 2) (char= c0 t0) (char= c1 t1))
                   (and (= len1 3) (>= len2 3) (char= c0 t0) (char= c1 t1) (char= c2 t2)))
               (setf token (make-token terminal (subseq line 0 len1) pos)
                     len len1))
           (setf delim-op-list (cdr delim-op-list))))

    (values token len)))

(defmacro match-sym ()
  `(multiple-value-bind (tk l) (read-delim-op line (make-pos filepath line-num col))
     (when tk
       (setf token tk
             col (+ col l)
             line (subseq line l)))
     tk))

(defun tokenize-stream (code-stream &optional (filepath ""))
  (let ((line-num 1)
        (tokens '()))
    
    ;; Iterate over lines
    (loop as line = (read-line code-stream nil nil)
       while line do
         (let ((col 1)
               start end token-text token)

           (loop while (not (string= line "")) do
                (cond
                  ;; Match whitespace.
                  ((match-regex "[ \t]+"))
                  
                  ;; Match single-line comments.
                  ((match-regex "%.*$"))

                  ;; Upper word
                  ((match-regex "[A-Z][a-zA-Z0-9_]*")
                   (let ((kw-sym (gethash token-text *keywords*)))
                     (if kw-sym
                         (push (make-token kw-sym token-text (make-pos filepath line-num col)) tokens)
                         (push (make-token-with-value 'UPPER-WORD token-text token-text
                                                      (make-pos filepath line-num col)) tokens))))

                  ;; Lower word
                  ((match-regex "[a-z][a-zA-Z0-9_]*")
                   (let ((kw-sym (gethash token-text *keywords*)))
                     (if kw-sym
                         (push (make-token kw-sym token-text (make-pos filepath line-num col)) tokens)
                         (push (make-token-with-value 'LOWER-WORD token-text token-text
                                                      (make-pos filepath line-num col)) tokens))))

                  ;; Numbers
                  ((or
                    (match-regex "0")
                    (match-regex "[1-9][0-9]*")
                    (match-regex "[0-9]+\\.[0-9]+(e[+-][0-9]+)?")
                    (match-regex "\\.[0-9]+(e[+-][0-9]+)?"))
                   (push (make-token-with-value 'NUMBER (read-from-string token-text) token-text
                                                (make-pos filepath line-num col)) tokens))

                  ;; Match string
                  ((match-regex "'[^']*'")
                   (push (make-token-with-value 'STRING (subseq token-text 1 (1- end)) token-text
                                                (make-pos filepath line-num col)) tokens))
                  
                  ;; Symbols / operators / delimiters.
                  ((match-sym)
                   (push token tokens))
                  
                  ;; Unmatched
                  (t (error "Lexer error at ~s" line)))))
                  

                  
         (incf line-num))
    (nreverse tokens)))


(defun tokenize-file (filepath)
  "Tokenizes a TPTP language source file"
  (let ((s (open (merge-pathnames filepath))))
    (if s (tokenize-stream s)
        nil)))

(defun dump-token-list (tokens)
  (when tokens
    (loop for token in tokens do
         (format t "~s ~s ~s~%"
                 (token-terminal token)
                 (token-text token)
                 (if (token-value token)
                     (token-value token)
                     "")))))
  
(defun test0 ()
  (dump-token-list (tokenize-file "/home/gautham/work/tptp/TPTP-v6.0.0/Axioms/AGT001+0.ax")))

(defun test1 ()
  (dump-token-list (tokenize-file "/home/gautham/work/tptp/TPTP-v6.0.0/Problems/AGT/AGT001+1.p")))

(defun test2 ()
  (dump-token-list (tokenize-file "/home/gautham/work/tptp/TPTP-v6.0.0/tptp/AGT/AGT001+1.tptp")))

(defun make-stream-lexer (code-stream)
  "Creates a TPTP language lexer to tokenize the code in the given stream"
  (let ((token-list (tokenize-stream code-stream)))
    (dump-token-list token-list)
    (lambda ()
      (let ((token (pop token-list)))
        (when token (format t "LEXER returning ~s ~s~%" (token-terminal token) (token-text token)))
        (if (null token)
            (values nil nil)
            (values (token-terminal token) token))))))
      
        
(defun make-file-lexer (filename)
  "Creates a TPTP language lexer to tokenize the specified file"
  (let ((s (open (merge-pathnames filename))))
    (if s (make-stream-lexer s)
        nil)))
