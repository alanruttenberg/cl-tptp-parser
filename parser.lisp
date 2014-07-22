;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: MONTY.PARSER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; TPTP language parser.

(in-package :cl-tptp-parser.parser)

#|
(defun parse-file (filename)
  (make-instance 'tptp-file
                 :statements (yacc:parse-with-lexer (make-file-lexer filename) *tptp-grammar*)
                 :token nil))
|#

(defun parse-file (filename)
  (let ((statements (yacc:parse-with-lexer (make-file-lexer filename) *tptp-grammar*)))
    (make-instance 'file
                   :statements statements
                   :token nil)))

(defun parse-syn ()
  (parse-file "/home/gautham/work/lisp/cl-tptp-parser/test/SYN000+1.p"))

(defun parse-test-0 ()
  (parse-file "/home/gautham/work/tptp/TPTP-v6.0.0/tptp/AGT/AGT001+1.tptp"))

(defun parse-test-1 ()
  (parse-file "/home/gautham/work/tptp/TPTP-v6.0.0/Problems/AGT/AGT001+1.p"))
