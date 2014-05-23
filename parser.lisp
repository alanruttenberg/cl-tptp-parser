;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: MONTY.PARSER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; TPTP language parser.

(in-package :cl-tptp-parser.parser)

(defun parse-file (filename)
  (yacc:parse-with-lexer (make-file-lexer filename) *tptp-grammar*))
