;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-USER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; Package Definitions.

(in-package :cl-user)

(defpackage :cl-tptp-parser.basic
  (:documentation "Package information"))

(defpackage :cl-tptp-parser.parser
  (:documentation "Lexer and parser module")
  (:use :common-lisp)
  (:export :make-file-lexer :tokenize-file))

(defpackage :cl-tptp-parser
  (:documentation "TPTP language parser"))
