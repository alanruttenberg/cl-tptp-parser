;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: CL-USER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; ASDF System Definitions.

(in-package :cl-user)

(eval-when (:compile-toplevel)
  (error "This ASDF file should be run interpreted."))

;;; Basic
(asdf:defsystem :cl-tptp-parser.basic
  :description "Package information"
  :components ((:file "package")))

;;; Parser
(asdf:defsystem :cl-tptp-parser.parser
  :description "Lexer and parser module"
  :depends-on (:yacc :cl-tptp-parser.basic :cl-ppcre)
  :components ((:file "parser"
                      :depends-on ("grammar" "lexer"))
               (:file "grammar")
               (:file "lexer")))

;;; Main system
(asdf:defsystem :cl-tptp-parser
    :description "TPTP language parser"
    :depends-on (:cl-tptp-parser.basic :cl-tptp-parser.parser))
