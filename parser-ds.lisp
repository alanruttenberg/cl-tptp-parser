;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: MONTY.PARSER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; TPTP language parser data structures.

(in-package :cl-tptp-parser.parser)

(defmacro extend-class (class base-class documentation)
  `(defclass ,class (,base-class)
     ()
     (:documentation ,documentation)))
    
(defclass ast-node ()
  ((token :initarg  :token
          :initform (error ":token must be specified")
          :reader   ast-token))
  (:documentation "Base AST node"))

(defclass ast-named-node (ast-node)
  ((name :initarg :name
         :initform (error ":name must be specified")
         :reader   ast-name))
  (:documentation "Base named AST node"))

(defmethod print-object ((node ast-named-node) stream)
  (format stream "<~a>" (ast-name node)))

(extend-class tptp-term ast-named-node "Base term")
(extend-class tptp-function-term tptp-term "Function term")
(extend-class tptp-variable tptp-term "Variable")
(extend-class tptp-conditional-term tptp-term "Conditional term")
(extend-class tptp-let-term tptp-term "Let term")

(extend-class tptp-plain-term tptp-function-term "Plain term")
(extend-class tptp-defined-term tptp-function-term "Defined term")
(extend-class tptp-system-term tptp-function-term "System term")

(extend-class tptp-constant tptp-plain-term "Constant")

(extend-class tptp-functor tptp-constant "Functor")




(extend-class tptp-predicate ast-named-node "Predicate")

(extend-class tptp-proposition ast-named-node "Proposition")

(extend-class tptp-statement ast-node "Statement")

(defclass tptp-include (tptp-statement)
  ((file :initarg :file
         :initform (error ":file must be specified")
         :reader   file))
  (:documentation "Include directive"))

(defmethod print-object ((node tptp-include) stream)
    (format stream "<INCLUDE ~a>" (file node)))

(defclass tptp-file (ast-node)
  ((statements :initarg  :statements
               :initform (error ":statements must be specified")
               :reader   file-statements))
  (:documentation "File"))

(defmethod print-object ((file tptp-file) stream)
  (loop for statement in (file-statements file) do
       (format stream "~a~%" statement)))
