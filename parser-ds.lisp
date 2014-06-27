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
  (format stream "<~a ~a>" (type-of node) (ast-name node)))

(extend-class term ast-named-node "Base term")
(extend-class function-term term "Function term")
(extend-class var term "Variable")
(extend-class conditional-term term "Conditional term")
(extend-class let-term term "Let term")

(extend-class plain-term function-term "Plain term")
(extend-class defined-term function-term "Defined term")
(extend-class system-term function-term "System term")

(extend-class constant plain-term "Constant")

(extend-class functor constant "Functor")




(extend-class predicate ast-named-node "Predicate")

(extend-class proposition ast-named-node "Proposition")

(defclass plain-atomic-formula (ast-node)
  ((predicate :initarg  :predicate
              :initform (error ":predicate must be specified")
              :reader predicate)
   (arguments :initarg  :arguments
              :initform nil
              :reader arguments)))

(defmethod print-object ((formula plain-atomic-formula) stream)
  (if (null (arguments formula))
      (format stream "(~a)" (predicate formula))
      (format stream "(~a~a)" (predicate formula) (arguments formula))))
    
(extend-class statement ast-node "Statement")

(defclass include (statement)
  ((file :initarg :file
         :initform (error ":file must be specified")
         :reader   file))
  (:documentation "Include directive"))

(defmethod print-object ((node include) stream)
  (format stream "<INCLUDE ~a>" (file node)))

(extend-class annotated statement "Annotated formula")

(defclass fof-statement (annotated)
  ((name    :initarg  :name
            :initform (error ":name must be specified")
            :reader   name)
   (role    :initarg  :role
            :initform (error ":role must be specified")
            :reader   role)
   (formula :initarg  :formula
            :initform (error ":formula must be specified")
            :reader   formula))
  (:documentation "FOF statement"))

(defmethod print-object ((statement fof-statement) stream)
  (format stream "<FOF ~a ~a ~a>" (name statement) (role statement) (formula statement)))

(defclass file (ast-node)
  ((statements :initarg  :statements
               :initform (error ":statements must be specified")
               :reader   statements))
  (:documentation "File"))

(defmethod print-object ((file file) stream)
  (loop for statement in (statements file) do
       (format stream "~a~%" statement)))
