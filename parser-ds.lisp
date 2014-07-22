;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: MONTY.PARSER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; TPTP language parser data structures.

(in-package :cl-tptp-parser.parser)

(defmacro extend-class (class base-class documentation)
  (if (listp base-class)
      `(defclass ,class ,base-class
         ()
         (:documentation ,documentation))
      `(defclass ,class (,base-class)
         ()
         (:documentation ,documentation))))
  
    
(defclass ast-node ()
  ((token :initarg  :token
          :initform (error ":token must be specified")
          :reader   ast-token))
  (:documentation "Base AST node"))

(defclass ast-named ()
  ((name :initarg  :name
         :initform (error ":name must be specified")
         :reader   ast-name))
  (:documentation "Base named AST node"))

(defclass ast-argumented ()
  ((arguments :initarg  :arguments
              :initform (error ":arguments must be specified")
              :reader arguments))
  (:documentation "Node with arguments"))

(defmethod print-object ((node ast-named) stream)
  (format stream "~a" (ast-name node)))

(extend-class term ast-node "Base term")
(extend-class function-term term "Function term")
(extend-class var (term ast-named) "Variable")
(extend-class conditional-term term "Conditional term")
(extend-class let-term term "Let term")

(extend-class plain-term function-term "Plain term")

(defclass constant (plain-term)
  ((value :initarg  :value
          :initform (error ":value must be specified")
          :reader   value))
  (:documentation "Constant"))

(defmethod print-object ((constant constant) stream)
  (format stream "~a" (value constant)))

(defclass function-call (plain-term ast-argumented)
  ((functor :initarg  :functor
            :initform (error ":functor must be specified")
            :reader   functor))
  (:documentation "Plain term with arguments"))

(defmethod print-object ((term function-call) stream)
  (format stream "(~a ~{~a~})" (functor term) (arguments term)))
           

(extend-class defined-term function-term "Defined term")
(extend-class system-term function-term "System term")

;;(extend-class constant plain-term "Constant")

(extend-class functor constant "Functor")




(extend-class predicate ast-named-node "Predicate")

(extend-class proposition ast-named-node "Proposition")

(extend-class fof-formula ast-node "FOF formula")
(extend-class fof-logic-formula fof-formula "FOF logic formula")
(extend-class fof-sequent fof-formula "FOF sequent")

(defclass fof-binary-formula (fof-logic-formula)
  ((binop :initarg  :binop
          :initform (error ":binop must be specified")
          :reader   binop)
   (left  :initarg  :left
          :initform (error ":left must be specified")
          :reader   left)
   (right  :initarg  :right
          :initform (error ":right must be specified")
          :reader   right))
  (:documentation "FOF binary formula"))

(defmethod print-object ((formula fof-logic-formula) stream)
    (format stream "(~a ~a ~a)" (binop formula) (left formula) (right formula)))

(extend-class fof-unitary-formula fof-logic-formula "FOF unitary formula")

(defclass fof-quantified-formula (fof-unitary-formula)
  ((quantifier :initarg  :quantifier
               :initform (error ":quantifier must be specified (ALL or ANY)")
               :reader   quantifier)
   (variables  :initarg  :variables
               :initform (error ":variables must be specified")
               :reader   variables)
   (formula    :initarg  :formula
               :initform (error ":formula must be specified")
               :reader   formula))
  (:documentation "FOF quantified formula"))

(defmethod print-object ((qf fof-quantified-formula) stream)
  (format stream "(FOR~a ~s ~s)" (quantifier qf) (variables qf) (formula qf)))
              
(defclass plain-atomic-formula (ast-node ast-argumented)
  ((predicate :initarg  :predicate
              :initform (error ":predicate must be specified")
              :reader predicate))
  (:documentation "Plain atomic formula"))

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
  (format stream "(INCLUDE ~a)" (file node)))

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
  (format stream "(FOF ~a ~a ~a)" (name statement) (role statement) (formula statement)))

(defclass file (ast-node)
  ((statements :initarg  :statements
               :initform (error ":statements must be specified")
               :reader   statements))
  (:documentation "File"))

(defmethod print-object ((file file) stream)
  (loop for statement in (statements file) do
       (format stream "~a~%" statement)))
