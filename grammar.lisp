;;;; -*- Mode: LISP, Syntax: COMMON-LISP, Package: MONTY.PARSER -*-
;;;;
;;;; Author: Gautham Ganapathy (gautham@lisphacker.org)
;;;;
;;;; TPTP language grammar specification.

(in-package :cl-tptp-parser.parser)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun dump-tree (x)
    (if (null x)
        "<>"
        (if (listp x)
            (let ((s ""))
              (loop for e in x do
                   (setf s (concatenate 'string s " " (dump-tree e))))
              (concatenate 'string "(" s ") "))
            (format nil "<~a ~a>" (token-terminal x) (token-text x)))))
  
  (defmacro dump-1 (name)
    (lambda (a)
      (format t "~s: ~s~%~%" name a)
      ;;(dump-tree a)
      a))
  
  (defmacro dump-2 (name)
    (lambda (a b)
      (format t "~s: (~s ~s)~%~%" name a b)
      ;;(dump-tree a)
      ;;(dump-tree b)
      (list a b)))
  
  (defmacro dump-3 (name)
    (lambda (a b c)
      (format t "~s: (~s ~s ~s)~%~%" name a b c)
      ;;(dump-tree a)
      ;;(dump-tree b)
      ;;(dump-tree c)
      (list a b c)))
  )

(yacc:define-parser *tptp-grammar*
  (:start-symbol file-input)
  (:terminals (LOWER-WORD UPPER-WORD NUMBER STRING

                  ;; Formula types
                  THF TFF FOF CNF TPI

                  ;; Formula roles
                  AXIOM HYPOTHESIS LEMMA CONJECTURE

                  ;; System
                  INCLUDE

                  ;; Boolean
                  $TRUE $FALSE $DISTINCT

                  ;;Punctuation
                  |(| |)| |'| |.| |[| |]| |:|

                  ;; Operators
                  |!| |?| |~| |&| |\||
                  |<=>| |<=| |>=|
                  |<~>| |~\|| |~&|
                  |*| |+|
                  |-->|
                  
                  ;; Predicates
                  |=| |!=|))

  ;;(:precedence ((:left **) (:left %) (:left * / //) (:left + -)
    ;;            (:left << >> & \| ^ ~) (:left < > <= >= == !=) (:left NOT) (:left OR AND)))

  ;;(:print-lookaheads t)

  #|
  (tptp-file
   tptp-input
   (tptp-input tptp-file))

  (tptp-input
   annotated-formulat
   include-stmt)

  (annotated-formula
   thf-annotated
   tff-annotated
   fof-annotated
   cnf-annotated
   tpi-annotated)
  
  (fof-annotated
   (fof |(| name |,| formula-role |,| fof-formula annotations |)| |.| ))

  (formula-role
   AXIOM
   HYPOTHESIS
   LEMMA
   CONJECTURE)

  ;; FOF formulae.
  (fof-formula
   fof-logic-formula
   fof-sequent)
  
  (fof-logic-formula
   fof-binary-formula
   fof-unary-formula)
  
  (fof-binary-formula
   fof-binary-nonassoc
   fof-binary-assoc)
  
  (fof-binary-nonassoc
   (fof-unitary-formula binary-connective fof-unitary-formula))

  (fof-binary-assoc
   fof-or-formula
   for-and-formula)

  (fof-or-formula
   (fof-unitary-formula |\|| fof-unitary-formula)
   (fof-or-formula |\|| fof-unitary-formula)
   
   (fof-and-formula
    (fof-unitary-formula |&| fof-unitary-formula)
    (fof-and-formula |&| fof-unitary-formula))

  (fof-unitary-formula
   fof-quantified-formula
   fof-unary-formula
   atomic-formula
   ( |(| fof-logic-formula |)| ))

  (fof-quantified-formula
   (fol-quantifier |[| fof-variable-list |]| |:| fof-unitary-formula))

  (fof-variable-list
   variable
   (variable |,| fof-variable-list))

  (fof-unary-formula
   (unary-connective fof-unitary-formula)
   fol-infix-unary)
   
  (fof-sequent
   (fof-tuple |-->| fof-tuple)
   ( |(| fof-sequent |)| ))

  (fof-tuple
   ( |[| |]| )
   ( |[| fof-tuple-list |]| ))

  (fof-tuple-list
   fof-logic-formula
   (fof-logic-formula |,| fof-tuple-list))

  ;; Special formulae.
  (fol-infix-unary
   (term infix-inequality term))

  ;; Connectives
  (fol-quantifier
   |!|
   |?|)


  ;; System terms
  (variable
   UPPER-WORD)
  
  ;; General purpose
  (name
   atomic-word)

  (atomic-word
   LOWER-WORD
   single-quoted)

  |#
  (single-quoted
   STRING))

