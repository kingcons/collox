(defpackage :collox.parser
  (:use :cl :iterate))

(in-package :collox.parser)

(defclass expr () ())

(deftype bool ()
  '(member :true :false))

(deftype lox-value ()
  '(or number string bool nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun build-slot (slot-name)
    (let ((slot-type (case slot-name
                       ((left right expression) 'expr)
                       (operator 'collox.scanner:token)
                       (literal 'lox-value)))
          (reader-name (alexandria:symbolicate "EXPR-" slot-name)))
      `(,slot-name :initarg ,(alexandria:make-keyword slot-name)
                   :reader ,reader-name :type ,slot-type))))

(macrolet ((def (name &rest slots)
             `(defclass ,name (expr)
                ,(mapcar #'build-slot slots))))
  (def literal value)
  (def unary operator expression)
  (def binary left operator right)
  (def grouping expression))
