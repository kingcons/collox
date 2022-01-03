(defpackage :collox.parser
  (:use :cl :iterate)
  (:import-from :collox.scanner
                #:token-type
                #:token-value)
  (:export #:parse))

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

(defstruct parser
  (current 0 :type fixnum)
  (tokens '() :type list))

(defun expression (parser)
  (equality parser))

(defun unary (parser)
  (with-slots (tokens current) parser
    (let ((operator (nth current tokens)))
      (when (member (token-type operator) '(:bang :minus))
        (let ((expr (unary parser)))
          (make-instance 'unary :operator operator :expression expr)))
      (primary parser))))

(defun primary (parser)
  (with-slots (tokens current) parser
    (let* ((literal (nth current tokens))
           (value (case (token-type literal)
                    (:false :false)
                    (:true :true)
                    (:nil nil)
                    ((:number :string) (token-value literal)))))
      ;; TODO: Add error handling and come up with a better way to advance() here.
      (prog1
          (if value
              (make-instance 'literal :value value)
              (make-instance 'grouping :expression (expression parser)))
        (incf current)))))

(macrolet ((def (name nonterminal &rest token-types)
             `(defun ,name (parser)
                (with-slots (tokens current) parser
                  (let ((expr (,nonterminal parser)))
                    (iter (for operator = (nth current tokens))
                      (while (and operator (member (token-type operator) ',token-types)))
                      (incf current)
                      (let ((right (,nonterminal parser)))
                        (setf expr (make-instance 'binary
                                                  :left expr
                                                  :operator operator
                                                  :right right))))
                    expr)))))
  (def equality comparison :bang-equal :double-equal)
  (def comparison term :greater :greater-equal :less :less-equal)
  (def term factor :minus :plus)
  (def factor unary :slash :star))

(defun parse (tokens)
  (let ((parser (make-parser :tokens tokens)))
    ;; TODO: Add error handling.
    (expression parser)))
