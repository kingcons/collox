(defpackage collox.util
  (:use :cl :iterate)
  (:import-from :alexandria
                #:with-input-from-file)
  (:export #:define-printer
           #:with-lines
           #:with-prompt
           #:source-location
           #:source-path
           #:source-start
           #:source-finish))
(in-package :collox.util)

(defparameter *print-object-identity* nil)

(defmacro define-printer (type (&rest vars) format-string &rest args)
  "Define printer is a helper macro for generating a PRINT-OBJECT
method. DEFINE-PRINTER provides a shorthand for the common case where
slot values need to be safely displayed but not read back in."
  `(defmethod print-object ((,type ,type) stream)
     (let ,(iter (for v in vars)
                 (collect `(,v (handler-case (slot-value ,type ',v)
                                 (unbound-slot () :unbound)))))
       (print-unreadable-object (,type stream :type t)
         (let ((*print-pretty* nil))
           (format stream ,format-string ,@args))
         (when *print-object-identity*
           (format stream " {~X}" (sb-kernel:get-lisp-obj-address ,type)))))))

(defmacro with-lines ((line-var file &optional (stream-var 'in))
                      &body body)
  "WITH-LINES is a helper macro for iterating through lines in a file.
Reading EOF will end the loop. LINE-VAR will be bound to the current line.
FILE can be a string, pathname, or an open stream."
  `(with-input-from-file (,stream-var ,file)
     (iter (for ,line-var = (read-line ,stream-var nil nil))
       (while ,line-var)
       ,@body)))

(defmacro with-prompt ((title input &key eof-handler)
                        &body body)
  "WITH-PROMPT is a helper macro for defining a REPL or input prompt.
TITLE will serve as the prompt. INPUT will be bound to each line entered
by the user. EOF-HANDLER defaults to quitting to the terminal."
  `(handler-bind ((end-of-file (or ,eof-handler
                                   (lambda (&rest args)
                                     (sb-ext:exit)))))
     (iter
       (format t "~&~(~A~)> " ,title)
       (finish-output)
       (let ((,input (read-line)))
         ,@body))))

(defclass source-location ()
  ((path
    :initarg :path :initform nil
    :reader source-path)
   (line
    :initarg :line :initform (error 'no-source-line)
    :reader source-line)
   (start
    :initarg :start :initform (error 'no-source-start)
    :reader source-start)
   (finish
    :initarg :finish :initform (error 'no-source-finish)
    :reader source-finish)))

(define-printer source-location (line start finish)
  "L~d:~d,~d" line start finish)
