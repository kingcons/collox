(defpackage collox.util
  (:use :cl :iterate)
  (:import-from :alexandria
                #:read-stream-content-into-string
                #:with-input-from-file
                #:with-unique-names)
  (:export #:define-printer
           #:with-prompt
           #:with-source
           #:*source-path*
           #:source-location
           #:source-path
           #:source-line
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

(defvar *source-path* nil
  "The current file being parsed by the interpreter.")

(defclass source-location ()
  ((path
    :initarg :path :initform *source-path*
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

(defun call-with-source-path (name function)
  (let ((*source-path* (probe-file name)))
    (with-input-from-file (input-stream name)
      (funcall function input-stream))))

(defmacro with-source-path ((pathname input-stream) &body body)
  `(call-with-source-path ,pathname (lambda (,input-stream) ,@body)))

(defmacro with-source ((source-var file &key (lines nil))
                       &body body)
  "WITH-SOURCE is a helper macro for reading a FILE of source code. During execution,
COLLOX.UTIL:*SOURCE-PATH* is bound to the open file to ease debugging. By default,
BODY executes in an environment where SOURCE-VAR is bound to a string of the file
contents. If the keyword arg LINES is non-nil, each line will be individually bound
to SOURCE-VAR and then processed by executing BODY."
  (with-unique-names (input-stream)
    `(with-source-path (,file ,input-stream)
       ,(if lines
            `(iter (for ,source-var = (read-line ,input-stream nil nil))
               (while ,source-var)
               ,@body)
            `(let ((,source-var (read-stream-content-into-string ,input-stream)))
               ,@body)))))

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
