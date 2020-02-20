(defpackage collox
  (:use :cl))
(in-package :collox)

(opts:define-opts
  (:name :help
   :description "Display usage instructions."
   :short #\h
   :long "help"))

(defun unknown-option (condition)
  (format t "WARN: ~s option was not recognized.~%"
          (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-bind ((opts:unknown-option #'unknown-option))
        (opts:get-opts))
    (when-option (options :help)
      (opts:describe :usage-of "collox" :args free-args))))

(defun deploy ()
  (sb-ext:save-lisp-and-die "bin/collox"
                            :executable t
                            :compression 9
                            :toplevel #'main))
