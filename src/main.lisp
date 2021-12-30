(defpackage collox
  (:use :cl :iterate)
  (:export #:main))

(in-package :collox)

(opts:define-opts
  (:name :help
   :description "Display usage instructions."
   :short #\h
   :long "help"))

(defun usage ()
  (opts:describe
   :usage-of "collox"
   :args "FILE"
   :suffix "In the absence of a file argument a REPL is started."))

(defun unknown-option (condition)
  (format t "WARN: ~s option was not recognized.~%"
          (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(defun display-tokens (source)
  (let ((tokens (collox.scanner:tokenize source)))
    (iter (for token in tokens)
      (print token))
    (fresh-line)))

(defun run-file (file)
  (collox.util:with-source (source file)
    (display-tokens source)))

(defun run-repl ()
  (collox.util:with-prompt (:collox input)
    (display-tokens input)))

(defun main ()
  (multiple-value-bind (options free-args)
      (handler-bind ((opts:unknown-option #'unknown-option))
        (opts:get-opts))
    (when-option (options :help)
      (usage)
      (uiop:quit))
    (case (length free-args)
      (0 (run-repl))
      (1 (run-file (first free-args)))
      (otherwise (usage)))))
