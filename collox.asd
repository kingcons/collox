(defsystem "collox"
  :version "0.1.0"
  :author "Brit Butler"
  :license "BSD"
  ;; TODO: Investigate using esrap for parsing, screamer for ... other stuff
  :depends-on ("alexandria"
               "trivia"
               "iterate"
               "unix-opts")
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("scanner" "util"))
                 (:file "scanner" :depends-on ("util"))
                 (:file "util"))))
  :description "A Common Lisp Lox Interpreter"
  :build-operation "program-op"
  :build-pathname "bin/collox"
  :entry-point "collox:main"
  :in-order-to ((test-op (test-op "collox.tests"))))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))

(defsystem "collox.tests"
  :author "Brit Butler"
  :license "BSD"
  :depends-on ("collox"
               "fiasco")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for collox"
  :perform (test-op (op c) (symbol-call :fiasco :run-tests :collox.tests)))
