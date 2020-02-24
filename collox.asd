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
                ((:file "main" :depends-on ("util"))
                 (:file "scanner" :depends-on ("util"))
                 (:file "util"))))
  :description "A Common Lisp Lox Interpreter"
  :in-order-to ((test-op (test-op "collox/tests"))))

(defsystem "collox/tests"
  :author "Brit Butler"
  :license "BSD"
  :depends-on ("collox"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for collox"
  :perform (test-op (op c) (symbol-call :rove :run c)))
