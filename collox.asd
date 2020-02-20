(defsystem "collox"
  :version "0.1.0"
  :author "Brit Butler"
  :license "BSD"
  :depends-on ("alexandria"
               "trivia"
               "iterate"
               "unix-opts")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
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
