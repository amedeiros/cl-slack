(defsystem "cl-slack"
  :long-name "common-lisp-slack"
  :version "0.0.1"
  :author "Andrew Medeiros"
  :maintainer "Andrew Medeiros"
  :mailto "andrew@amedeiros.com"
  :license "MIT"
  :homepage "https://github.com/amedeiros/cl-slack"
  :bug-tracker "https://github.com/amedeiros/cl-slack/issues"
  :source-control "https://github.com/amedeiros/cl-slack.git"
  :depends-on (:dexador
               :alexandria
               :websocket-driver-client
               :com.inuoe.jzon)
  :components ((:module "src"
                :components
                ((:file "cl-slack" :depends-on ("util" "client"))
                 (:file "util")
                 (:file "client"))))
  :description "CL Slack Framework"
  :long-description "Common Lisp framework for interacting with slack."
  :in-order-to ((test-op (test-op "cl-slack/tests"))))

(defsystem "cl-slack/tests"
  :author "Andrew Medeiros"
  :license "MIT"
  :depends-on ("cl-slack"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-slack"
  :perform (test-op (op c) (symbol-call :rove :run c)))
