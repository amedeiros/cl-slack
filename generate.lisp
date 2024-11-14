;; Code to generate this project skeleton
(ql:quickload :cl-project)
(cl-project:make-project #p"/Users/amedeiros/code/lisp_projects/cl-slack"
  :name "cl-slack"
  :long-name "common-lisp-slack"
  :author "Andrew Medeiros"
  :maintainer "Andrew Medeiros"
  :email "andrew@amedeiros.com"
  :license "MIT"
  :homepage "https://github.com/amedeiros/cl-slack"
  :bug-tracker "https://github.com/amedeiros/cl-slack/issues"
  :source-control "https://github.com/amedeiros/cl-slack.git"
  :version "0.0.1"
  :description "CL Slack Framework"
  :long-description "Common Lisp framework for interacting with slack."
  :depends-on '(:dexador :alexandria :websocket-driver-client)
  :use '(:cl :dexador :alexandria :websocket-driver-client)
  :import-from '()
  :export '(test-function test-constant)
  :code '((alexandria:define-constant test-constant "hallo" :test 'string=)
          (defun test-function (user)
            "docstring"
            (concat test-constant " " user)))
  :load-system t)
