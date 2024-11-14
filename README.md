# Cl-Slack - CL Slack Framework

Slack app framework for Common Lisp. Currently supports async websocket and does not support webhook callback yet.

## Installation

Git clone into your quicklisp local-projects directory.

## Usage

```lisp
(ql:quickload :cl-slack)
(ql:quickload :com.inuoe.jzon)

; Set initial value so we can load this many times in a repl
(defvar app nil)

; Create a new app.
(setf app (cl-slack:make-async-app
           :token (uiop:getenv "SLACK_APP_TOKEN")
           :bot-token (uiop:getenv "SLACK_BOT_TOKEN")))

; Respond to a wave!
(cl-slack:define-message-handler app ":wave:" (message say)
                                 (funcall say :text "Howdy! :wave:"))

; Respond to a help request.
(cl-slack:define-message-handler app "^!help$" (message say)
                                 (funcall say
                                   :text (format nil "<@~A> Working on it!" (cl-slack:message-user message))))

; Respond with blocks.
(cl-slack:define-message-handler app "^!block-test$" (message say)
                                 (funcall say
                                   :blocks (com.inuoe.jzon:parse #P"./block-example.json")))

; Start
; (cl-slack:connect-async-app app)
; Close
; (cl-slack:close-async-app app)
```

## Author

* Andrew Medeiros (andrew@amedeiros.com)

## Copyright

Copyright (c) 2024 Andrew Medeiros (andrew@amedeiros.com)

## License

Licensed under the MIT License.
