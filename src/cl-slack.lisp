(defpackage cl-slack
  (:use :cl)
  (:import-from :event-emitter
                :on)
  (:import-from :com.inuoe.jzon
                :parse
                :stringify)
  (:import-from :alexandria
                :switch
                :alist-hash-table)
  (:import-from :cl-slack.util
                :print-hashtable
                :get-nested-value
                :format-flush
                :print-flush)
  (:import-from :cl-slack.client
                :rest-ws-auth
                :post-chat-msg
                :get-auth-test)
  (:export
   ; Async (websocket) app functions
   :async-app
   :make-async-app
   :async-app-ws
   :add-message-handler
   :define-message-handler
   :connect-async-app
   :close-async-app

   ; Message functions for the struct passed to callables on messages
   :message-user
   :message-text
   :message-team-id
   :message-channel
   :message-ts))
(in-package :cl-slack)

;; Messages and functons for messages to/from slack

(defstruct (bot-info (:constructor make-bot-info))
  (id nil :type string)
  (url nil :type string)
  (team nil :type string)
  (team-id nil :type string)
  (user nil :type string)
  (user-id nil :type string)
  (is-enterprise-install nil :type boolean))

(defun get-bot-info (token)
  "Get the bot info and convert into bot-info struct"
  (let ((bot (get-auth-test :token token)))
    (when (gethash "error" bot)
          (error (format nil "Error getting bot info ~A~%" (gethash "error" bot))))
    (make-bot-info
      :id (gethash "bot_id" bot)
      :url (gethash "url" bot)
      :team (gethash "team" bot)
      :team-id (gethash "team_id" bot)
      :user (gethash "user" bot)
      :user-id (gethash "user_id" bot)
      :is-enterprise-install (gethash "is_enterprise_install" bot))))

; Message to Say function
(defstruct (message (:constructor make-message))
  (user nil :type string)
  (text nil :type string)
  (team-id nil :type string)
  (channel nil :type string)
  (ts nil :type string))

;; ResponseMessage
(defstruct (channel-response (:constructor make-channel-response))
  "A struct that represents a response to a channel"
  (channel nil :type string)
  (text nil :type string)
  (charset "UTF-8" :type string)
  (blocks nil))

;; Async App.
(defstruct (async-app (:constructor make-async-app (&key token bot-token)))
  "Async application (websocket) for Slack."
  ws ; Websocket Object
  bot ; Bot-info Object
  (token nil :type string) ; Authentication app token xapp-1-blah-blah
  (bot-token nil :type string) ; Authentication bot token xoxb-1-blah-blah
  (hello-event (make-hash-table) :type hash-table) ; Store the hello event from Slack which includes connection info
  (message-handlers
   (make-hash-table :test 'equal) :type hash-table))

;; Async Application functions

(defun add-message-handler (app pattern callable)
  "Store a pattern to later match to messages and execute the callable on match"
  (setf (gethash pattern (async-app-message-handlers app)) callable))

(defmacro define-message-handler (app pattern (&rest args) &body body)
  "Define a message handler for the given app and pattern."
  `(add-message-handler
     ,app
     ,pattern
     (lambda ,args
       (declare (ignorable ,@args)) ; In case not all args are used
       ,@body)))

(defun async-send-message (app data)
  "Send a string over the websocket connection."
  (wsd:send (async-app-ws app) (stringify data)))

(defun ack-message (app envelope-id)
  "Acknowledge the message with the envelope-id to slack"
  (async-send-message
    app
    (alist-hash-table `(("envelope_id" . ,envelope-id)))))

(defun say (app channel)
  "This the say function we pass to our message handlers for sending messages.
  Wrap in a lambda to pass down the app and automatic channel response handling."
  (lambda (&key (text "") (blocks nil))
    (let ((response
           (post-chat-msg
             :token (async-app-bot-token app)
             :data (make-channel-response :channel channel :text text :blocks blocks)))))))

(defun process-inbound-message (app payload)
  "Process an inbound message and trigger any corresponding callable based on regex matching."
  (let* ((text (get-nested-value payload "payload" "event" "text"))
         (message (make-message
                    :user (get-nested-value payload "payload" "event" "user")
                    :text text
                    :team-id (get-nested-value payload "payload" "team_id")
                    :channel (get-nested-value payload "payload" "event" "channel")
                    :ts (get-nested-value payload "payload" "event" "ts"))))
    (maphash
      (lambda (pattern callable)
        (when (cl-ppcre:scan pattern text)
              (funcall callable
                message
                (say app
                     (get-nested-value payload "payload" "event" "channel")))))
      (async-app-message-handlers app))))

(defun process-payload (app payload)
  "Process a json payload from slack for the given app"
  (let ((value (gethash "type" payload))
        (envelope-id (gethash "envelope_id" payload)))
    ; Always ack the message unless missing envelope-id
    (when envelope-id
          (ack-message app envelope-id))

    (switch (value :test #'string-equal)
      ("hello" ; Comeback to this for connecting timeout
              (setf (async-app-hello-event app) payload))
      ("events_api"
       (let ((payload-type (get-nested-value payload "payload" "event" "type"))
             (payload-subtype (get-nested-value payload "payload" "event" "subtype"))
             (user-id (get-nested-value payload "payload" "event" "user"))
             (bot-id (bot-info-user-id (async-app-bot app))))
         (if (string-equal "message_deleted" payload-subtype)
             (print-flush "TODO: Handle message deletes!")
             (switch (payload-type :test #'string-equal)
               ("message"
                ; Ignore ourself!
                (unless (string-equal user-id bot-id)
                  (process-inbound-message app payload)))
               ("app_mention"
                (print-flush "TODO: App Mentions!"))
               ((format-flush t "Unknown payload type: ~A~%" payload-type))))))
      ("disconnect"
       (print-flush "Received disconnect!")
       (close-async-app app))
      ; otherwise
      ((format-flush t "Unknown type: ~A~%" value)))))

(defun on-connect-async-app (app callable)
  "On connect apply callable
  We wrap the callable in another lambda to pass down the app to the callable."
  (on :open
      (async-app-ws app)
      (lambda ()
        (funcall callable app))))

(defun on-message-async-app (app callable)
  "For every message received appply the callable.
  We wrap the callable in another lambda to pass down the app to the callable."
  (on :message
      (async-app-ws app)
      (lambda (message)
        (funcall callable app (parse message)))))

(defun on-close-async-app (app callable)
  "If there is a close on the socket run the callable"
  (on :close
      (async-app-ws app)
      (lambda (&key code reason)
        (funcall callable app code reason))))

(defun on-error-async-app (app callable)
  "If there is an error on the socket run the callable"
  (on :error
      (async-app-ws app)
      (lambda (error)
        (funcall callable app error))))

(defun close-async-app (app)
  "Close the websocket connection."
  (let ((ws (async-app-ws app)))
    (when ws
          (wsd:close-connection ws)
          (setf (async-app-ws app) nil)
          (setf (async-app-hello-event app) (make-hash-table))))
  t)

(defun ws-connection (url)
  "Connect to the websocket endpoint returned from rest-ws-auth.
  This is the low level websocket object."
  (wsd:make-client url))

(defun connect-async-app (app)
  "Creat and open the websocket connection.
  Set the default internal on connect and on message functions."
  (unless (async-app-ws app)
    ; Get/Set the Bot-Info from Slack
    (setf (async-app-bot app) (get-bot-info (async-app-bot-token app)))
    ; Get/Create/Set the Websocket to Slack
    (setf (async-app-ws app)
      (ws-connection
        (rest-ws-auth (async-app-token app))))
    ; Add some on-action callables
    (on-connect-async-app app (lambda (app) (format-flush t "Connected to slack!")))
    (on-message-async-app app #'process-payload)
    (on-close-async-app app (lambda (app code reason)
                              (format-flush t "Closed because '~A' (Code=~A)~%" reason code)))
    (on-error-async-app app (lambda (app error)
                              (format-flush t "Got an error: ~S~%" error)
                              (close-async-app app)))
    ; Start the websocket connection
    (wsd:start-connection (async-app-ws app)))
  t)
