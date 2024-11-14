(defpackage :cl-slack.client
  (:use :cl)
  (:import-from :com.inuoe.jzon
                :parse
                :stringify)
  (:import-from :alexandria
                :switch)
  (:export
   :api-request
   :rest-ws-auth
   :define-slack-endpoint-and-function))

(in-package :cl-slack.client)

;; Rest API Functions
(defparameter *default-headers* '(("Content-type" . "application/json")))

; https://api.slack.com/
(defparameter *base-url* "https://slack.com/api/"
              "Base url for requests to slack.")

(defparameter *slack-endpoints* (make-hash-table :test 'equal)
              "Hashtable to hold slack endpoints and their rest method")

(defmacro define-slack-endpoint-and-function (key value method name &optional headers)
  "Define a new Slack API endpoint, store it in the hash table, and create a corresponding function."
  `(progn
    ;; Add the endpoint to the hash table
    (setf (gethash ,key *slack-endpoints*) (list :value ,value :method ,method))

    ;; Define the function for the Slack API endpoint
    (defun ,name (&rest args)
      (let* ((endpoint (gethash ,key *slack-endpoints*))
             (url (getf endpoint :value))
             (method (getf endpoint :method)))
        (apply #'api-request
            method ;; Use the method from the hash table
          url ;; Use the URL from the hash table
          :headers ,(or headers '*default-headers*)
          args)))
    ;; Auto export the function
    (export ',name)))

;; Define Slack API endpoints and corresponding functions
(define-slack-endpoint-and-function :apps-connections-open "apps.connections.open" :post apps-connections-open)
(define-slack-endpoint-and-function :chat-post-message "chat.postMessage" :post post-chat-msg)
(define-slack-endpoint-and-function :bots-info "bots.info" :get bots-info)
(define-slack-endpoint-and-function :auth-test "auth.test" :post get-auth-test)

;; Client interface
(defun get-r (url &optional headers)
  "Send a get request to slack."
  (dex:get url :headers headers))

(defun post-r (url data &optional headers)
  "Send a post request to slack."
  (dex:post url :content (stringify data) :headers headers))

(defun api-request (method endpoint &key (base-url *base-url*) (headers nil) (data nil) (token nil))
  "Make an api request to slack"
  ; Push the authorization header onto the headers with the bearer token
  (let ((url (format nil "~A~A" base-url endpoint)))
    (when token
          (push `("Authorization" . ,(format nil "Bearer ~A" token)) headers))
    (handler-case
        (switch (method :test #'eq)
          (:get
           (get-r url headers))
          (:post
           (parse (post-r url data headers))))
      (dex:http-request-bad-request (e)
                                    (format *error-output* "Bad request: ~A" (dex:response-status e)))
      (dex:http-request-failed (e)
                               (format *error-output* "Request failed: ~A, Status: ~D" e (dex:response-status e)))
      (error (e)
        (format *error-output* "Unexpected error: ~A" e)))))

(defun rest-ws-auth (token)
  "Authenticate with Slack and receive a WS URL in response."
  (let ((response (apps-connections-open :token token)))
    (if (gethash "ok" response)
        (gethash "url" response) ; Return the URL
        (error (format ; Throw an error
                   nil
                 "Failed to authenticate with slack: ~A"
                 (gethash "error" response))))))
