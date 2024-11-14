(defpackage :cl-slack.util
  (:use :cl)
  (:export
   :get-nested-value
   :format-flush
   :print-flush
   :print-hashtable))
(in-package :cl-slack.util)

;; Utility Functions
(defun get-nested-value (hash &rest keys)
  "Retrieve the value from a nested hash table given a sequence of keys."
  (reduce (lambda (acc key)
            (gethash key acc))
      keys
    :initial-value hash))

(defun format-flush (stream control-string &rest args)
  "Mimics `format`, but flushes the output stream after formatting."
  (apply #'format stream control-string args)
  (finish-output stream))

(defun print-flush (message)
  (print message)
  (finish-output *standard-output*))

(defun print-hashtable (table &optional (indent-level 0))
  "Recursively print the contents of a hashtable. 
  Handles nested hashtables and prints out yaml."
  (let ((indent (make-string indent-level :initial-element #\space)))
    (maphash (lambda (key value)
               (format-flush t "~a~a: " indent key)
               (if (hash-table-p value)
                   (progn
                    (format-flush t "~%")
                    (print-hashtable value (+ indent-level 2)))
                   (format-flush t "~a~%" value)))
             table)))