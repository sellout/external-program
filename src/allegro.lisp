;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.franz.com/support/documentation/6.2/doc/os-interface.htm#subprocess-functions-1

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

(defmethod run (program args &key input if-input-does-not-exist output if-output-exists error if-error-exists)
  (let ((input-stream (etypecase input
                        (stream input)
                        ((or pathname string)
                         (open input
                               :if-does-not-exist if-input-does-not-exist))
                        (null nil)
                        (boolean *standard-input*))))
    (multiple-value-bind (output-string error-string status)
        (excl.osi:command-output (format nil "~s~{ ~a~}" program args)
                                 :whole t
                                 :input input-stream)
      (typecase output
        (stream (write-sequence output-string output))
        ((or pathname string)
         (with-open-file (out output
                              :direction :output
                              :if-exists if-output-exists)
           (write-sequence output-string out)))
        (boolean (and output (write-sequence output-string *standard-output*))))
      (typecase error
        (stream (write-sequence error-string error))
        ((or pathname string)
         (with-open-file (err error
                              :direction :output
                              :if-exists if-error-exists)
           (write-sequence error-string err)))
        (boolean (and error (write-sequence error-string *error-output*))))
      (values :exited status))))
