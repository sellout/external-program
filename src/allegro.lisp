;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.franz.com/support/documentation/6.2/doc/os-interface.htm#subprocess-functions-1

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :osi))

(defmethod run
    (program args
     &key
     input if-input-does-not-exist output if-output-exists error if-error-exists
     environment replace-environment-p status-hook
     &allow-other-keys)
  (when status-hook (warn ":STATUS-HOOK is not supported by Allegro."))
  (let* ((input-stream (etypecase input
                         (stream input)
                         ((or pathname string)
                          (open input
                                :if-does-not-exist if-input-does-not-exist))
                         (null nil)
                         (boolean *standard-input*)))
         (output-stream (etypecase input
                          (stream output)
                          ((or pathname string)
                           (open output
                                 :direction :output
                                 :if-exists if-output-exists))
                          (null nil)
                          (boolean *standard-output*)))
         (error-stream (etypecase input
                         (stream error)
                         ((or pathname string)
                          (open error
                                :direction :output
                                :if-exists if-error-exists))
                         (null nil)
                         (boolean *standard-output*)
                         (symbol output-stream))))
    (values :exited
            (excl.osi:with-command-io
                ((make-shell-string program args
                                    environment replace-environment-p))
              (:input (stream)
                      (when input-stream
                        (make-echo-stream input-stream stream)))
              (:output (line)
                       (when output-stream (write-line line output-stream)))
              (:error-output (line)
                             (when error-stream
                               (write-line line error-stream)))))))
