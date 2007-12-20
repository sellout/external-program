;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.gnu.org/software/clisp/impnotes/shell.html

(defstruct external-process
  in-stream
  out-stream)

(defmethod run (program args &key input if-input-does-not-exist output if-output-exists error if-error-exists)
  (let ((result (ext:run-program program :arguments args
                                 :input (if (eq input t) :terminal input)
                                 :output (if (eq output t) :terminal output)
                                 :if-output-exists if-output-exists
                                 :wait t)))
    (values (if result :exited :signaled) result)))


(defmethod start (program args &key pty input if-input-does-not-exist output if-output-exists error if-error-exists status-hook)
  (multiple-value-bind (primary-stream input-stream output-stream)
      (ext:run-program program :arguments args
                       :input (if (eq input t) :terminal input)
                       :output (if (eq output t) :terminal output)
                       :if-output-exists if-output-exists
                       :wait nil))
  (cond ((eq input output :stream)
         (close primary-stream)
         (make-external-process :in-stream input-stream
                                :out-stream output-stream))
        ((eq input :stream)
         (make-external-process :in-stream primary-stream))
        ((eq output :stream)
         (make-external-process :out-stream primary-stream))))

(defmethod process-input-stream (process)
  (external-process-in-stream process))

(defmethod process-output-stream (process)
  (external-process-out-stream process))

(defmethod process-error-stream (process)
  nil)

(defmethod process-p (process)
  (typep process 'external-process))
