;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://common-lisp.net/project/cmucl/doc/cmu-user/extensions.html#toc46

(defmethod run (program args &key pty input if-input-does-not-exist output if-output-exists error if-error-exists status-hook &rest rest)
  (process-status (apply #'ext:run-program program args :wait t rest)))

(defgeneric start (program args &key pty input if-input-does-not-exist output if-output-exists error if-error-exists status-hook &rest rest)
  (apply #'ext:run-program program args :wait nil rest))

(defgeneric signal-process (process signal)
  (ext:process-kill process (cdr (assoc signal *signal-mapping*))))

(defmethod process-id (process)
  (ext:process-pid process))

(defmethod process-input-stream (process)
  (ext:process-input process))

(defmethod process-output-stream (process)
  (ext:process-output process))

(defmethod process-error-stream (process)
  (ext:process-error process))

(defmethod process-status (process)
  (values (ext:process-status process) (ext:process-exit-code process)))

(defmethod process-p (process)
  (ext:process-p process))
