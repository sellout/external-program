;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.sbcl.org/manual/Support-For-Unix.html

(defmethod run (program args &rest rest)
  (process-status (apply #'sb-ext:run-program
                         program args :search t :wait t rest)))

(defmethod start (program args &rest rest)
  (apply #'sb-ext:run-program program args :search t :wait nil rest))

(defmethod signal-process (process signal)
  (sb-ext:process-kill process (cdr (assoc signal *signal-mapping*))))

(defmethod process-input-stream (process)
  (sb-ext:process-input process))

(defmethod process-output-stream (process)
  (sb-ext:process-output process))

(defmethod process-error-stream (process)
  (sb-ext:process-error process))

(defmethod process-status (process)
  (values (sb-ext:process-status process) (sb-ext:process-exit-code process)))

(defmethod process-p (process)
  (sb-ext:process-p process))
