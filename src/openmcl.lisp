;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://openmcl.clozure.com/Doc/External_002dProgram-Dictionary.html

(defmethod run (program args &rest rest)
  (process-status (apply #'ccl:run-program program args :wait t rest)))

(defmethod start (program args &rest rest)
  (apply #'ccl:run-program program args :wait nil rest))

(defmethod signal-process ((process ccl:external-process) signal)
  (ccl:signal-external-process process (if (keywordp signal)
                                           (cdr (assoc signal *signal-mapping*))
                                           signal)))

(defmethod process-id ((process ccl:external-process))
  (ccl:external-process-id process))

(defmethod process-input-stream ((process ccl:external-process))
  (ccl:external-process-input-stream process))

(defmethod process-output-stream ((process ccl:external-process))
  (ccl:external-process-output-stream process))

(defmethod process-error-stream ((process ccl:external-process))
  (ccl:external-process-error-stream process))

(defmethod process-status ((process ccl:external-process))
  (ccl:external-process-status process))

(defmethod process-p ((process ccl:external-process))
  t)
