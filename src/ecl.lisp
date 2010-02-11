;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; No docs, but code is in ecl/src/c/unixsys.d

(defstruct external-process
  inputp
  outputp
  stream)

(defmethod run (program args &key input if-input-does-not-exist output if-output-exists error if-error-exists)
  (if (or output error)
      (warn "Can not control RUN-PROGRAM output in ECL."))
  (if input (error "Can not send input to RUN-PROGRAM in ECL."))
  (values :exited (si:system (format nil "~s~{ ~a~}" program args))))

(defmethod start (program args &key input if-input-does-not-exist output if-output-exists error if-error-exists)
  (if (eq error :stream)
      (error "ECL can not create a stream for error output."))
  (make-external-process :inputp (eq input :stream)
                         :outputp (eq output :stream)
                         :stream (ext:run-program program args
                                                  :wait nil
                                                  :input input
                                                  :output output
                                                  :error error)))

(defmethod process-input-stream (process)
  (if (external-process-inputp process)
      (external-process-stream process)))

(defmethod process-output-stream (process)
  (if (external-process-outputp process)
      (external-process-stream process)))

(defmethod process-error-stream (process)
  nil)

(defmethod process-p (process)
  (typep process 'external-process))
