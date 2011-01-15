;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.sbcl.org/manual/Running-external-programs.html

(defun convert-environment (rest environment replace-environment-p)
  (let ((env (reformat-environment environment)))
    (setf (getf rest :environment)
          (if replace-environment-p
              (append env '("PATH=''"))
              (append env (sb-ext:posix-environ)))))
  (remf rest :replace-environment-p)
  rest)

(defmethod run
    (program args
     &rest rest &key environment replace-environment-p &allow-other-keys)
  (process-status (apply #'sb-ext:run-program
                         program (stringify-args args) :search t :wait t
                         (convert-environment rest
                                              environment
                                              replace-environment-p))))

(defmethod start
    (program args
     &rest rest &key environment replace-environment-p &allow-other-keys)
  (apply #'sb-ext:run-program program (stringify-args args) :search t :wait nil
         (convert-environment rest environment replace-environment-p)))

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
