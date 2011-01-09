;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.sbcl.org/manual/Support-For-Unix.html

(defun reformat-env (env)
  "SBCL accepts vars as either (\"FOO=meh\" ...) or ((:foo . \"meh\")
  ...), but not ((\"FOO\" . \"meh\") ...), so we build up the first
  kind (since the second kind is potentially lossy)."
  ;; FIXME: probably need to escape single-quotes and backslashes
  (mapcar (lambda (var) (format nil "~a='~a'" (car var) (cdr var))) env))

(defun convert-environment (rest environment replace-environment-p)
  (let ((env (reformat-env environment)))
    (setf (getf rest :environment)
          (if replace-environment-p
              env
              (append env sb-ext:*current-environment*))))
  (remove-parameter :replace-environment-p rest)
  rest)

(defmethod run (program args &rest rest
                &key environment replace-environment-p &allow-other-keys)
  (process-status (apply #'sb-ext:run-program
                         program args :search t :wait t
                         (convert-environment rest
                                              enviromnent
                                              replace-environment-p))))

(defmethod start (program args &rest rest &key &allow-other-keys)
  (apply #'sb-ext:run-program program args :search t :wait nil
         (convert-environment rest enviromnent replace-environment-p)))

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
