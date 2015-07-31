(in-package #:external-program)

(defun rename-parameter (from-name to-name rest)
  (setf (getf rest to-name) (getf rest from-name))
  (remf rest from-name)
  rest)

(defun stringify-args (args)
  (mapcar (lambda (arg)
            (typecase arg
              (string                arg)
              (sequence              (coerce arg 'string))
              ((or symbol character) (string arg))
              (number                (princ-to-string arg))
              (pathname              (namestring arg))))
          args))

(defun environment-list (environment)
  "Convert ENVIRONMENT alist to a POSIX environment list."
  (mapcar (lambda (var) (format nil "~a=~a" (car var) (cdr var)))
          environment))

(defun embed-environment (program args environment replace-environment-p)
  (if (or environment replace-environment-p)
      (values "env"
              (append (when replace-environment-p (list "-i"))
                      (environment-list environment)
                      (cons program args)))
      (values program args)))

(defun make-shell-string (program args environment replace-environment-p)
  ;; FIXME Perform shell escaping (assuming shell is BASH?)
  (format nil "~:[~;env -i~] ~{~a ~}~a~{ ~s~}"
          replace-environment-p
          (environment-list environment)
          program
          (stringify-args args)))
