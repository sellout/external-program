(defpackage external-program-tests
  (:use #:cl #:external-program))
;;; FIXME: need to add a testing framework

(in-package #:external-program-tests)

;;; FIXME: should probably signal a condition if program isn't found
;;; ... but can't guarantee that 71 isn't returned by the program
;;; itself ...
(defun should-not-have-access-to-shell-builtins ()
  (multiple-value-bind (status code)
      (external-program:run "source" '("foo"))
    (and (eq status :exited) (= code 71))))

(defun should-discover-programs-in-path ()
  (multiple-value-bind (status code)
      (external-program:run "which" '("which"))
    (and (eq status :exited) (= code 0))))

(defun should-be-able-to-use-pathname-as-program ()
  (multiple-value-bind (status code)
      (external-program:run (make-pathname :name "which") '("which"))
    (and (eq status :exited) (= code 0))))

(defun should-be-able-to-use-pathnames-as-args ()
  (multiple-value-bind (status code)
      (external-program:run "which" (list (make-pathname :name "which")))
    (and (eq status :exited) (= code 0))))

(defun should-be-able-to-use-numbers-as-args ()
  (multiple-value-bind (status code)
      (external-program:run "grep" (list "-C" 3 "should" (path-to-file)))
    (and (eq status :exited) (= code 0))))

(defun environment-vars-should-override-existing ()
  (multiple-value-bind (status code)
      (external-program:run "which" '("which") :environment '(("PATH" . "")))
    (and (eq status :exited) (/= code 0))))

(defun empty-env-should-erase-all ()
  (multiple-value-bind (status code)
      (external-program:run "which" '("which") :replace-environment-p t)
    (and (eq status :exited) (= code 71))))