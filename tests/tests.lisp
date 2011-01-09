(defpackage external-program-tests
  (:use #:cl #:external-program #:fiveam)
  (:shadow #:run))

(in-package #:external-program-tests)

(def-suite tests)

(in-suite tests)

;;; FIXME: should probably signal a condition if program isn't found
;;; ... but can't guarantee that 71 isn't returned by the program
;;; itself ...
(test should-not-have-access-to-shell-builtins
  (multiple-value-bind (status code)
      (external-program:run "source" '("foo"))
    (is (eq status :exited))
    (is (= code 71))))

(test should-discover-programs-in-path
  (multiple-value-bind (status code)
      (external-program:run "which" '("which"))
    (is (eq status :exited))
    (is (= code 0))))

(test should-be-able-to-use-pathname-as-program
  (multiple-value-bind (status code)
      (external-program:run (make-pathname :name "which") '("which"))
    (is (eq status :exited))
    (is (= code 0))))

(test should-be-able-to-use-pathnames-as-args
  (multiple-value-bind (status code)
      (external-program:run "which" (list (make-pathname :name "which")))
    (is (eq status :exited))
    (is (= code 0))))

(test should-be-able-to-use-numbers-as-args
  (multiple-value-bind (status code)
      (external-program:run "grep"
                            (list "-C" 3 "should" #.*compile-file-truename*))
    (is (eq status :exited))
    (is (= code 0))))

(test environment-vars-should-override-existing
  (multiple-value-bind (status code)
      (external-program:run "which" '("which") :environment '(("PATH" . "")))
    (is (eq status :exited))
    (is (/= code 0))))

(test empty-env-should-erase-all
  (multiple-value-bind (status code)
      (external-program:run "which" '("which") :replace-environment-p t)
    (is (eq status :exited))
    (is (= code 71))))
