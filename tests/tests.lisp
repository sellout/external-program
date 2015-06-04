(defpackage external-program-tests
  (:use #:cl #:external-program #:fiveam)
  (:shadow #:run))

(in-package #:external-program-tests)

(def-suite tests)

(in-suite tests)

(test should-discover-programs-in-path
  (multiple-value-bind (status code)
      (external-program:run "which" '("ls"))
    (is (eq :exited status))
    (is (= 0 code))))

(test should-be-able-to-use-pathname-as-program
  (multiple-value-bind (status code)
      (external-program:run (make-pathname :name "ls") '("."))
    (is (eq :exited status))
    (is (= 0 code))))

(test should-be-able-to-use-pathnames-as-args
  (multiple-value-bind (status code)
      (external-program:run "ls"
                            (list (merge-pathnames (make-pathname :name nil)
                                                   #.*compile-file-truename*)))
    (is (eq :exited status))
    (is (= 0 code))))

(test should-be-able-to-use-numbers-as-args
  (multiple-value-bind (status code)
      (external-program:run "grep"
                            '("-C" 3 "should" #.*compile-file-truename*))
    (is (eq :exited status))
    (is (= 0 code))))

(test should-allow-spaces-in-args
  (multiple-value-bind (status code)
      (external-program:run "grep"
                            '("should probably" #.*compile-file-truename*))
    (is (eq :exited status))
    (is (= 0 code))))

(test environment-vars-should-override-existing
  (multiple-value-bind (status code)
      (external-program:run "which" '("which") :environment '(("PATH" . "")))
    (is (eq :exited status))
    (is (/= 0 code))))

(test empty-env-should-erase-all
  (let* (status
         code
         (output
           (with-output-to-string (out)
             (multiple-value-setq (status code)
               #-(or clisp ecl)
               (external-program:run "/usr/bin/env" nil :output out :environment nil :replace-environment-p t)
               #+(or clisp ecl)
               (external-program:run "/usr/bin/env" nil :environment nil :replace-environment-p t)))))
    (is (eq :exited status))
    (is (= 0 code))
    #-(or clisp ecl)
    (is-false (search "=" output))))

#-(or clisp ecl)
(test environment-vars-should-be-set
  (let* ((environment '(("external program test var" . "test val")))
         status
         code
         (output
           (with-output-to-string (out)
             (multiple-value-setq (status code)
               #-(or clisp ecl)
               (external-program:run "env" nil :output out :environment environment)
               #+(or clisp ecl)
               (external-program:run "env" nil :environment environment)))))
    (is (eq :exited status))
    (is (= 0 code))
    #-(or clisp ecl)
    (is-true (search "external program test var=test val" output))))
