;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(defpackage external-program-system
  (:use #:cl #:asdf))

(in-package :external-program-system)

(defsystem external-program
  :author "Greg Pfeil <greg@technomadic.org>"
  :licence "LLGPL"
  :version "0.0.3"
  :components
  ((:module "src"
            :components
            ((:file "external-program")
             (:file #+allegro "allegro"
                    #+armedbear "armedbear"
                    #+clisp "clisp"
                    #+cmu "cmucl"
                    #+ecl "ecl"
                    ;; #+gcl "gcl"
                    ;; #+liquid "liquid"
                    #+lispworks "lispworks"
                    ;; #+lucid "lucid"
                    #+openmcl "openmcl"
                    #+sbcl "sbcl"
                    ;; #+scl "scieneer"
                    #-(or allegro armedbear clisp cmu ecl lispworks openmcl
                          sbcl)
                    "unsupported"
                    :depends-on ("external-program"))))))