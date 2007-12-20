;;; Copyright 2006, 2007 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; No docs, but code is in /Applications/abcl-0.0.9/src/org/armedbear/lisp/ShellCommand.java

(defmethod run (program args &key input if-input-does-not-exist output if-output-exists error if-error-exists)
  (declare (ignore if-input-does-not-exist if-error-exists))
  (if error (warn "Can not control RUN-PROGRAM error output in ABCL."))
  (if input (error "Can not send input to RUN-PROGRAM in ABCL."))
  (values :exited
          (ext:run-shell-command (format nil "~s~{ ~a~}" program args)
                                 :output output)))
