;;; Copyright 2006-2008 Greg Pfeil
;;; Distributed under the LLGPL (see LICENSE file)

(in-package :external-program)

;;;; Documentation at http://www.lispworks.com/documentation/lwl42/LWRM-U/html/lwref-u-421.htm

(defstruct external-process
  inputp
  outputp
  stream)

(defmethod run (program args &key input output error &allow-other-keys)
  (when error
    (warn "Can not control EXTERNAL-PROGRAM:RUN error output in LispWorks."))
  (when input
    (error "Can not send input to EXTERNAL-PROGRAM:RUN in LispWorks."))
  (values :exited
          (sys:call-system-showing-output (cons program args)
                                          :prefix ""
                                          :show-cmd nil
                                          :output-stream output
                                          :wait t)))

(defmethod start (program args &key input output error &allow-other-keys)
  (when error
    (warn "Can not control EXTERNAL-PROGRAM:RUN error output in ABCL."))
  (let ((direction (cond ((and (eq input :stream) (eq output :stream)) :io)
                         ((eq input :stream) :input)
                         ((eq output :stream) :output))))
    (if direction
        (make-external-process :inputp input :outputp output
                               :stream (sys:open-pipe (format nil "~s~{ ~a~}"
                                                              program args)
                                                      :direction direction))
        (sys:call-system-showing-output (cons program args)
                                        :prefix ""
                                        :show-cmd nil
                                        :output-stream output
                                        :wait nil))))

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
