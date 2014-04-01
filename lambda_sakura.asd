#|
  This file is a part of lambda_sakura project.
  Copyright (c) 2014 lambda_sakura
|#

#|
  Author: lambda_sakura
|#

(in-package :cl-user)
(defpackage lambda_sakura-asd
  (:use :cl :asdf))
(in-package :lambda_sakura-asd)

(defsystem lambda_sakura
  :version "0.1"
  :author "lambda_sakura"
  :license "MIT"
  :depends-on (:cl-japanese-streams
	       :cl-ppcre
	       :cl-containers
	       :cl-annot
	       ;; :lisp-interface-library
	       :alexandria
	       :anaphora
	       :split-sequence
	       :series
	       :iterate
	       :kmrcl
	       :cl-interpol)
  :components ((:module "src"
                :components
                ((:file "lambda_sakura"))))
  :description "lambda_sakura's basic utilities for Common Lisp."
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op lambda_sakura-test))))
