#|
  This file is a part of lambda_sakura project.
  Copyright (c) 2014 lambda_sakura
|#

(in-package :cl-user)
(defpackage lambda_sakura-test-asd
  (:use :cl :asdf))
(in-package :lambda_sakura-test-asd)

(defsystem lambda_sakura-test
  :author "lambda_sakura"
  :license ""
  :depends-on (:lambda_sakura
               :cl-test-more)
  :components ((:module "t"
                :components
                ((:test-file "lambda_sakura"))))

  :defsystem-depends-on (:cl-test-more)
  :perform (test-op :after (op c)
                    (funcall (intern #. (string :run-test-system) :cl-test-more)
                             c)
                    (asdf:clear-system c)))
