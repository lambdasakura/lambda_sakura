#|
  This file is a part of lambda_sakura project.
  Copyright (c) 2014 lambda_sakura
|#

(in-package :cl-user)
(defpackage lambda_sakura
  (:use :cl
	:alexandria
	:anaphora
	:cl-containers
	:cl-japanese-streams
	:split-sequence
	:series))
(in-package :lambda_sakura)

(series::install)
