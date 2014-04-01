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

(defun pwd ()
  "get current directory"
  #+sbcl
  (sb-posix:getcwd))

(defun chdir (pathspec)
  "change current directory"
  #+sbcl
  (sb-posix:chdir pathspec))

(defun get-env (name &optional default)
    #+CMU
    (let ((x (assoc name ext:*environment-list*
                    :test #'string=)))
      (if x (cdr x) default))
    #-CMU
    (or
     #+Allegro (sys:getenv name)
     #+CLISP (ext:getenv name)
     #+ECL (si:getenv name)
     #+SBCL (sb-unix::posix-getenv name)
     #+LISPWORKS (lispworks:environment-variable name)
     default))


(defun exec-command (command &rest args)
  (princ command)
  (princ args)
  (with-output-to-string (out)
    #+sbcl
    (sb-ext:run-program
     command
     args
     :search (get-env "PATH")
     :output out)))


