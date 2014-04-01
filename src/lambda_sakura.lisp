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

(annot:enable-annot-syntax)
(series::install)

@export
(defun get-argv ()
  #+sbcl sb-ext:*posix-argv*
  #+ecl(ext:command-args)
  #+ccl ccl:*command-line-argument-list*
  #-(or sbcl ecl ccl)(error "no argv"))

@export
(defun get-pid ()
  #+sbcl(sb-posix:getpid)
  #+ecl(ext:getpid)
  #+ccl(ccl::getpid)
  #-(or sbcl ecl ccl)(error "no getpid"))

@export
(defun chdir (pathspec)
  "change current directory"
  (setf *default-pathname-defaults* (truename pathspec))
  #+sbcl(sb-posix:chdir pathspec)
  #+ecl(ext:chdir pathspec)
  #+ccl(ccl::%chdir (namestring pathspec))
  #-(or ecl sbcl ccl)(error "no chdir"))

@export
(defun pwd ()
  "get current directory"
  (get-cwd))

@export
(defun get-cwd ()
  #+sbcl(sb-posix:getcwd)
  #+ecl(namestring (ext:getcwd))
  #+ccl(namestring (current-directory))
  #-(or sbcl ecl ccl)(error "no getcwd"))

@export
(defun current-directory ()
  (let ((cwd (get-cwd)))
    (if cwd
	(pathname (ensure-trailing-slash cwd))
	(error "Could not get current directory."))))

@export
(defun mkdir (dir &optional (mode #o755))
  #+sbcl(sb-posix:mkdir dir mode)
  #+ccl(ccl::%mkdir dir mode)
  #-(or sbcl ccl)(error "no mkdir"))

@export
(defun homedir ()
  (user-homedir-pathname))

@export
(defun home-relative (path)
  (merge-pathnames (parse-namestring path) (homedir)))

@export
(defun getuid ()
  #+sbcl(sb-unix:unix-getuid)
  #+ccl(ccl::getuid)
  #-(or sbcl ecl ccl)(error "no getuid"))

@export
(defun rootp ()
  (zerop (get-uid)))

@export
(defun exec-command (command &rest args)
  (with-output-to-string (out)
    #+sbcl
    (sb-ext:run-program command args :search (get-env "PATH") :output out)))

@export
(defun run-program (program args &key capture-output-p)
  #+sbcl(let ((result (sb-ext:run-program program
					  (ensure-list args)
					  :wait t :search t
					  :input t
					  :output (if capture-output-p
						      :stream
						      t))))
	  (values (sb-ext:process-exit-code result)
		  (sb-ext:process-output result)))
  #-(or sbcl)(error "no run-program"))

@export
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

(defun ensure-trailing-slash (path)
  (if (or (string= "" path)
	  (char= (aref path (1- (length path))) #\/))
      path
      (concatenate 'string path "/")))
