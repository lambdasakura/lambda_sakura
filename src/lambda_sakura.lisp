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
  #+ccl ccl:*command-line-argument-list*
  #-(or sbcl ccl)(error "no argv"))

@export
(defun get-pid ()
  #+sbcl(sb-posix:getpid)
  #+ccl(ccl::getpid)
  #-(or sbcl ccl)(error "no getpid"))

@export
(defun chdir (pathspec)
  "change current directory"
  (setf *default-pathname-defaults* (truename pathspec))
  #+sbcl(sb-posix:chdir pathspec)
  #+ccl(ccl::%chdir (namestring pathspec))
  #-(or sbcl ccl)(error "no chdir"))

@export
(defun pwd ()
  "get current directory"
  (get-cwd))

@export
(defun get-cwd ()
  #+sbcl(sb-posix:getcwd)
  #+ccl(namestring (current-directory))
  #-(or sbcl ccl)(error "no getcwd"))

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
  #-(or sbcl ccl)(error "no getuid"))

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
  #+clisp (ext:getenv name)
  #+ccl(ccl:getenv name)
  #+sbcl (sb-unix::posix-getenv name)
  #-(or sbcl ccl clisp)(error "no getenv"))

(defun ensure-trailing-slash (path)
  (if (or (string= "" path)
	  (char= (aref path (1- (length path))) #\/))
      path
      (concatenate 'string path "/")))

@export
(defun string+ (&rest args)
  (apply #'concatenate 'string args))


@export
(defun split-seq (s &optional (delim #\,))
  (labels ((split-seq1 (s delim rel)
	     (if-let ((i1 (position delim s)))
	       (split-seq1 (subseq s (1+ i1)) delim
			   (pushnew (subseq s 0 i1) rel))
	       (nreverse (pushnew s rel)))))
    (split-seq1 s delim '())))
