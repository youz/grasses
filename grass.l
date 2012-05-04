;;; -*- mode:Lisp; package:grass -*-
;;;
;;; grass.l - Grass to Lisp compiler for xyzzy
;;; https://github.com/youz/grasses
;;;
;;; Copyright (C) 2008-2011 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
;;; All rights reserved.
;;;
;;; Grass language
;;; http://www.blue.sky.or.jp/grass/
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;; 1. Redistributions of source code must retain the above copyright
;;;    notice, this list of conditions and the following disclaimer.
;;; 2. Redistributions in binary form must reproduce the above copyright
;;;    notice, this list of conditions and the following disclaimer in
;;;    the documentation and/or other materials provided with the
;;;    distribution.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS''
;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
;;; THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS
;;; BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;

(provide "grass")

(defpackage "grass"
  (:use :lisp :editor))

(in-package :grass)
(export '(parse run to-lisp
	  enable-reader disable-reader))

;;; parser
(defun parse (str)
  (setq str (substitute-string str "[^WwVv‚v‚—‚u‚–]" ""))
  (map () (lambda (b a) (nsubstitute a b str)) "‚v‚—‚u‚–" "Wwvv")
  (setq str (subseq str (position #\w str)))
  (let ((result nil))
    (flet ((parse-apps (str)
	     (do ((p 0 (match-end 0)) (apps nil))
		 ((not (string-match "\\(W+\\)\\(w+\\)" str p))
		  (if (< p (length str))
		      (error "malformed application:~%~A" str)
		    (nreverse apps)))
	       (push `(:app ,(length (match-string 1)) ,(length (match-string 2))) apps))))
      (dolist (code (split-string str #\v))
	(case (char code 0)
	  (#\w (let ((apppos (position #\W code)))
		 (push `(:abs ,(or apppos (length code)) ,(if apppos (parse-apps code)))
		       result)))
	  (#\W (mapc #'(lambda (app) (push app result)) (parse-apps code)))))
      (nreverse result))))


;;; primitives
(defun make-char (code)
  (lambda (&optional f)
    (cond ((null f) code)
	  ((functionp f)
	   (if (= code (funcall f))
	       (lambda (x) (lambda (y) x))
	     (lambda (x) (lambda (y) y))))
	  (t (error "~S isn't a character object" f)))))

(defun make-out (os)
  (lambda (chfn)
    (princ (code-char (funcall chfn)) os)
    chfn))

(defun make-in (is)
  (lambda (arg)
    (let ((c (read-char is nil nil)))
      (if (null c) arg
	(make-char (char-code c))))))

(defvar succ
  (lambda (chr)
    (make-char (mod (1+ (funcall chr)) 256))))


;;; translator
(defun make-form (tree d &optional e)
  (let ((label (car tree))
	(arity (cadr tree))
	(code (caddr tree)))
    (unless (and (eq label :abs) (integerp arity) (listp code))
      (error "malformed abstraction code:~%~A" tree))
    (if (< 0 arity)
	(let* ((arg (gensym)) (dd `(,arg ,@d)))
	  `(lambda (,arg) ,(make-form `(:abs ,(1- arity) ,code) dd)))
      (let (acc (fin :abs))
	`(let*
	     ,(dolist (n code (reverse acc))
		(let ((label (car n))
		      (sym (gensym)))
		  (push sym d)
		  (setq fin label)
		  (cond
		   ((eq label :app)
		    (push `(,sym (funcall ,(nth (cadr n) d) ,(nth (caddr n) d))) acc))
		   ((eq label :abs)
		    (push `(,sym ,(make-form n (cdr d))) acc))
		   (t (error "malformed code:~%~A" code)))))
	   ,(if (and e (eq :abs fin))
		`(funcall ,#0=(car d) ,#0#)
	      #0#))))))


(defun to-lisp (str &key (run t) (output *standard-output*) (input *standard-input*))
  `(let* ((out (make-out ,output))
	  (in (make-in ,input))
	  (w (make-char 119)))
     ,(make-form `(:abs 0 ,(parse str)) '(out succ w in) run)))

(defun run (str &key (run t) (output *standard-output*) (input *standard-input*))
  (eval (to-lisp str :run run :output output :input input)))



;;;  reader
(defvar *grass-readtable* nil)
(defvar *backup-readtable* nil)

(defun sharp-w-reader (s c n)
  (let ((grasscode (if (peek-char t s nil nil) (format nil "w~A" (read s)) "w")))
    `(run ,grasscode :run ,(not (eql n 0)))))

(defun enable-reader ()
  (unless #1=*grass-readtable*
    (setq #1# (copy-readtable))
    (set-dispatch-macro-character #\# #\w #'sharp-w-reader #1#)
    (setq *backup-readtable* *readtable*))
  (setq *readtable* #1#)
  (values))

(defun disable-reader ()
  (when *backup-readtable*
    (setq *readtable* *backup-readtable*))
  (values))


#|
*usage*

user> (require "grass")
t
user> (grass:run "wWWwwww")
w
#<lexical-closure: (anonymous)>
user> (grass:to-lisp "wWWwwww")
(let* ((grass::out (grass::make-out #<buffer stream 10884568>))
       (grass::in (grass::make-in #<general-input-stream 10883764>))
       (grass::w (grass::make-char 119)))
  (let* ((#1=#:G237 (lambda (#:G238)
                      (let* ((#2=#:G239 (funcall grass::out grass::w)))
                        #2#))))
    (funcall #1# #1#)))
user> (grass:enable-reader)
user> (mapcar #ww '(1 2 3))
(1 2 3)
user> (si:closure-body #ww)
(lambda (#1=#:G231) (let* nil #1#))

|#
