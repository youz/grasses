;;;
;;; grass.lisp - Grass interpretor & compiler for Common Lisp
;;; https://github.com/youz/grasses
;;;
;;; Copyright (C) 2020 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
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


(declaim (optimize (speed 3) (debug 0) (safety 0)))

(defpackage #:grass
  (:use #:cl)
  (:export #:parse
           #:run
           #:run-string
           #:run-file
           #:compile-grass
           #:compile-grass-string
           #:compile-grass-file))

(in-package #:grass)

(defstruct app
  (fun 0 :type fixnum :read-only t)
  (arg 0 :type fixnum :read-only t))

(defstruct abst
  (codelen 0 :type fixnum)
  (code (make-array 8
		    :initial-element (make-app :fun 0 :arg 0)
		    :element-type '(or abst app)
		    :adjustable t
		    :fill-pointer 0)
	:type (vector (or app abst))))
(deftype insn () '(or app abst))

(declaim (ftype (function (abst app) abst) append-abst))
(defun abst-append (abst app)
  (vector-push-extend app (abst-code abst))
  (incf (abst-codelen abst))
  abst)

(defstruct prim
  (name "" :type string :read-only t)
  (fun nil :type (function (t) t) :read-only t))

(defstruct fn
  (abst nil :type abst :read-only t)
  (env nil :type (cons (or fn prim fixnum)) :read-only t))

(deftype value () '(or fixnum fn prim function))
(deftype env () '(cons value))
(defgeneric apply-value (fun arg))

(declaim (ftype (function (stream) (function (value) value)) make-prim-out make-prim-in))
(defun make-prim-out (output-stream)
  (declare (type stream output-stream))
  (lambda (a)
    (declare (type value a))
    (unless (integerp a)
      (error "Out: ~A is not a char" a))
    (princ (code-char a) output-stream)
    a))

(defun make-prim-in (input-stream)
  (declare (type stream input-stream))
  (lambda (a)
    (declare (type value a))
    (let ((c (read-char input-stream nil nil)))
      (if c (char-code c) a))))

(declaim (ftype (function (value) value) prim-succ prim-true prim-false))
(defun prim-succ (a)
  (unless (integerp a)
    (error "Succ: ~A is not a char" a))
  (mod (1+ a) 256))

(defun prim-true (x)
  (make-prim :name "const a" :fun (lambda (y) (declare (ignore y)) x)))

(defun prim-false (x)
  (declare (ignore x))
  (make-prim :name "id" :fun (lambda (y) y)))

(defvar *ctrue* (make-prim :name "true" :fun #'prim-true))
(defvar *cfalse* (make-prim :name "false" :fun #'prim-false))

(declaim (ftype (function (abst (cons value)) value) eval-code))
(defun eval-code (abst env)
  (let ((e env)
	(code (abst-code abst))
	(codelen (abst-codelen abst)))
    (tagbody
      :start
      (loop
	for i from 0 to (1- codelen)
	for insn = (aref code i)
	do
	(cond ((app-p insn)
	       (let ((f (nth (1- (app-fun insn)) e))
		     (a (nth (1- (app-arg insn)) e)))
		 (if (and (= i (1- codelen)) (fn-p f))
		     (let ((abst (fn-abst f)))
		       (setq code (abst-code abst)
			     codelen (abst-codelen abst)
			     e (cons a (fn-env f)))
		       (go :start))
		   (push (apply-value f a) e))))
	      ((abst-p insn)
	       (push (make-fn :abst insn :env e) e)))))
    (car e)))

(defmethod apply-value ((f fixnum) (a fixnum))
  (if (= f a) *ctrue* *cfalse*))

(defmethod apply-value ((f fixnum) (not-a-number t))
  *cfalse*)

(defmethod apply-value ((p prim) (a t))
  (declare (type value a))
  (funcall (prim-fun p) a))

(defmethod apply-value ((f fn) (a t))
  (declare (type value a))
  (let ((env (cons a (fn-env f))))
    (eval-code (fn-abst f) env)))


(defun lex (s)
  (let ((result nil)
	(cc nil)
	(n 0))
    (loop
      for c = (read-char s nil nil)
      while c
      for rc = (case c (#\ｗ #\w) (#\Ｗ #\W) (#\ｖ #\v) (t c))
      do (cond ((eq rc cc) (incf n))
	       ((find rc "wWv")
		(when cc (push (cons cc n) result))
		(setq cc rc n 1))
	       (t ;nop
		)))
    (push (cons cc n) result)
    (setq result (member #\w result :key #'car))
    (unless result
      (error "unexpected eof"))
    (nreverse result)))

(defun parse (s)
  (let ((al (lex s))
	(main (make-abst)))
    (labels ((read-app ()
	       (let ((f (pop al))
		     (a (pop al)))
		 (unless (eq (car a) #\w)
		   (error "unexpected 'v' (f: ~A, a: ~A)" f a))
		 (make-app :fun (cdr f) :arg (cdr a))))
	     (read-abst ()
	       (let ((arity (cdr (pop al)))
		     (abst (make-abst)))
		 (loop while (not (or (null al) (eq #\v (caar al))))
		   do (abst-append abst (read-app)))
		 (when al (pop al)) ; skip v
		 (dotimes (i (1- arity))
		   (let ((outer (make-abst)))
		     (abst-append outer abst)
		     (setq abst outer)))
		 abst)))
      (loop while al
	do (case (caar al)
	     (#\w (abst-append main (read-abst)))
	     (#\W (abst-append main (read-app)))
	     (#\v (pop al))))
      main)))

(defun parse-string (str)
  (with-input-from-string (s str)
    (parse s)))

(declaim (ftype (function (stream stream) env) initial-env))
(defun initial-env (stdin stdout)
  (list (make-prim :name "Out" :fun (make-prim-out stdout))
	(make-prim :name "Succ" :fun #'prim-succ)
	119
	(make-prim :name "In" :fun (make-prim-in stdin))))

(defun run (s &key (stdin *standard-input*) (stdout *standard-output*))
  (let ((main (parse s)))
    (abst-append main (make-app :fun 1 :arg 1))
    (eval-code main (initial-env stdin stdout))
    t))

(defun run-string (str &key (stdin *standard-input*) (stdout *standard-output*))
  (with-input-from-string (s str)
    (run s :stdin stdin :stdout stdout)))

(defun run-file (path &key (stdin *standard-input*) (stdout *standard-output*))
  (with-open-file (s path :direction :input :external-format :utf-8)
    (run s :stdin stdin :stdout stdout)))


;;; grass to lisp compiler

(defmethod apply-value ((p function) (a t))
  (declare (type value a))
  (funcall p a))

(defgeneric compile-insn (i e))
(defmethod compile-insn ((i app) e)
  (declare (type (cons symbol) e))
  `(apply-value ,(nth (1- (app-fun i)) e) ,(nth (1- (app-arg i)) e)))

(defmethod compile-insn ((i abst) e)
  (declare (type (cons symbol) e))
  (let* ((code (abst-code i))
	 (codelen (abst-codelen i))
	 (asym (gensym))
	 (e (cons asym e)))
    (case codelen
      (0 '#'identity)
      (1 `(lambda (,asym) ,(compile-insn (aref code 0) e)))
      (t (let ((binds (loop
			for i from 0 to (- codelen 2)
			for sym = (gensym)
			collect (list sym (compile-insn (aref code i) e))
			do (setq e (cons sym e)))))
	   `(lambda (,asym)
	      (let* ,binds ,(compile-insn (aref code (1- codelen)) e))))))))

(defun compile-grass (s func-name)
  (let ((main (parse s)))
    (abst-append main (make-app :fun 1 :arg 1))
    (let* ((sym-succ (make-symbol "Succ"))
	   (sym-w (make-symbol "w"))
	   (sym-in (make-symbol "In"))
	   (esyms (list sym-succ sym-w sym-in))
	   (top (compile-insn main esyms))
	   (sym-out (caadr top))
	   (body (caddr top)))
      `(defun ,func-name (&key (stdin *standard-input*) (stdout *standard-output*))
	 (let* ((,sym-out (make-prim-out stdout))
		(,sym-succ #'prim-succ)
		(,sym-w 119)
		(,sym-in (make-prim-in stdin)))
	   ,body
	   t)))))

(defun compile-grass-string (str func-name)
  (with-input-from-string (is str)
    (compile-grass is func-name)))

(defun compile-grass-file (path func-name)
  (with-open-file (is path :direction :input :external-format :utf-8)
    (compile-grass is func-name)))
