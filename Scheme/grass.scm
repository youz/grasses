#!/usr/bin/env gosh
;;;
;;; grass.scm - Grass interpreter for Gauche
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

(use text.tr)

(define (succ c)
  (unless (integer? c)
    (errorf "Succ: ~A is not a char" c))
  (mod (+ c 1) 256))

(define (out c)
  (unless (integer? c)
    (errorf "Out: ~A is not a char" c))
  (write-char (integer->char c))
  c)

(define (in eof)
  (let1 b (read-byte)
    (if (eof-object? b) eof b)))

(define church-true (^x (^y x)))
(define church-false (^x (^y y)))

(define eval-code
  (rec (r code env dump)
    (cond
     [(null? code)
      (let1 ret (car env)
        (if (null? dump)
            ret
            (r (caar dump) (cons ret (cdar dump)) (cdr dump))))]
     [(eq? (caar code) 'app)
      (let* ((app (car code))
             (rest (cdr code))
             (f (list-ref env (cadr app)))
             (a (list-ref env (caddr app))))
         (cond
          [(pair? f)
           (let ((f-arity (car f))
                 (f-code (cadr f))
                 (f-env (caddr f)))
             (if (= f-arity 1)
                 (if (null? rest)
                     (r f-code (cons a f-env) dump)
                     (r f-code (cons a f-env) (cons (cons rest env) dump)))
                 (let1 fn (list (- f-arity 1) f-code (cons a f-env))
                   (r rest (cons fn env) dump))))]
          [(integer? f)
            (let1 b (if (and (integer? a) (= f a)) church-true church-false)
              (r rest (cons b env) dump))]
          [else
           (r rest (cons (f a) env) dump)]
           ))]
     [(eq? (caar code) 'abs)
      (let* ((abs (car code))
             (a-arity (cadr abs))
             (a-code (cddr abs)))
        (r (cdr code) (cons (list a-arity a-code env) env) dump))]
     [else
      (errorf "bug: runtime error~%code: ~S~%env : ~S~%dump: ~S"
              code env dump)])))

(define (parse src)
  (define (%parse s)
    (rxmatch-case s
      (#/^$/ (#f) ())
      (#/^(w+)(.*)/ (#f a r)
       (list `(abs ,(string-length a) . ,(%parse r))))
      (#/^(W+)(w+)(.*)/ (#f m n r)
       (cons `(app ,(- (string-length m) 1) ,(- (string-length n) 1))
             (%parse r)))
      (else (error "syntax error"))))
  (let1 s (regexp-replace-all* (string-tr src "Ｗｗｖ" "Wwv") #/[^Wwv]/ "" #/^[^w]+/ "")
    (append-map %parse (string-split s #\v))))

(define (run-grass src)
  (eval-code (parse src) (list out succ 119 in) '((((app 0 0))))))

(define (main args)
  (when (null? (cdr args))
    (format #t "usage: gosh ~A <source.grass>" (car args))
    (exit 1))
  (call-with-input-file (cadr args)
    (^p (run-grass (port->string p))))
  0)
