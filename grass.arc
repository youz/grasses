;;;
;;; grass.arc - Grass interpreter & compiler for Arc
;;; https://github.com/youz/grasses
;;;
;;; Copyright (C) 2012 Yousuke Ushiki  <citrus.yubeshi@gmail.com>
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

;; parser
(def parse-grass (src)
  (withs (parse-apps (afn (str)
                       (unless (is str "")
                         (withs (f (or (pos #\w str) (err "syntax error"))
                                 a (or (pos #\W (cut str f)) (- len.str f)))
                           `((app ,(- f 1) ,(- a 1)) ,@(self:cut str (+ a f))))))
          parse-abs (fn (str)
                      (let arity (or (pos #\W str) len.str)
                        `(abs ,arity ,(parse-apps (cut str arity))))))
    (mappend [(if (is _.0 #\w) list:parse-abs parse-apps) _]
             (tokens (keep [find _ "wWvV"] src) [find _ "vV"]))))

;; interpreter
(def eval-code (code env)
  (let ((insn a b c) . rest) code
    (case insn
      abs (eval-code rest (cons (list 'abs a b env) env))
      app (with (fun env.a arg env.b)
            (if (isa fun 'int)
                 (if (is fun arg)
                     (rfn ctrue (x) (fn (y) x))
                     (rfn cfalse (x) (fn (y) y)))
                (isa fun 'fn)
                 (eval-code rest (cons (fun arg) env))
                (and acons.fun (is fun.0 'abs))
                 (withs ((tag arity code dmp) fun
                         v (if (is --.arity 0)
                               (eval-code code (cons arg dmp))
                               `(abs ,arity ,code (,arg ,@dmp))))
                   (eval-code rest (cons v env)))
                (err "illegal state")))
      nil env.0
      (err "broken code"))))

(def run-grass (src)
  (with (code (parse-grass src)
         env (list (rfn grass-write (c) (pr:coerce c 'char) c)
                   (rfn grass-succ (c) (mod ++.c 256))
                   119
                   (rfn grass-read (a) (or (readb) a))))
    (if (is 'abs (car:last code))
        (eval-code (+ code '((app 0 0))) env)
        (eval-code code env))))


;; compiler
(def abs->arc (abst env0)
  (let (tag arity code) abst
    (unless (is tag 'abs) (err "abs required, but got : " tag))
    (if (> arity 0) (w/uniq arg
                      `(fn (,arg) ,(abs->arc `(abs ,(- arity 1) ,code) (cons arg env0))))
        (no code)   env0.0
        ((rfn rec (code env)
           (withs (((tag a b) . rest) code
                   arg (uniq)
                   body (case tag
                          abs (abs->arc `(abs ,a ,b) env)
                          app (list env.a env.b)
                          (err "abs or app required, but got : " tag)))
             (if rest
                 `((fn (,arg)
                     ,(rec rest (cons arg env)))
                   ,body)
                 (is tag 'app) body
                 `((fn (,arg) (,arg ,arg)) ,body))))
         code env0))))

(def grass->arc (src)
  (write
   `(withs (ctrue (fn (x) (fn (y) x))
            cfalse (fn (x) (fn (y) y))
            charfn  [fn ((o arg))
                      (if no.arg _ (is _ (errsafe:arg)) ctrue cfalse)]
            grass-succ  [charfn:mod (+ (_) 1) 256]
            grass-write [do (pr:coerce (_) 'char) _]
            w charfn.119
            grass-read  [iflet n (readb) charfn.n _])
      ,(abs->arc `(abs 0 ,(parse-grass src))
                 '(grass-write grass-succ w grass-read)))))
