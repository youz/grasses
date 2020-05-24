(unless (cdr sb-ext:*posix-argv*)
  (format t "usage: sbcl --script rungrass.lisp <program.grass>")
  (quit))

(setq sb-impl::*default-external-format* :utf-8)
(load "grass.fasl")
(grass:run-file (cadr sb-ext:*posix-argv*))
