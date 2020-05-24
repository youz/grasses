(unless (cdr sb-ext:*posix-argv*)
  (format t "usage: sbcl --script rungrass_c.lisp <program.grass>")
  (quit))

(setq sb-impl::*default-external-format* :utf-8)
(load "grass.fasl")
#.(grass:compile-grass-file (cadr sb-ext:*posix-argv*) 'grass-main)
(grass-main)
