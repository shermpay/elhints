#!/usr/bin/env -S emacs -Q -l
;; -*- lexical-binding: t -*-

(defvar arg0)
(setq arg0 (pop argv))

(unless arg0
  (error "Missing arg"))

(add-to-list 'load-path ".")

(require 'elhints)

(setopt elhints-elisp-grammar-source-dir arg0)

(dolist (file (directory-files "./ui-tests" t))
  (unless (member file '("." ".."))
	(find-file file)
	(emacs-lisp-mode)
	(elhints-mode)))

;; Local Variables:
;; flymake-diagnostic-functions: (elisp-flymake-byte-compile t)
;; End:
