;;; elhints-test.el --- Tests for elhints  -*- lexical-binding: t; -*-

(require 'elhints)

(require 'ert)
(require 'generator)
(require 'treesit)


;;; Code:

(ert-deftest parse-arglist-test ()
  "Test ELHINTS--PARSE-ARGLIST."
  (should (equal (=--parse-arglist '(a b) '(0 1))
				 (list (=--arg-info-make 0 "a")
					   (=--arg-info-make 1 "b")))))

(ert-deftest call-node->info-test ()
  "Test call-node->info."
  (ert-with-test-buffer (:name "test")
	(insert "(1+ 42)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=--call-info-make :start-pos 1
										:fun-symbol '1+
										:arg-info-list (list (=--arg-info-make 5 "number"))))))))


(ert-deftest call-infos-iter-test ()
  (ert-with-test-buffer (:name "no-funcall")
	(insert "42")
	(should (eq (condition-case condition
					(iter-next (=--call-infos-iter))
				  (iter-end-of-sequence (car condition)))
				'iter-end-of-sequence)))


  (ert-with-test-buffer (:name "one funcall")
	(insert "(1+ 99)")
	(let ((it (=--call-infos-iter)))
	  (should (equal (iter-next it)
					 (=--call-info-make
					  :start-pos 1
					  :fun-symbol '1+
					  :arg-info-list (list (=--arg-info-make 5 "number")))))
	  (should (eq (condition-case x
					  (iter-next it)
					(iter-end-of-sequence (car x)))
				  'iter-end-of-sequence))))

  (ert-with-test-buffer (:name "two funcalls")
	(insert "(1+ 99) (1- 100)")
	(let ((lst))
	  (iter-do (x (=--call-infos-iter))
		(push x lst))
	  (setq lst (nreverse lst))
	  (should (equal lst
					 (list
					  (=--call-info-make
					   :start-pos 1
					   :fun-symbol '1+
					   :arg-info-list (list (=--arg-info-make 5 "number")))
					  (=--call-info-make
					   :start-pos 9
					   :fun-symbol '1-
					   :arg-info-list (list (=--arg-info-make 13 "number"))))))))

  (ert-with-test-buffer (:name "filter remove all")
	(insert "(1+ 99) (1- 100)")
	(let ((lst))
	  (iter-do (x (=--call-infos-iter (current-buffer) (point-min) (point-max) (lambda (&rest args) nil)))
		(push x lst))
	  (setq lst (nreverse lst))
	  (should (eq lst nil)))))


;; Local Variables:
;; read-symbol-shorthands: (("=-" . "elhints-"))
;; End:

(provide 'elhints-test)

;;; elhints-test.el ends here
