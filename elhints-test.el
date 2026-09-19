;;; elhints-test.el --- Tests for elhints  -*- lexical-binding: t; -*-

(require 'elhints)

(require 'ert)
(require 'generator)
(require 'treesit)


;;; Code:

(ert-deftest call-node->info-test ()
  "Test call-node->info."
  (ert-with-test-buffer (:name "1 required arg")
	(insert "(1+ 42)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol '1+
									   :arg-info-vec (vector (=-arg-info-make 5 "number" "42")))))))

  (ert-with-test-buffer (:name "2 required arg")
	(insert "(eq 0 1)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol 'eq
									   :arg-info-vec (vector (=-arg-info-make 5 "obj1" "0")
															 (=-arg-info-make 7 "obj2" "1")))))))
  (ert-with-test-buffer (:name "1 optional arg 0 given")
	(insert "(pos-bol)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol 'pos-bol
									   :arg-info-vec (vector))))))
  (ert-with-test-buffer (:name "1 optional arg 1 given")
	(insert "(pos-bol 3)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol 'pos-bol
									   :arg-info-vec (vector (=-arg-info-make 10 "n" "3" :kind 'optional)))))))
  (ert-with-test-buffer (:name "(+)")
	(insert "(+)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol '+
									   :arg-info-vec (vector))))))

  (ert-with-test-buffer (:name "(+ 1)")
	(insert "(+ 1)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol '+
									   :arg-info-vec (vector (=-arg-info-make 4 "numbers-or-markers" "1" :kind 'rest)))))))

  (ert-with-test-buffer (:name "(+ 1 2)")
	(insert "(+ 1 2)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol '+
									   :arg-info-vec (vector (=-arg-info-make 4 "numbers-or-markers" "1" :kind 'rest)
															 (=-arg-info-make 6 "numbers-or-markers" "2" :kind 'rest)))))))

  (ert-with-test-buffer (:name "(format \"%s\" 42)")
	(insert "(format \"%s\" 42)")
	(let ((parser (treesit-parser-create 'elisp)))
	  (should (equal (=--call-node->info
					  (treesit-node-child (treesit-parser-root-node parser) 0))
					 (=-call-info-make :start-pos 1
									   :fun-symbol 'format
									   :arg-info-vec (vector (=-arg-info-make 9 "string" "\"%s\"")
															 (=-arg-info-make 14 "objects" "42" :kind 'rest))))))))

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
					 (=-call-info-make
					  :start-pos 1
					  :fun-symbol '1+
					  :arg-info-vec (vector (=-arg-info-make 5 "number" "99")))))
	  (should (eq (condition-case x
					  (iter-next it)
					(iter-end-of-sequence (car x)))
				  'iter-end-of-sequence))))

  (ert-with-test-buffer (:name "one funcall multi-args")
	(insert "(start-process \"name\" \"buf\" \"prog\")")
	(let ((it (=--call-infos-iter)))
	  (should (equal (iter-next it)
					 (=-call-info-make
					  :start-pos 1
					  :fun-symbol 'start-process
					  :arg-info-vec (vector (=-arg-info-make 16 "name" "\"name\"")
											(=-arg-info-make 23 "buffer" "\"buf\"")
											(=-arg-info-make 29 "program" "\"prog\"")))))
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
					  (=-call-info-make
					   :start-pos 1
					   :fun-symbol '1+
					   :arg-info-vec (vector (=-arg-info-make 5 "number" "99")))
					  (=-call-info-make
					   :start-pos 9
					   :fun-symbol '1-
					   :arg-info-vec (vector (=-arg-info-make 13 "number" "100"))))))))

  (ert-with-test-buffer (:name "filter remove all")
	(insert "(1+ 99) (1- 100)")
	(let ((lst))
	  (iter-do (x (=--call-infos-iter (current-buffer) (point-min) (point-max) (lambda (&rest _args) nil)))
		(push x lst))
	  (setq lst (nreverse lst))
	  (should (eq lst nil)))))

(ert-deftest default-filter-function-test ()
  (ert-with-test-buffer (:name "skip min-num-args 2")
	(insert "(+ 1)")
	(let ((elhints-display-min-num-args 2)
		  (it (=--call-infos-iter)))
	  (should (not (=-default-filter-function (iter-next it))))))
  (ert-with-test-buffer (:name "keep min-num-args 2")
	(insert "(+ 1 2 3 4)")
	(let* ((elhints-display-min-num-args 2)
		   (it (=--call-infos-iter))
		   (call-info (iter-next it)))
	  (should call-info)
	  (should (=-default-filter-function call-info))))
  (ert-with-test-buffer (:name "keep below min-num-args contains literal")
	(insert "(list nil)")
	(let* ((elhints-display-min-num-args 5)
		   (it (=--call-infos-iter))
		   (call-info (iter-next it)))
	  (should call-info)
	  (should (=-default-filter-function call-info)))))

;; Local Variables:
;; read-symbol-shorthands: (("=-" . "elhints-"))
;; End:

(provide 'elhints-test)

;;; elhints-test.el ends here
