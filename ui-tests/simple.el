(add-to-list 'load-path "/tmp" t)

;; Rest args
(list 1 2 3 4 5)
(list 1 2 `(+ 1 2))


;; Backticks
(let ((expr `(directory-files "/" t "foo" t ,(1+ 2))))
  (eval expr))
