;;; elhints.el --- Provide hints in Emacs Lisp files  -*- lexical-binding: t; -*-

;;; Commentary:
;; 
;;
;; Copyright 2026 Sherman Pay
;;
;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;

;;; Code:

(require 'cl-lib)
(require 'generator)
(require 'jit-lock)
(require 'seq)
(require 'treesit)

(defface elhints-hint-face '((t (:height 0.8 :inherit shadow)))
  "Face used for hint overlays."
  :group 'elhints)

(defconst elhints--overlay-kind 'elhints--overlay
  "The overlay property to identify elhints overlays.")

(defvar-local elhints--parser nil "The buffer local Tree-sitter parser.")
(defconst elhints--treesit-thing-settings
  '((elisp
	 (list "list")
	 (symbol "symbol")
	 (call elhints--call-node-p))))

(cl-defstruct (elhints-arg-info (:constructor elhints-arg-info-make (start-pos name-str value-str &key (kind 'positional))))
  "Argument info"
  start-pos name-str value-str kind)

(cl-defstruct (elhints-call-info (:constructor elhints-call-info-make))
  "Contains function call information from a parser."
  start-pos fun-symbol arg-info-vec)

(defun elhints--parse-arglist (arglist call-node)
  "Parse ARGLIST and CALL-NODE to a vector of ELHINTS-ARG-INFO."
  ;; (message "arglist=%s; call-node=%s, len=%d" arglist call-node (treesit-node-child-count call-node 'named))
  (cl-flet ((next-arg-name (arg-list) (and-let* ((arg (car arg-list))) (symbol-name arg))))
	(cl-loop
	 ;; subtract function node
	 with num-args = (1- (treesit-node-child-count call-node 'named))
	 with arg-info-vec = (make-vector num-args nil)
	 with kind = 'positional
	 for i from 0 below num-args
	 ;; args don't include function node
	 as arg-node = (treesit-node-child call-node (1+ i) 'named)
	 as arg-list = arglist then (or (cdr arg-list) arg-list)
	 as arg-str = (next-arg-name arg-list)
	 when (= (aref arg-str 0) ?&)
	 do
	 (setq kind (intern (substring-no-properties arg-str 1)))
	 (setf arg-list (cdr arg-list))
	 (setq arg-str (next-arg-name arg-list))
	 end
	 do
	 (let ((pos (treesit-node-start arg-node))
		   (value-str (treesit-node-text arg-node)))
	   (aset arg-info-vec
			 i
			 (=-arg-info-make pos arg-str value-str :kind kind)))
	 finally return arg-info-vec)))


(defun elhints--call-node->info (call-node)
  "Create ELHINTS-CALL-INFO from CALL-NODE."
  (let* ((fun-node (treesit-node-child call-node 0 'named))
		 (fun-symbol (intern (treesit-node-text fun-node)))
		 (arg-info-vec (elhints--parse-arglist (help-function-arglist fun-symbol t) call-node)))
	(cl-assert (string-equal (treesit-node-type fun-node) "symbol"))
	(cl-assert (arrayp arg-info-vec))
	(=-call-info-make :start-pos (treesit-node-start call-node)
					   :fun-symbol fun-symbol
					   :arg-info-vec arg-info-vec)))

(defun elhints--call-node-p (node)
  "Return t if NODE is a call node."
  (when-let* ((head (and (string-equal (treesit-node-type node) "list")
						 (treesit-node-child node 0 'named))))
	;; TODO: change this definition
	(and
	 (string-equal (treesit-node-type head) "symbol")
	 (functionp (intern (treesit-node-text head))))))

(iter-defun elhints--call-infos-iter (&optional buffer start end filter-fun)
  "Returns a generator that yields ELHINTS-CALL-INFO records."
  (let* ((treesit-thing-settings elhints--treesit-thing-settings)
		 (parser (or elhints--parser (treesit-parser-create 'elisp buffer)))
		 (pos (or start (point-min)))
		 (end (or end (point-max)))
		 (next-node (treesit-node-at pos parser)))
	(progn
	  (while-let ((call-node (treesit-search-forward next-node 'call))
				  (within-region (<= (treesit-node-start call-node) end))
				  (call-info (elhints--call-node->info call-node)))
		(when (if filter-fun
				  (funcall filter-fun call-info)
				t)
		  (iter-yield call-info))
		(setq next-node call-node)
		))))

(defun elhints-call-info-add-overlays (buffer start end call-info)
  "Add overlays to BUFFER between START and END based on CALL-INFO."
  (seq-doseq (arg-info (=-call-info-arg-info-vec call-info))
	(let* ((arg-pos (=-arg-info-start-pos arg-info))
		   (arg-name (=-arg-info-name-str arg-info))
		   (ov (make-overlay arg-pos (+ arg-pos (length arg-name)) buffer)))
	  ;; (message "ov: %s :: %s @ %s" arg-name (type-of arg-name) arg-pos)
	  (when (and arg-name (<= start arg-pos end))
		(overlay-put ov 'before-string (propertize (concat arg-name ":") 'face 'elhints-hint-face))
		(overlay-put ov =--overlay-kind t))
	  )))

;;; Treesitter Grammar installation

(defcustom elhints-elisp-grammar-source-dir
  nil
  "The source directory of the tree-sitter elisp grammar."
  :type 'directory
  :group 'elhints)

(defun elhints-ensure-grammar ()
  "Ensure the Elisp tree-sitter grammar is available; prompt to compile if missing."
  (interactive)
  (unless (assoc 'elisp treesit-language-source-alist)
	(unless elhints-elisp-grammar-source-dir
	  (user-error "elhints: Elisp grammar is not installed and custom variable elhints-elisp-grammar-source-dir is set to %s;  Set it to a valid directory containing tree-sitter elisp grammar"
				  elhints-elisp-grammar-source-dir))
    (add-to-list 'treesit-language-source-alist (cons 'elisp (list elhints-elisp-grammar-source-dir))))
  (unless (treesit-language-available-p 'elisp)
    (if (y-or-n-p "elhints: Package requires the Elisp tree-sitter grammar.  Install it now? ")
        (treesit-install-language-grammar 'elisp)
      (user-error "elhints: Elisp tree-sitter grammar is required"))))

;;; Core
(defgroup elhints nil
  "Customization group for elhints."
  :group 'lisp
  :version 31.0)

(defcustom elhints-display-min-num-args 3
  "The minimum number of arguments for function calls displaying hints."
  :type 'natnum
  :group 'elhints)

(defun elhints-default-filter-function (call-info)
  "Default function to use for option `elhints-filter-function'.

CALL-INFO is a `elhints-call-info` struct.

Returns t if the CALL-INFO node should render hints."
  (let ((arg-info-vec (=-call-info-arg-info-vec call-info)))
	(or (>= (length arg-info-vec)
			elhints-display-min-num-args)
		(cl-loop for arg-info across arg-info-vec
				 when (member (=-arg-info-value-str arg-info)
							  '("nil" "t"))
				 return t))))

(defcustom elhints-filter-function 'elhints-default-filter-function
  "Function that is called by elhints to filter out call nodes from hints.

It is called with a single argument: the `elhints-call-info'`"
  :type 'function
  :group 'elhints)

(defun elhints--add-hints (buffer start end)
  "Add hints for BUFFER between START and END."
  (iter-do (info (=--call-infos-iter buffer start end
									elhints-filter-function))
	(=-call-info-add-overlays buffer start end info)))

(defun elhints--remove-hints (buffer start end)
  "Remove hints for BUFFER between START END."
  (with-current-buffer buffer
	(remove-overlays start end =--overlay-kind t)))

(defun elhints--update-hints (start end)
  "Update hints for current buffer between START and END."
  (let ((buf (current-buffer)))
	(elhints--remove-hints buf start end)
	(elhints--add-hints buf start end)))

(define-minor-mode elhints-mode
  "Provide hints within Emacs Lisp code."
  :lighter nil
  (if elhints-mode
	  (progn
		(elhints-ensure-grammar)
		(setq-local elhints--parser (treesit-parser-create 'elisp (current-buffer)))
		(jit-lock-register #'elhints--update-hints 'contextual))
	(jit-lock-unregister #'elhints--update-hints)
	(=--remove-hints (current-buffer) (point-min) (point-max))
	(treesit-parser-delete elhints--parser)
	(setq-local elhints--parser nil)))

(provide 'elhints)

;;; elhints.el ends here

;; Local Variables:
;; read-symbol-shorthands: (("=-" . "elhints-"))
;; End:
