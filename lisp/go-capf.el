;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019
;;;
;;; based on company-go
;;;
;;; note: requires version of gocode with `sexp' output format.
;;; see https://github.com/mdempsky/gocode/pull/110

(defgroup go-capf nil
  "Completion back-end for Go."
  :group 'completion
  :prefix "go-capf-")

(with-eval-after-load 'go-mode
  (add-hook 'kill-emacs-hook
			(lambda ()
			  (when (file-exists-p (expand-file-name
									(concat "gocode-daemon." (or (getenv "USER") "all"))
									temporary-file-directory))
				(ignore-errors (call-process "gocode" nil nil nil "close"))))))

(defcustom go-capf-gocode (executable-find "gocode")
  "Path to gocode binary."
  :type 'file
  :group 'go-capf)

(defcustom go-capf-gocode-flags nil
  "Additional flags to pass to gocode"
  :type 'file
  :group 'go-capf)

(defun go-capf--completions (&rest _ignore)
  (let* ((temp (generate-new-buffer " *gocode*")))
	(prog2
		(apply #'call-process-region
			   (append (list (point-min) (point-max)
							 go-capf-gocode
							 nil temp nil)
					   go-capf-gocode-flags
					   (list "-f=sexp" "autocomplete"
							 (or (buffer-file-name) "")
							 (format "c%d" (- (point) 1)))))
		(with-current-buffer temp
		  (goto-char (point-min))
		  (mapcar #'cadr (read temp)))
	  (kill-buffer temp))))

;;;###autoload
(defun go-completion-at-point-function ()
  (unless go-capf-gocode
	(error "gocode either not installed or not in path"))
  (list (save-excursion
		  (unless (memq (char-before) '(?\. ?\t ?\n ?\ ))
			(forward-word -1))
		  (point))
		(point)
		(completion-table-with-cache #'go-capf--completions)
		:exclusive 'no))

(provide 'go-capf)
