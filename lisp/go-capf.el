;;; published with CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019
;;;
;;; note: requires version of gocode with `sexp' output format.

(defgroup go-capf nil
  "Completion back-end for Go."
  :group 'completion)

(defcustom go-capf-gocode (executable-find "gocode")
  "Path to gocode binary."
  :type 'file
  :group 'go-capf)

(defun go-capf--completions (&rest _ignore)
  (let* ((temp (generate-new-buffer " *gocode*")))
	(prog2
		(call-process-region (point-min) (point-max)
							 go-capf-gocode
							 nil temp nil
							 "-f=sexp" "autocomplete"
							 (or (buffer-file-name) "")
							 (format "c%d" (- (point) 1)))
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
		(completion-table-with-cache #'go-capf--completions)))

(provide 'go-capf)