;;; published with CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019

(defgroup cpp-capf nil
  "Completion back-end for C using clang."
  :group 'completion)

(defcustom cpp-capf-include-paths
  '("/usr/local/include"
	"/usr/lib/llvm-7/lib/clang/7.0.1/include"
	"/usr/include/x86_64-linux-gnu"
	"/usr/include"
	"." ".." "../..")
  "Paths to directories with header files"
  :type 'list
  :group 'cpp-capf)

(defcustom cpp-capf-extra-flags nil
  "Additional flags to call clang with"
  :type 'list
  :group 'cpp-capf)

(defcustom cpp-capf-clang (executable-find "clang++")
  "Path to clang binary."
  :type 'file
  :group 'cpp-capf)

(defun cpp-capf--completions (&rest _ignore)
  (let* ((temp (generate-new-buffer " *clang*")))
	(prog2
		(apply
		 #'call-process-region
		 (append (list (point-min) (point-max)
					   cpp-capf-clang nil temp nil
					   "-cc1" "-fsyntax-only"
					   "-code-completion-macros")
				 (mapcar (apply-partially #'concat "-I")
						 cpp-capf-include-paths)
				 cpp-capf-extra-flags
				 (list (format
						"-code-completion-at=-:%d:%d"  
						(line-number-at-pos)
						(1+ (length (encode-coding-region
									 (line-beginning-position)
									 (point) 'utf-8 t))))
					   "-")))
	  (with-current-buffer temp
		(goto-char (point-min))
		(let (result)
		  (while (search-forward-regexp
				  "^COMPLETION: \\(.+\\) : "
				  nil t)
			(push (match-string 1) result))
		  result))
	  (kill-buffer temp))))

;;;###autoload
(defun cpp-completion-at-point-function ()
  (unless cpp-capf-clang
	(error "company either not installed or not in path"))
  (list (save-excursion
		  (unless (memq (char-before) '(?\. ?\t ?\n ?\ ?\; ?\)))
			(forward-word -1))
		  (point))
		(point)
		(completion-table-with-cache #'cpp-capf--completions)
		:exclusive 'no))

(provide 'cpp-capf)
