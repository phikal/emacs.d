;;; c-insert.el --- Quickly insert global C features -*- lexical-binding: t -*-

;; Author: Philip K. <philip@warpmail.net>
;; Version: 0.1.0
;; Keywords: faces
;; Package-Requires: ((emacs "24.4"))
;; URL: https://git.sr.ht/~zge/c-insert

;; This file is NOT part of Emacs.
;;
;; This file is in the public domain, to the extent possible under law,
;; published under the CC0 1.0 Universal license.
;;
;; For a full copy of the CC0 license see
;; https://creativecommons.org/publicdomain/zero/1.0/legalcode

;;; Commentary:
;;
;; TODO



(require 'rx)
(eval-when-compile (require 'subr-x))
(eval-when-compile (require 'cl-lib))

 ;; CUSTOMISABLE VARIABLES

(defgroup c-insert nil
  "Easily insert global C features"
  :group 'convenience
  :prefix "ch-")

(defcustom c-insert-header-additional-paths nil
  "Non-default paths to use when looking for headers."
  :safe t
  :type '(list string))

 ;; NON-CUSTOMISEABLE VARIABLES 

(defconst c-insert--header-regexp
  (rx bol (* space) "#" (* space) "include" (* space) "<" (* space)
	  (? (group (+ (not (any ">")))) "/")
	  (+ (group (+ (not (any ">")))))
	  (* space) ">" (* space) eol)
  "Pattern that describes a c header")

(defvar c-insert--headers nil
  "List of paths of all c-headers.")

 ;; INTERNAL FUNCTIONS

(defun c-insert--get-gcc-dirs ()
  "Run a GCC compatible compiler to find header paths."
  (with-temp-buffer
	(call-process (or (executable-find "gcc")
					  (executable-find "clang")
					  (error "no suitable compiler found"))
				  nil t nil
				  "-xc" "-E" "-v" "-")
	(goto-char (point-min))
	(search-forward "#include <...> search starts here:")
	(forward-line 1)
	(let ((start (point)))
	  (search-forward "End of search list.")
	  (beginning-of-line)
	  (narrow-to-region start (point)))
	(cl-loop initially (goto-char (point-min))
			 until (eobp) 
			 collect (buffer-substring (1+ (line-beginning-position))
									   (line-end-position))
			 do (forward-line 1))))

(defun c-insert--load-headers (&optional force)
  "Collect/prepare all c headers into a list and then assign the
result to `c-insert--headers'.

When FORCE is set, don't use cache and rebuild the list."
  (when (or (not c-insert--headers) force)
	(cl-loop with gcc-dirs = (c-insert--get-gcc-dirs)
			 for dir in (cl-union c-insert-header-additional-paths gcc-dirs)
			 append (cl-loop for header in (directory-files-recursively dir "\\.h$")
							 collect (string-remove-prefix (concat dir "/") header))
			 into headers
			 finally (setq c-insert--headers (sort (delete-dups headers) #'string-lessp))))
  c-insert--headers)

(defun c-insert--order-headers-in-region (beg end)
  (save-mark-and-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (goto-char (point-min))
	  (let (headers)
		(while (search-forward-regexp c-insert--header-regexp nil t)
		  (cl-pushnew (match-string-no-properties 2)
					  (alist-get (or (match-string-no-properties 1) "")
								 headers nil nil #'equal)
					  :test #'string=))
		(delete-region beg end)
		(dolist (group (cl-sort headers #'string-lessp :key #'car))
		  (dolist (header (sort (cdr group) #'string-lessp))
			(insert "#include <"
					(if (string-empty-p (car group))
						"" (concat (car group) "/"))
					header ">\n"))
		  (newline)))
	  (setq end (point))))
  (goto-char end))

 ;; INTERACTIVE FUNCTIONS

(defun c-insert-order-headers ()
  "Rearrange c headers."
  (interactive)
  (save-mark-and-excursion
	(goto-char (point-min))
	(while (search-forward-regexp c-insert--header-regexp nil t)
	  (beginning-of-line)
	  (let ((begin (point)))
		(while (or (looking-at-p c-insert--header-regexp)
				   (looking-at-p (rx bol (* space) eol)))
		  (forward-line))
		(c-insert--order-headers-in-region begin (point))))))

(defun c-insert-local-header (file &optional arg)
  "Add `file' to the top of the current file.

To prevent the automatic sorting of headers, press \\[universal-argument]"
  (interactive (list (file-relative-name (read-file-name "Local header: "))
					 current-prefix-arg))
  (unless (derived-mode-p 'c-mode)
	(message "Warning: `c-insert-local-header' is not meant to be used in non-C modes"))
  (save-mark-and-excursion
	(goto-char (point-min))
	(when file (insert (format "#include \"%s\"\n" file)))
	(unless arg (c-insert-order-headers))))

(defun c-insert-header-at-point (header)
  (interactive (list (completing-read "Header: " (c-insert--load-headers))))
  (beginning-of-line)
  (insert (format "#include <%s>\n" header)))

(defun c-insert-header (header &optional arg)
  "Add `header' to the top of the current file.

To prevent the automatic sorting of headers, press \\[universal-argument]"
  (interactive (list (completing-read "Header: " (c-insert--load-headers))
					 current-prefix-arg))
  (unless (derived-mode-p 'c-mode)
	(message "Warning: `c-insert-header' is not meant to be used in non-C modes"))
  (save-mark-and-excursion
	(goto-char (point-min))
	(forward-comment (buffer-size))
	(c-insert-header-at-point header)
	(unless arg (c-insert-order-headers))))



(provide 'c-insert)

;;; c-insert.el ends here
