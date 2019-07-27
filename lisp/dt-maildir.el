;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019

(require 'cl-lib)

(defgroup dt-maildir nil
  "Functions to check for maildir messages."
  :group 'mail
  :prefix "dt-maildir-")

(defcustom dt-maildir-maildir-list
  `(,(expand-file-name (or (getenv "MAILDIR")
						   (getenv "MAIL")
						   "~/Mail")))
  "An alist of maildir directories to search. The first element
  is the name (personal, job, ...), the second the relative
  path."
  :type 'list
  :group 'dt-maildir)

(defcustom dt-maildir-maildir-base-dir nil
  "Default directory to search for maildir directories in. Set to
nil if you use absolute paths in `dt-maildir-maildir-list'"
  :type '(file :must-match t)
  :group 'dt-maildir)

(defun dt-maildir--check-mail (dir)
  (cddr (directory-files (expand-file-name "new" dir))))

;;;###autoload
(defun dt-maildir-mail-function ()
  "Function to assign to `display-time-mail-function'. Will use
`dt-maildir-maildir-base-dir' and `dt-maildir-maildir-list' to check for new
messages."
  (cl-some #'dt-maildir--check-mail
		   (mapcar (lambda (dir)
					 (expand-file-name dir dt-maildir-maildir-base-dir))
				   dt-maildir-maildir-list)))

(provide 'dt-maildir)
