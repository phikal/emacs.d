;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019

(require 'rx)
(require 'cl-lib)
(require 'bbdb-com)

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

(defconst dt-maildir--from-regexp
  (rx bol "From: " (group (*? nonl)
						  (group (+ (in ?_ ?- ?% word))
								 "@"
								 (+ (in ?_ ?- ?% word)))
						  (* nonl) eol))
  "Regular expression to extract the sender of a message in group
  1, and the address in group 2.")

(defun dt-maildir--check-mail (dir)
  (cddr (directory-files (expand-file-name "new" dir))))

(defun dt-maildir--mail-from (file)
  (with-temp-buffer
	(insert-file-contents-literally file)
	(goto-char (point-min))
	(save-match-data
	  (when (search-forward-regexp dt-maildir--from-regexp nil t)
		(cons (match-string 1)
			  (match-string 2))))))

(defun dt-maildir--collect-senders (dir)
  (cl-loop for file in (dt-maildir--check-mail dir)
		   as mail = (expand-file-name file (concat dir "/new"))
		   as from = (cdr (dt-maildir--mail-from mail))
		   when (bbdb-search (bbdb-records) :mail from)
		   collect (bbdb-record-name (car it))))

(defun dt-maildir-who-from ()
  "Collect information on who the unread messages are from. If
called interactively, also print this into the minibuffer."
  (interactive)
  (cl-loop for dir in dt-maildir-maildir-list
		   as path = (expand-file-name dir dt-maildir-maildir-base-dir)
		   when (cl-loop for from in (dt-maildir--collect-senders path)
						 concat from into list
						 finally return (and list (format "%s (%d):%s" dir (length list)
														  (if (string= list "") ""
															(concat " " list)))))
		   collect it into all
		   finally return (let ((msg (mapconcat #'identity all " ")))
							(when (called-interactively-p 'interactive)
							  (message "%s" msg))
							msg)))

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
