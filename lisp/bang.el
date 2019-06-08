;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019
;;; 
;;; based on bang from http://chneukirchen.org/dotfiles/.emacs

(require 'rx)

(defconst bang--command-regexp
  (rx bos (* space)
	  (or (seq (group "!" )
			   (or (group (+ digit))
				   (group (+ alnum))))
		  (group "<") (group ">") (group "|") "")
	  (? (* space)
		 (group (* not-newline)
				(not space)
				(*? not-newline)))
	  eos)
  "Regular expression to parse input to `bang'.")

(defvar bang--last-commands '()
  "List of non-history commands last executed by `bang'.")

(defvar bang-history-size 80
  "Number of commands to save in `bang--last-commands'.")

(defun bang--remember-command (command)
  (push command bang--last-commands)
  (let ((overflow (- (length bang--last-commands)
					 bang-history-size)))
	(when (> overflow 0)
	  (setq bang--last-commands
			(nbutlast bang--last-commands overflow)))))

(defun bang--find-last-command (prefix)
  (catch 'found
	(dolist (cmd bang--last-commands)
	  (when (string-prefix-p prefix cmd)
		(throw 'found cmd)))
	(error "no such command in history")))

(defun bang--get-command-number (arg rest)
  (let* ((num (string-to-number arg))
		 (pos (- (length bang--last-commands)
				 (1- num)))
		 (cmd (nth pos bang--last-commands)))
	(concat cmd " " rest)))

(defun bang-history ()
  "Display a buffer with overview of previous bang commands."
  (interactive)
  (let ((buf (get-buffer-create "*bang-history*"))
		(i (length bang--last-commands)))
	(with-current-buffer buf
	  (setq buffer-read-only nil)
	  (delete-region (goto-char (point-min))
					 (point-max))
	  (dolist (cmd bang--last-commands)
		(insert (format "%d\t%s\n" i cmd))
		(setq i (1- i)))
	  (special-mode))
	(pop-to-buffer buf)))

(defun bang (command beg end)
  "Intelligently execute string COMMAND in inferior shell, like
with `shell-command'.

When COMMAND starts with
  <  the output of COMMAND replaces the current selection
  >  COMMAND is run with the current selection as input
  |  the current selection is filtered through COMMAND
  !  executes the last command that started with COMMAND,
     or if a number, re-execute nth last command

Without any argument, `bang' will behave like `shell-command'.

Inside COMMAND, % is replaced with the current file name. To
insert a literal % quote it using a backslash."
  (interactive (list (read-shell-command "Bang command: ")
					 (if mark-active (region-beginning) (point-min))
					 (if mark-active (region-end) (point-max))))
  (save-match-data
	(unless (string-match bang--command-regexp command)
	  (error "Invalid command"))
	(let ((has-! (match-string-no-properties 1 command))
		  (num-! (match-string-no-properties 2 command))
		  (arg-! (match-string-no-properties 3 command))
		  (has-< (match-string-no-properties 4 command))
		  (has-> (match-string-no-properties 5 command))
		  (has-| (match-string-no-properties 6 command))
		  (rest  (match-string-no-properties 7 command)))
	  (cond (arg-! (bang (bang--find-last-command arg-!)
						 beg end))
			(num-! (bang (bang--get-command-number num-! rest)
						 beg end))
			(has-< (delete-region beg end)
				   (shell-command rest t shell-command-default-error-buffer)
				   (exchange-point-and-mark))
			(has-> (shell-command-on-region
					beg end rest nil nil
					shell-command-default-error-buffer t))
			(has-| (shell-command-on-region
					beg end rest t t
					shell-command-default-error-buffer t))
			(t (shell-command command nil shell-command-default-error-buffer)))
	  (when (or has-! has->)
		(with-current-buffer "*Shell Command Output*"
		  (kill-ring-save (point-min) (point-max))))
	  (unless (or num-! arg-!)
		(bang--remember-command command)))))

(provide 'bang)
