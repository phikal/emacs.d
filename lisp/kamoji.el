;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019

(require 'ring)

(defvar kamoji--last-used (make-ring 32)
  "Ring of last kamojis inserted")
(defvar kamoji--last-category nil
  "Symbol of last category used to insert a kamoji")



(defun kamoji--parse-buffer (buf)
  "Parse a buffer creating a alist.

Categories are delimited by an group separator (ASCII 35), which
are in turn split into tags and kamojis. These two are kept apart
by a record separator (ASCII 36). Both tags and kamojis split
their unit components by unit separators (ASCII 37)."
  (with-current-buffer buf
	(let (records end)
	  (goto-char (point-min))
	  (while (save-excursion (setq end (search-forward "" nil t)))
		(save-restriction
		  (narrow-to-region (point) end)
		  (let* ((names (split-string (buffer-substring
									   (point-min)
									   (1- (search-forward "")))
									  ""))
				 (kamojis (split-string (buffer-substring
										 (point) (point-max)) "")))
			(dolist (name names)
			  (push (cons name (nbutlast kamojis 1))
					records))))
		(goto-char end))
	  records)))

(defun kamoji-parse-file (filename)
  "Parse FILENAME for a list of Kamoji categories."
  (with-temp-buffer
	(insert-file-contents filename)
	(kamoji--parse-buffer (current-buffer))))

(defconst kamoji-alist
  (let* ((dir (if load-file-name
				  (file-name-directory load-file-name)
				default-directory))
		 (file (expand-file-name "KAMOJIS" dir)))
	  (kamoji-parse-file file))
  "Alist of various kamojis.")



(defun kamoji--select-kamoji (category)
  "General function to interactively select a kamoji from
`kamoji-alist'. Will first query a category, then a specific
kamoji."
  (let* ((moods (mapcar #'car kamoji-alist))
		 (category (or category (completing-read "Category: " moods nil t)))
		 (list (cdr (assoc category kamoji-alist)))
		 (kamoji (completing-read (format "Kamoji (%s): " category) list)))
	(setf kamoji--last-category category)
	(ring-insert kamoji--last-used kamoji)
	kamoji))

(defun kamoji--choose-kamoji (arg)
  "Helper function to choose a kamoji

No prefix argument opens a category then kamoji menu, a single
prefix argument re-opens the last category, and a double prefix
argument lists the last used kamojis."
  (cond ((and (>= arg 16) (< 0 (ring-size kamoji--last-used)))
		 (completing-read "Last inserted Kamojis: " (ring-elements kamoji--last-used)))
		((>= arg 4) (kamoji--select-kamoji kamoji--last-category))
		(t (kamoji--select-kamoji nil))))



;;;###autoload
(defun kamoji-insert-kill-ring (arg)
  "Insert a kamoji directly into `kill-ring'.

See `kamoji--choose-kamoji' in regards to how the prefix argument
is handled."
  (interactive "p")
  (kill-new (kamoji--choose-kamoji arg)))

;;;###autoload
(defun kamoji-insert (arg)
  "Insert a kamoji directly into the current buffer.

See `kamoji--choose-kamoji' in regards to how the prefix argument
is handled."
  (interactive "p")
  (insert (kamoji--choose-kamoji arg)))

(provide 'kamoji)
