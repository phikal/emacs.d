;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019


(defvar resize-window-mode-map
  (let ((m (make-sparse-keymap)))
	(dolist (k '(("<down>" . enlarge-window)
				 ("<up>" . shrink-window)
				 ("<right>" . enlarge-window-horizontally)
				 ("<left>" . shrink-window-horizontally)
				 ("C-<up>" . delete-other-windows)
				 ("C-<left>" . split-window-below)
				 ("C-<right>" . split-window-right)
				 ("C-<down>" . delete-window)
				 ("<escape>" . resize-window-mode))
			   m)
	  (define-key m (kbd (car k)) (cdr k))))
  "resize-window-mode keymap")

;;;###autoload
(define-minor-mode resize-window-mode
  "Minor mode to make the resizing and moving of frames more easy"
  :lighter " <@>"
  :keymap resize-window-mode-map
  :global t)

(provide 'resize-window)
;;; resize-windows.el ends here
