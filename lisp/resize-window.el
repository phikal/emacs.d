;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019


(defvar resize-window-mode-map
  (let ((m (make-sparse-keymap)))
	(suppress-keymap m t)
	(dolist (k '(("k" . enlarge-window)
				 ("j" . shrink-window)
				 ("h" . enlarge-window-horizontally)
				 ("l" . shrink-window-horizontally)
				 ("<up>" . windmove-up)
				 ("<down>" . windmove-down)
				 ("<left>" . windmove-left)
				 ("<right>" . windmove-right)
				 ("o" . other-window)
				 ("1" . delete-other-windows)
				 ("2" . split-window-below)
				 ("3" . split-window-right)
				 ("0" . delete-window)
				 ("w" . delete-window)
				 ("q" . resize-window-mode)
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
