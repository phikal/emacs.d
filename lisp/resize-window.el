;;; resize-windows.el --- minor mode to resize and reorder windows -*- lexical-binding: t -*-

;; Author: Philip K.
;; Maintainer: Philip K.
;; Version: 1.1
;; Package-Requires: windmove

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Code:

(defvar resize-window-mode-map
  (let ((m (make-sparse-keymap)))
	(dolist (k '(("<down>" . enlare-window)
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
  :require 'windmove
  :lighter " <@>"
  :keymap resize-window-mode-map
  :global t)

(provide 'resize-windows)
;;; resize-windows.el ends here
