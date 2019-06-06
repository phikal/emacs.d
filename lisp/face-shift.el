;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019

(require 'color)
(require 'cl-lib)

(defconst face-shift-blueish   '((0.9 0 0) (0 1 0) (0 0 1)))
(defconst face-shift-pinkish   '((1 0 0)   (0 0.9 0) (0 0 1)))
(defconst face-shift-yellowish  '((1 0 0)   (0 1 0)   (0 0 0.9)))
(defconst face-shift-peachish  '((1 0 0)   (0 0.9 0) (0 0 0.9)))
(defconst face-shift-greenish  '((0.9 0 0) (0 1 0)   (0 0 0.9)))
(defconst face-shift-purpleish '((0.9 0 0) (0 0.9 0) (0 0 1)))

(defvar face-shift-faces
  (append '(default fringe)
		  (seq-filter
		   (lambda (sym)
			 (string-match-p (rx bos (or "font-lock-"
										 "mode-line-"))
							 (symbol-name sym)))
		   (face-list)))
  "Faces that `face-shift' should distort.")

(defun face-shift-by (face prop mat)
  (let* ((mt (lambda (vec)
			   (mapcar (lambda (row)
						 (apply #'+ (cl-mapcar #'* row vec)))
					   mat)))
		 (bg (face-attribute face prop))
		 (colors (color-name-to-rgb bg))
		 (trans (nconc (funcall mt colors) '(2)))
		 (ncolor (apply #'color-rgb-to-hex trans)))
	(unless (eq bg 'unspecified)
	  (face-remap-add-relative face `(,prop ,ncolor)))
	ncolor))

(defun face-shift (trans)
  (lambda ()
	(dolist (face face-shift-faces)
	  (face-shift-by face :foreground trans)
	  (face-shift-by face :background trans))
	(redraw-frame)))
