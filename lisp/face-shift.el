;;; -*- lexical-binding: t -*-
;;; published under CC0 into the public domain
;;; author: philip k. [https://zge.us.to], 2019

(require 'color)
(eval-when-compile (require 'cl-lib))


(defvar face-shift-intensity 0.9
  "Value to replace a `int' symbol with in `face-shift-colors'.")
(defvar face-shift-minimum 0
  "Value to replace a `min' symbol with in `face-shift-colors'.")
(defvar face-shift-maximum 1
  "Value to replace a `max' symbol with in `face-shift-colors'.")

(defconst face-shift-colors
  '((blue .   ((int min min) (min max min) (min min max)))
	(pink .   ((max min min) (min int min) (min min max)))
	(yellow . ((max min min) (min max min) (min min int)))
	(peach .  ((max min min) (min int min) (min min int)))
	(green .  ((int min min) (min max min) (min min int)))
	(purple . ((int min min) (min int min) (min min max))))
  "Alist of matrices representing RGB transformations towards a
  certain hue. Symbols `int', `max' and `min' are substituted
  with `face-shift-intensity', `face-shift-maximum' and
  `face-shift-minimum' respectively.")

(defvar face-shift-faces
  (append '(default cursor region isearch)
		  (cl-remove-if-not
		   (lambda (sym)
			 (string-match-p (rx bos "font-lock-")
							 (symbol-name sym)))
		   (face-list)))
  "Faces that `face-shift' should distort.")


(defun face-shift-by (face prop mat)
  "Call `face-remap-add-relative' on FACE by distorting the
colour behind PROP by MAT in an RGB colour space."
  (let* ((mvp (lambda (vec)
			   (mapcar (lambda (row)
						 (apply #'+ (cl-mapcar #'* row vec)))
					   mat)))
		 (bg (face-attribute face prop))
		 (colors (color-name-to-rgb bg))
		 (trans (nconc (funcall mvp colors) '(2)))
		 (ncolor (apply #'color-rgb-to-hex trans)))
	(unless (eq bg 'unspecified)
	  (face-remap-add-relative face `(,prop ,ncolor)))
	ncolor))

(defun face-shift (color)
  "Produce a function that will shift all background and
foreground colours behind the faces listed in `face-shift-faces',
that can then be added to a hook."
  (let ((mat (cl-sublis
			  `((int . ,face-shift-intensity)
				(max . ,face-shift-maximum)
				(min . ,face-shift-minimum))
			  (cdr (assq color face-shift-colors)))))
	(lambda ()
	  (dolist (face face-shift-faces)
		(face-shift-by face :foreground mat)
		(face-shift-by face :background mat)))))

(provide 'face-shift)
