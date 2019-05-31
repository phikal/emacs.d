;; (package-initialize) - prevent package.el from adding stuff
(require 'cl-lib)
(require 'subr-x)
(let ((file-name-handler-alist nil)
	  (package-enable-at-startup nil)
	  (message-log-max 16384)
      (gc-cons-threshold (lsh 1 30))
	  (gc-cons-percentage 0.6)
	  (auto-window-vscroll nil))
  (org-babel-load-file (expand-file-name "conf.org" user-emacs-directory))
  (garbage-collect))
