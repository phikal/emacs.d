;; (package-initialize) - prevent package.el from adding stuff
(require 'cl-lib)
(require 'subr-x)
(let ((file-name-handler-alist nil)
      (gc-cons-threshold (lsh 1 30)))
  (org-babel-load-file (expand-file-name "conf.org" user-emacs-directory)))
