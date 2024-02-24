(dolist (var '((left . 0.1)
	       (top . 0.1)
	       (width . 0.6)
	       (height . 0.6)))
  (add-to-list 'default-frame-alist var))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup t)
