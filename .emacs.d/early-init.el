(dolist (var '((left . 0)
	       (top . 0)
	       (fullscreen . fullheight)
	       (width . 0.5)
	       (font . "Iosevka-16")))
  (add-to-list 'default-frame-alist var)
  (add-to-list 'initial-frame-alist var))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup t)

(setq use-package-always-ensure t)
