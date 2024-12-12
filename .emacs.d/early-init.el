(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setq package-enable-at-startup t)

(setq use-package-always-ensure t)
