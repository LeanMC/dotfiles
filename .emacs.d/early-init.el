(require 'package)
(require 'use-package-ensure)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(setopt use-package-always-ensure t)
(unless package-archive-contents
  (package-refresh-contents))
