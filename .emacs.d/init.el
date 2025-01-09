(defvar leanmc-default-font "Victor Mono-20")
(defvar leanmc-variable-pitch-font "CMU Serif-20")
(dolist (x
	 `((fullscreen . fullboth)
	   (font . ,leanmc-default-font)))
  (add-to-list 'default-frame-alist x)
  (add-to-list 'initial-frame-alist x))

(when (eq system-type 'darwin)
  (setopt
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super))

(defun flash-mode-line()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setopt visible-bell nil
      ring-bell-function 'flash-mode-line)

(setopt inhibit-startup-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(global-display-line-numbers-mode)
(setopt display-line-numbers-type 'relative)

(setopt blink-cursor-mode nil)
(global-hl-line-mode 1)

(fido-vertical-mode 1)
(setopt completion-auto-help 'always)
(setopt completions-max-height 20)
(setopt completions-format 'one-column)
(setopt completion-auto-select 'second-tab)
(setopt completions-detailed t)

(electric-pair-mode 1)

(set-face-font 'fixed-pitch leanmc-default-font)
(set-face-font 'variable-pitch leanmc-variable-pitch-font)

(use-package color-theme-sanityinc-tomorrow
  :config
  (load-theme 'sanityinc-tomorrow-night t nil))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode))

(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
  :config (all-the-icons-completion-mode))

;; emacs 30 will make much of this code unnecessary with the addition of mode-line-format-right-align
(defun leanmc-left-right-modeline (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
aligned respectively."
  (let* ((available-width (- (window-width) (length left) 10)))
    (format (format " %%s %%%ds " available-width) left right)))


(defun leanmc-modeline-icon-buffer-edit-status ()
  "Return a lock icon if file is read-only, link if it's unedited, or broken link if it's edited"
  (if buffer-read-only
      (propertize (format "%s" (all-the-icons-faicon "lock"))
		  'help-echo "Read-only")
    (if (buffer-modified-p)
	(propertize (format "%s" (all-the-icons-faicon "chain-broken"))
		    'help-echo "Edited")
      (propertize (format "%s" (all-the-icons-faicon "link"))
		  'help-echo "No changes"))))


(defun leanmc-modeline-icon-major-mode ()
  "Return an icon for the current major mode"
  (propertize (all-the-icons-icon-for-mode major-mode)
	      'help-echo (format "%s" major-mode)
	      'local-map mode-line-major-mode-keymap))


;;Bring it all together with setq-default and the :eval keyword, along with some more % constructs
(setq-default mode-line-format '((:eval
				  (leanmc-left-right-modeline
				   (format-mode-line
				    (concat
				     "%e "
				     (leanmc-modeline-icon-buffer-edit-status)
				     " %b"))
				   (format-mode-line
				    (concat
				     "%p%% "
				     (format " %s " (leanmc-modeline-icon-major-mode))))))))

(setopt org-hide-emphasis-markers t)
(setopt org-startup-folded t)
(defun leanmc-org-mode-hook ()
  (variable-pitch-mode 1)
  (visual-line-mode 1)
  (dolist (f (list 'org-block 'org-code 'org-table 'org-block-begin-line 'org-block-end-line 'org-meta-line 'line-number))
    (set-face-attribute f nil :inherit 'fixed-pitch)))
(add-hook 'org-mode-hook 'leanmc-org-mode-hook)

(use-package ledger-mode
  :mode ("\\.dat\\'"
	 "\\.ledger\\'")
  :config (when (eq system-type 'windows-nt) (setq ledger-binary-path "~/ledger.exe")))

(setq auth-sources '("~/.authinfo"))
(use-package magit)
