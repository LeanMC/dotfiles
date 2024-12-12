(dolist (var '((left . 0)
	       (top . 0)
	       (fullscreen . fullheight)
	       (width . 0.5)
	       (font . "Iosevka-16")
	       (undecorated . t)))
  (add-to-list 'default-frame-alist var)
  (add-to-list 'initial-frame-alist var))

(when (eq system-type 'darwin)
  (setq
   ns-command-modifier 'control
   ns-option-modifier 'meta
   ns-control-modifier 'super))

(defun flash-mode-line()
  (invert-face 'mode-line)
  (run-with-timer 0.1 nil #'invert-face 'mode-line))

(setq visible-bell nil
      ring-bell-function 'flash-mode-line)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(setq line-move-visual nil
      track-eol t)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(setq-default cursor-type 'bar)
(setq-default blink-cursor-blinks 0)
(global-hl-line-mode 1)

(fido-vertical-mode 1)

(use-package company
  :config
  (global-company-mode))

(use-package recentf
    :bind ("C-x C-r" . recentf-open)
    :ensure nil)
(save-place-mode 1)

(electric-pair-mode 1)

(set-face-font 'variable-pitch "Georgia-16")

(use-package ligature
:load-path "path-to-ligature-repo"
:config
;; Enable all Iosevka ligatures in programming modes
(ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->" "<->" "<-->" "<--->" "<---->" "<!--"
                                     "<==" "<===" "<=" "=>" "=>>" "==>" "===>" ">=" "<=>" "<==>" "<===>" "<====>" "<!---"
                                     "<~~" "<~" "~>" "~~>" "::" ":::" "==" "!=" "===" "!=="
                                     ":=" ":-" ":+" "<*" "<*>" "*>" "<|" "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
;; Enables ligature checks globally in all buffers. You can also do it
;; per mode with `ligature-mode'.
(global-ligature-mode t))

(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

(use-package dracula-theme
  :config
  (load-theme 'dracula t nil))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(global-set-key (kbd "C-x C-b") 'ibuffer)
(use-package all-the-icons-ibuffer
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode))

(use-package all-the-icons-completion
  :init (all-the-icons-completion-mode))

;; taken from https://emacs.stackexchange.com/questions/5529/how-to-right-align-some-items-in-the-modeline
;; this allows for some items to be right-justified. will be obsolete in emacs30 with mode-line-format-right-align
 (defun leanmc-left-right-modeline (left right)
 "Return a string of `window-width' length containing LEFT, and RIGHT
aligned respectively."
 (let* ((available-width (- (window-width) (length left) 2)))
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

(setq org-hide-emphasis-markers t)

(setq auth-sources '("~/.authinfo"))
(use-package forge
  :after magit)

(use-package ledger-mode
    :mode ("\\.dat\\'"
           "\\.ledger\\'"))
;;    :custom (ledger-clear-whole-transactions t))
       
  (use-package flycheck-ledger :after ledger-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(mixed-pitch zenburn-theme solarized-theme ligature ledger-mode gruvbox-theme forge flycheck-ledger emmet-mode dracula-theme company all-the-icons-ibuffer all-the-icons-dired all-the-icons-completion)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
