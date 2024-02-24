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

(recentf-mode 1)
(use-package recentf
    :bind ("C-x C-r" . recentf-open))
(save-place-mode 1)

(electric-pair-mode 1)

(set-face-font 'default "JetBrains Mono-16")
(set-face-font 'variable-pitch "IBM Plex Sans-16")

(use-package ligature
  :config
  (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
				       "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
				       "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
				       "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
				       "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
				       "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
				       ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
				       "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
				       "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
				       "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
				       "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))

  (global-ligature-mode t))

(load-theme 'dracula t nil)

(use-package all-the-icons
  :if (display-graphic-p))

(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode)

(all-the-icons-completion-mode)

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
