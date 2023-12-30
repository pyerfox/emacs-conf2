
(setq gc-cons-threshold (* 1024 1024 100))
(setq read-process-output-max (* 1024 1024))

(global-unset-key (kbd "S-SPC"))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq select-enable-clipboard nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)
(setq visual-bell t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)
  (dolist (mode '(org-mode-hook
                  term-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil
		    :font "Hack"
		    :weight 'normal
		    :height 100)
(set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))

(load-theme 'tsdh-dark t)


(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
