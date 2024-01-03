(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install a package via the elpaca macro
;; See the "recipes" section of the manual for more details.

;; (elpaca example-package)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; When installing a package which modifies a form used at the top-level
;; (e.g. a package which adds a use-package key word),
;; use `elpaca-wait' to block until that package has been installed/configured.
;; For example:
;; (use-package general :demand t)
;; (elpaca-wait)

;; Expands to: (elpaca evil (use-package evil :demand t))
;; (use-package evil :demand t)

;; Turns off elpaca-use-package-mode current declaration
;; Note this will cause the declaration to be interpreted immediately (not deferred).
;; Useful for configuring built-in emacs features.
;; (use-package emacs :elpaca nil :config (setq ring-bell-function #'ignore))

;; Don't install anything. Defer execution of BODY
;; (elpaca nil (message "deferred"))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-vsplit-window-right t)
  (setq evil-split-window-below t)
  (setq evil-undo-system 'undo-redo)
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (setq evil-collection-mode-list '(dashboard dired ibuffer))
  (evil-collection-init))

(use-package evil-tutor)

(use-package diminish)

(load-file (concat user-emacs-directory "vendor/buffer-move.el"))

(defun reload-init-file ()
  (interactive)
  (load-file user-init-file)
  (load-file user-init-file))

(use-package general
  :config
  (general-evil-setup)

  (general-create-definer my-leader-def
    :states '(normal insert visual emacs)
    :keymaps 'override
    :prefix "SPC"
    :global-prefix "S-SPC")

  (my-leader-def
      "f c" '((lambda ()
              (interactive)
              (find-file (expand-file-name "config.org" user-emacs-directory)))
              :wk "Edit emacs config")
      "f r" '(counsel-recentf :wk "Find recent files")
      "TAB TAB" '(comment-line :wk "Comment Lines"))

  (my-leader-def
      "b" '(:ignore t :wk "Buffer")
      "b b" '(switch-to-buffer :wk "Switch buffer")
      "b i" '(ibuffer :wk "Ibuffer")
      "b k" '(kill-this-buffer :wk "Kill this buffer")
      "b n" '(next-buffer :wk "Next buffer")
      "b p" '(previous-buffer :wk "Previous buffer")
      "b r" '(revert-buffer :wk "Reload buffer"))

  (my-leader-def
      "e" '(:ignore t :wk "Evaluate/Eshell")
      "e b" '(eval-buffer :wk "Evaluate elisp in buffer")
      "e d" '(eval-defun :wk "Evaluate defun containing or after point")
      "e e" '(eval-expression :wk "Evaluate and elisp expression")
      "e l" '(eval-last-sexp :wk "Evaluate elisp expression before point")
      "e r" '(eval-region :wk "Evaluate elisp in region")
      "e s" '(eshell :wk "Eshell")
      "e h" '(counsel-esh-history :wk "Eshell"))


  (my-leader-def
      "w" '(:ignore t :wk "Windows")
      ;; split
      "w c" '(evil-window-delete :wk "Close window")
      "w n" '(evil-window-new :wk "New window")
      "w s" '(evil-window-split :wk "Horizontal split window")
      "w v" '(evil-window-vsplit :wk "Vertical split window")
      ;; motion
      "w h" '(evil-window-left :wk "Window left")
      "w j" '(evil-window-down :wk "Window down")
      "w k" '(evil-window-up :wk "Window up")
      "w l" '(evil-window-right :wk "Window right")
      "w w" '(evil-window-next :wk "Window next")
      ;; move
      "w H" '(buf-move-left :wk "Buffer move left")
      "w J" '(buf-move-down :wk "Buffer move down")
      "w K" '(buf-move-up :wk "Buffer move up")
      "w L" '(buf-move-right :wk "Buffer move right"))

  (my-leader-def
      "h" '(:ignore t :wk "Help")
      "h f" '(describe-function :wk "Describe function")
      "h v" '(describe-variable :wk "Describe variable")
      "h r r" '(reload-init-file :wk "Reload emacs config"))

  (my-leader-def
      "t" '(:ignore t :wk "Toggle")
      "t l" '(display-line-numbers-mode :wk "Toggle line numers")
      "t t" '(visual-line-mode :wk "Toggle truncated lines")
      "t v" '(vterm-toggle :wk "Toggle vterm")))

(use-package all-the-icons
  :ensure t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda () (all-the-icons-dired-mode t))))

;; (global-unset-key (kbd "S-SPC"))

(setq gc-cons-threshold (* 1024 1024 100))
(setq read-process-output-max (* 1024 1024))

(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq auto-save-default nil)
(setq select-enable-clipboard nil)
(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-message t)
(setq visual-bell t)

(electric-indent-mode -1)
(setq org-edit-src-content-indentation 0)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(column-number-mode)
(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
          vterm-mode-hook
          shell-mode-hook
          eshell-mode-hook))
(add-hook mode (lambda () (display-line-numbers-mode -1))))

(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

(load-theme 'misterioso t)

(set-face-attribute 'default nil
  :font "Hack"
  :height 90
  :weight 'medium)
(set-face-attribute 'variable-pitch nil
  :font "Ubuntu"
  :height 100
  :weight 'medium)
(set-face-attribute 'fixed-pitch nil
  :font "Hack"
  :height 90
  :weight 'medium)
;; Makes commented text and keywords italics.
;; This is working in emacsclient but not emacs.
;; Your font must have an italic face available.
(set-face-attribute 'font-lock-comment-face nil
  :slant 'italic)
(set-face-attribute 'font-lock-keyword-face nil
  :slant 'italic)

;; This sets the default font on all graphical frames created after restarting Emacs.
;; Does the same thing as 'set-face-attribute default' above, but emacsclient fonts
;; are not right unless I also add this method of setting the default font.
(add-to-list 'default-frame-alist '(font . "Hack-9"))

(set-fontset-font "fontset-default" 'hangul '("D2Coding" . "unicode-bmp"))

(setq-default line-spacing 0.12)

(use-package toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook 'toc-org-enable))

(with-eval-after-load 'diminish
  (diminish 'org-indent-mode)
  (add-hook 'org-mode-hook 'org-indent-mode))

(use-package org-bullets
  :hook 
  (org-mode . org-bullets-mode))

;; <a <c <C <e <E <h <l <q <s <v
(require 'org-tempo)

(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

(setq eshell-rc-script (concat user-emacs-directory "eshell/profile")
      eshell-aliases-file (concat user-emacs-directory "eshell/aliases")
      eshell-history-size 5000
      eshell-buffer-maximum-lines 5000
      eshell-hist-ignoredups t
      eshell-scroll-to-bottom-on-input t
      eshell-destroy-buffer-when-process-dies t
      eshell-visual-commands '("bash" "fish" "htop" "ssh" "top" "zsh"))

(use-package vterm
  :config
  (setq shell-file-name "/bin/bash"
        vterm-max-scrollback 5000))

(use-package vterm-toggle
  :after vterm
  :config
  (setq vterm-toggle-fullscreen-p nil)
  ;(setq vterm-toggle-scope 'project)
  (add-to-list
   'display-buffer-alist
   '((lambda (buffer-or-name _)
       (let ((buffer (get-buffer buffer-or-name)))
         (with-current-buffer buffer
           (or (equal major-mode 'vterm-mode)
               (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (reusable-frames . visible)
                 (window-height . 0.3))))

(use-package which-key
  :init (which-key-mode 1)
  :diminish
  :config
  (setq wich-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 50
        which-key-allow-imprecise-window-fit nil
        which-key-separator " → " ))

(use-package sudo-edit
  :config
    (my-leader-def
    "f u" '(sudo-edit-find-file :wk "Sudo find file")
    "f U" '(sudo-edit :wk "Sudo edit file")))

(use-package rainbow-mode
  :diminish
  :hook
  ((org-mode prog-mode) . rainbow-mode))

(use-package counsel
  :after ivy
  :diminish
  :config (counsel-mode))

(use-package ivy
  :bind
  (("C-c C-r" . ivy-resume)
   ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format ("(%d/%d) "))
  (setq enable-recursive-minibuffers t)
  :diminish
  :config
  (ivy-mode))

(use-package all-the-icons-ivy-rich
  :ensure t
  :init (all-the-icons-ivy-rich-mode 1))

(use-package ivy-rich
  :after ivy
  :ensure t
  :init (ivy-rich-mode 1)
  :custom
  (ivy-virtual-abbreviate 'full)
  (ivy-rich-switch-buffer-align-virtual-buffer t)
  (ivy-rich-path-style 'abbrev))

(use-package company
  :defer 2
  :diminish
  :custom
  (company-begin-commands '(self-insert-command))
  (company-idle-delay .1)
  (company-minimum-prefix-length 2)
  (company-show-numbers t)
  (company-tooltip-align-annotations 't)
  (global-company-mode t))

(use-package company-box
  :after company
  :diminish
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :defer t
  :diminish
  :init (global-flycheck-mode))


