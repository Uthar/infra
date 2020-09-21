(require 'package)
(add-to-list 'package-archives
			 '("melpa" . "https://melpa.org/packages/"))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package epl
  :ensure t)

(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :custom
  (gcmh-idle-delay 10)
  (gcmh-high-cons-threshold #x20000000)
  :config (gcmh-mode t))

(use-package diminish
  :ensure t)

(use-package restart-emacs
  :ensure t)

(use-package direnv
  :ensure t
  :config (direnv-mode))

(use-package rg
  :ensure t)

(use-package wgrep
  :ensure t)

(use-package git-timemachine
  :ensure t
  :hook
  (git-timemachine-mode
   . (lambda ()
	   (mapcar
		(lambda (key)
		  (define-key evil-normal-state-local-map (kbd key)
			(lookup-key git-timemachine-mode-map key)))
		(mapcar
		 (lambda (cell)
		   (format "%c" (car cell)))
		 (cdr git-timemachine-mode-map))))))

(use-package neotree
  :ensure t
  :custom
  (neo-smart-open t)
  (neo-theme 'ascii))

(use-package hl-todo
  :ensure t
  :custom
  (hl-todo-highlight-punctuation ":")
  :config (global-hl-todo-mode))

(use-package projectile
  :ensure t
  :custom
  (projectile-completion-system 'ivy)
  :config (projectile-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :custom
  (sp-autoinsert-pair nil)
  (sp-autoskip-closing-pair nil)
  (sp-base-key-bindings 'paredit))

(use-package smartparens-config
  :ensure smartparens
  :hook
  (prog-mode . smartparens-mode))

(use-package magit
  :ensure t
  :custom
  (magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :ensure t)

(use-package winum
  :ensure t
  :config
  (dotimes (i 9)
    (let ((n (+ i 1)))
      (global-set-key
        (kbd (format "M-%i" n))
        (intern (format "winum-select-window-%i" n)))))
  (winum-mode))

(use-package ivy
  :ensure t
  :diminish ivy-mode
  :config (ivy-mode t))

(use-package counsel
  :ensure t
  :diminish counsel-mode
  :config (counsel-mode))

(use-package zenburn-theme
  :ensure t
  :config
  (load-theme 'zenburn t))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(defmacro my/user-emacs-subdirectory (dir)
  `(expand-file-name (concat user-emacs-directory ,dir)))

(defconst my/backup-dir (my/user-emacs-subdirectory "backups/"))
(defconst my/auto-save-dir (my/user-emacs-subdirectory "auto-save/"))
(defconst my/undo-tree-history-dir (my/user-emacs-subdirectory "undo-tree-history/"))

(use-package emacs
  :custom
  (create-lockfiles nil)
  (enable-recursive-minibuffers t)
  (custom-file null-device)
  (use-dialog-box nil)
  (require-final-newline t)
  (display-line-numbers-type 'relative)
  (scroll-margin 4)
  (scroll-conservatively 1000)
  (initial-scratch-message "")
  (inhibit-startup-screen t)
  (version-control t)
  (delete-old-versions t)
  (backup-directory-alist `((".*" . ,my/backup-dir)))
  (auto-save-file-name-transforms `((".*" ,my/auto-save-dir t)))
  (auto-save-list-file-prefix my/auto-save-dir)
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,my/undo-tree-history-dir)))
  (read-process-output-max (* 1024 1024))
  (indent-tabs-mode nil)
  (c-basic-offset 4)
  (tab-width 4)
  :config
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (save-place-mode t)
  (show-paren-mode t)
  (winner-mode t)
  (savehist-mode)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defun display-startup-echo-area-message ())
  (set-language-environment "UTF-8")
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (prog-mode . display-line-numbers-mode))

(use-package evil
  :diminish undo-tree-mode
  :ensure t
  :custom
  (evil-want-C-u-scroll t)
  (evil-kill-on-visual-paste nil)
  :config (evil-mode 1))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package anzu
  :ensure t
  :diminish anzu-mode
  :config (global-anzu-mode t))

(use-package evil-anzu
  :ensure t)

(use-package company
  :ensure t
  :diminish company-mode
  :hook (after-init . global-company-mode))

(use-package which-key
  :ensure t
  :diminish which-key-mode
  :custom (which-key-dont-use-unicode t)
  :config (which-key-mode))

(use-package editorconfig
  :ensure t
  :diminish editorconfig-mode
  :config (editorconfig-mode t))

(use-package dashboard
  :ensure projectile
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-items '((recents . 10)
					 (projects . 10)
					 (bookmarks . 5)))
  (dashboard-set-footer nil)
  :hook
  (dashboard-mode
   . (lambda ()
	   (mapcar (lambda (key)
				 (define-key evil-normal-state-local-map (kbd key)
				   (lookup-key dashboard-mode-map key)))
			   '("m" "p" "r"))))
  :config
  (defun dashboard-setup-startup-hook ()
	(add-hook 'after-init-hook (lambda ()
								 (dashboard-insert-startupify-lists)))
	(add-hook 'emacs-startup-hook (lambda ()
									(when (and (not (eval (daemonp)))
											   (< (length command-line-args) 2 ))
									  `(,(switch-to-buffer "*dashboard*")
										,(goto-char (point-min))
										,(redisplay))))))
  (dashboard-setup-startup-hook))

(use-package flycheck
  :ensure t
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :diminish flycheck-mode
  :config (global-flycheck-mode))

(use-package lsp-mode
  :init
  (require 'dap-lldb)
  :ensure t
  :custom
  (lsp-print-io t)
  (lsp-trace t)
  (lsp-enable-snippet nil)
  :commands lsp
  :hook
  ((python-mode . lsp)
   (c-mode . lsp)
   (c++-mode . lsp)
   (sh-mode . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode))

(use-package dap-mode
  :ensure t
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1))

(use-package company-lsp
  :ensure t
  :custom
  (company-lsp-cache-candidates t)
  (company-lsp-async t))

(use-package lsp-python-ms
  :ensure t
  :custom
  (lsp-python-ms-executable (executable-find "python-language-server"))
  :hook
  (python-mode
   . (lambda ()
	   (require 'lsp-python-ms)
	   (require 'dap-python)
	   (lsp))))
