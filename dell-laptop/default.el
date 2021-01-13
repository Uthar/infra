(eval-when-compile
  (require 'use-package))

(use-package diminish)

(use-package restart-emacs)

(use-package browse-kill-ring)

(use-package evil-matchit
  :config (global-evil-matchit-mode))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(use-package direnv
  :config (direnv-mode))

(use-package rg)

(use-package wgrep)

(use-package git-timemachine
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

(use-package hl-todo
  :custom
  (hl-todo-highlight-punctuation ":")
  :config (global-hl-todo-mode))

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-track-known-projects-automatically nil)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode))

(use-package smartparens
  :diminish
  :custom
  (sp-autoinsert-pair nil)
  (sp-autoskip-closing-pair nil)
  (sp-base-key-bindings 'paredit)
  :hook
  (prog-mode . smartparens-mode))

(use-package magit
  :custom
  (magit-completing-read-function 'ivy-completing-read)
  :hook
  (after-save . magit-after-save-refresh-status)
  :config
  (dotimes (i 4)
    (let ((n (1+ i)))
      (define-key magit-section-mode-map (kbd (format "M-%i" n)) nil)
      (define-key magit-section-mode-map (kbd (format "C-%i" n)) 
        (intern (format "magit-section-show-level-%i-all" n))))))

(use-package evil-magit)

(use-package winum
  :config
  (dotimes (i 9)
    (let ((n (+ i 1)))
      (global-set-key
        (kbd (format "M-%i" n))
        (intern (format "winum-select-window-%i" n)))))
  (winum-mode))

(use-package ivy
  :diminish
  :config (ivy-mode t))

(use-package counsel
  :diminish
  :config (counsel-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package go-mode)

(defun my/user-emacs-subdirectory (dir)
  (expand-file-name (concat user-emacs-directory dir)))

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
  (column-number-mode t)
  (line-number-mode nil)
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
  (defun my/comment-or-uncomment-region ()
    (interactive)
    (comment-or-uncomment-region (region-beginning) (region-end))
    (setq deactivate-mark nil))
  (defun x-copy ()
    (interactive)
    (when (region-active-p)
      (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
      (message "Yanked region to clipboard")
      (deactivate-mark)))
  (defun x-paste ()
    (interactive)
    (insert (shell-command-to-string "xsel -o -b")))
  (defun open-region-in-browser ()
    (interactive)
    (call-process-shell-command
     (format "$BROWSER \"%s\" &" (buffer-substring-no-properties (region-beginning) (region-end)))))
  (dotimes (i 9)
    (let ((n (+ i 1)))
      (define-key diff-mode-map (kbd (format "M-%i" n)) nil)))
  (global-set-key (kbd "C-;") 'my/comment-or-uncomment-region)
  :hook
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (prog-mode . display-line-numbers-mode))

(use-package evil
  :diminish undo-tree-mode
  :custom
  (evil-want-C-u-scroll t)
  (evil-kill-on-visual-paste nil)
  :config (evil-mode 1))

(use-package evil-surround
  :config (global-evil-surround-mode 1))

(use-package anzu
  :diminish
  :config (global-anzu-mode t))

(use-package evil-anzu)

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-minimum-prefix-length 3)
  :diminish
  :hook (after-init . global-company-mode))

(use-package which-key
  :diminish
  :custom (which-key-dont-use-unicode t)
  :config (which-key-mode))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode t))

(use-package dashboard
  :custom
  (dashboard-startup-banner 'logo)
  (dashboard-center-content t)
  (dashboard-items '((recents . 10)))
  (dashboard-set-footer nil)
  :hook
  (dashboard-mode
   . (lambda ()
	   (mapcar (lambda (key)
				 (define-key evil-normal-state-local-map (kbd key)
				   (lookup-key dashboard-mode-map key)))
			   '("m" "p" "r"))))
  (after-init
   . (lambda ()
       (dashboard-insert-startupify-lists)))
  (emacs-startup
   . (lambda ()
       (unless (or (symbol-value (daemonp))
                   (>= (length command-line-args) 2))
         (switch-to-buffer "*dashboard*")
         (goto-char (point-min))
         (redisplay)))))

(use-package flycheck
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :diminish
  :config (global-flycheck-mode))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  :hook
  (((c-mode c++-mode python-mode sh-mode go-mode) . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(use-package lsp-ui
  :custom (lsp-ui-doc-position 'top))

(use-package doom-themes
  :hook
  (after-init
   . (lambda ()
       (load-theme 'doom-gruvbox t))))

