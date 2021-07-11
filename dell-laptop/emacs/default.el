(eval-when-compile
  (require 'use-package)
  (setf use-package-expand-minimally t
        use-package-use-theme nil))


(put 'if 'lisp-indent-function 4)

(use-package diminish)

(use-package browse-kill-ring)

(use-package evil-matchit
  :after evil
  :config (global-evil-matchit-mode))

(use-package page-break-lines
  :diminish page-break-lines-mode
  :config (global-page-break-lines-mode))

(use-package direnv
  :custom (direnv-always-show-summary nil)
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

(use-package winum
  :config
  (require 'term)
  (dotimes (i 9)
    (let* ((n (1+ i))
           (command (intern (format "winum-select-window-%i" n))))
      (define-key diff-mode-map (kbd (format "M-%i" n)) nil)
      (define-key term-raw-map (kbd (format "M-%i" n)) command)
      (global-set-key (kbd (format "M-%i" n)) command)))
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

(defun state-dir (dir)
  (expand-file-name (concat user-emacs-directory dir "/")))

(defmacro with-inhibit-message (&rest body)
  `(let ((inhibit-message t))
     ,@body))

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
  (initial-buffer-choice t)
  (version-control t)
  (delete-old-versions t)
  (backup-directory-alist `((".*" . ,(state-dir "backups"))))
  (auto-save-file-name-transforms `((".*" ,(state-dir "auto-save") t)))
  (auto-save-list-file-prefix (state-dir "auto-save"))
  (indent-tabs-mode nil)
  (c-basic-offset 4)
  (tab-width 4)
  (column-number-mode t)
  (line-number-mode nil)
  :config
  (minibuffer-depth-indicate-mode t)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1)
  (tooltip-mode -1)
  (save-place-mode t)
  (show-paren-mode t)
  (winner-mode t)
  (recentf-mode)
  (savehist-mode)
  (put 'dired-find-alternate-file 'disabled nil)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (defun display-startup-echo-area-message ())
  (set-language-environment "UTF-8")
  (defun my/comment-or-uncomment ()
    (interactive)
    (if (region-active-p)
        (comment-or-uncomment-region (region-beginning) (region-end))
        (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
    (setq deactivate-mark nil))
  (global-set-key (kbd "C-;") 'my/comment-or-uncomment)
  (defun x-copy ()
    (interactive)
    (when (region-active-p)
      (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
      (message "Yanked region to clipboard")
      (deactivate-mark)))
  (defun x-paste ()
    (interactive)
    (insert (shell-command-to-string "xsel -o -b")))
  (define-key
    dired-mode-map
    (kbd "M-h")
    (lambda ()
      (interactive)
      (if (string-match-p "a" dired-actual-switches)
          (dired "." (remove ?a dired-listing-switches))
          (dired "." (concat dired-listing-switches "a")))
      (setq dired-listing-switches dired-actual-switches)))
  (defadvice switch-to-buffer (after save-recentf activate)
    (let ((file-name (buffer-file-name (current-buffer))))
      (when file-name
        (recentf-add-file file-name)
        (with-inhibit-message (recentf-save-list)))))
  :hook
  (before-save . delete-trailing-whitespace)
  (after-save . executable-make-buffer-file-executable-if-script-p)
  (find-file . (lambda () (with-inhibit-message (recentf-save-list))))
  (prog-mode . display-line-numbers-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode
   . (lambda ()
       (define-key dired-mode-map "N" nil)
       (define-key dired-mode-map "n" nil)
       (define-key evil-normal-state-local-map "l"
         (lambda ()
           (interactive)
           (if (file-directory-p (dired-get-filename))
               (dired-find-alternate-file)
               (dired-find-file))))
       (define-key evil-normal-state-local-map "h"
         (lambda ()
           (interactive)
           (find-alternate-file "..")))))
  (after-init . (lambda () (set-cursor-color "#999"))))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-kill-on-visual-paste nil)
  (evil-undo-system 'undo-tree)
  (evil-want-keybinding nil)
  :config (evil-mode 1))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-company-use-tng nil)
  :config
  (evil-collection-magit-setup)
  (evil-collection-git-timemachine-setup)
  (evil-collection-dired-setup)
  (evil-collection-help-setup)
  (evil-collection-company-setup)
  (evil-collection-term-setup))


(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

(use-package undo-tree
  :diminish
  :custom
  (undo-tree-auto-save-history t)
  (undo-tree-history-directory-alist `(("." . ,(state-dir "undo-tree-history"))))
  :config (global-undo-tree-mode))

(use-package anzu
  :diminish
  :config (global-anzu-mode t))

(use-package evil-anzu
  :after evil)

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-minimum-prefix-length 1)
  (company-show-numbers 'left)
  :diminish
  :hook (after-init . global-company-mode))

(use-package which-key
  :diminish
  :custom (which-key-dont-use-unicode t)
  :config (which-key-mode))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode t))

;; remove ?
(use-package flycheck
  :custom (flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :diminish
  :config (global-flycheck-mode))

(use-package lsp-mode
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-restart 'ignore)
  :hook
  (((c-mode c++-mode python-mode go-mode) . lsp)
   (lsp-mode . lsp-enable-which-key-integration)))

(defconst my-theme 'doom-gruvbox)

(use-package doom-themes
  :hook
  (after-init . (lambda () (load-theme my-theme t))))

(defvar my-theme-active? t)

(defun toggle-my-theme ()
  (interactive)
  (if my-theme-active?
      (disable-theme my-theme)
      (enable-theme my-theme))
  (setf my-theme-active? (not my-theme-active?)))

(bind-key "C-c t" 'toggle-my-theme)

(use-package slime
  :custom
  (slime-truncate-lines nil)
  (slime-net-coding-system 'utf-8-unix)
  (inferior-lisp-program "sbcl")
  (slime-contribs '(slime-asdf slime-company slime-quicklisp slime-fancy))
  (slime-company-completion 'fuzzy)
  (slime-repl-auto-right-margin t)
  (slime-repl-history-size 10000)
  (common-lisp-hyperspec-root "@clhs@/")
  (common-lisp-hyperspec-symbol-table "@clhs@/Data/Map_Sym.txt")
  :bind ("C-c s" . 'slime-selector)
  :hook
  (slime-mode . (lambda () (bind-key "C-]" 'slime-edit-definition 'evil-motion-state-local-map))))


;; repl window

(defvar repl-window nil)
(defvar repl-window-height 15)
(defvar last-repl-buffer nil)

(defun repl-window-selected? ()
  (and repl-window (eq repl-window (selected-window))))

(defun save-last-repl-buffer (frame)
  (when (repl-window-selected?)
    (setf last-repl-buffer (window-buffer repl-window))))

(add-to-list 'window-buffer-change-functions 'save-last-repl-buffer)

(defun save-repl-window-height (frame)
  (when repl-window
    (setf repl-window-height (window-height repl-window))))

(add-to-list 'window-size-change-functions 'save-repl-window-height)

(defun default-repl-buffer ()
  (ansi-term "bash"))

(defun open-repl-window ()
  (setf repl-window (split-window (frame-root-window) (- repl-window-height)))
  (select-window repl-window)
  (switch-to-buffer (or last-repl-buffer (default-repl-buffer))))

(defun close-repl-window ()
  (ignore-errors (delete-window repl-window))
  (setf repl-window nil))

(defun toggle-repl-window ()
  (interactive)
  (if repl-window
      (close-repl-window)
      (open-repl-window)))


;; search

(setenv "FZF_DEFAULT_COMMAND" "fd -H")

(defun universal-argument-provided? ()
  (>= (prefix-numeric-value current-prefix-arg) 4))

(defun guess-directory (cmd-name)
  (if (universal-argument-provided?)
      (counsel-read-directory-name (concat cmd-name " in directory: "))
      (or (if (featurep 'projectile) (projectile-project-root))
          default-directory)))

(defun counsel-fzf-in-project ()
  (interactive)
  (counsel-fzf "" (guess-directory "fzf")))

(defun counsel-ag-in-project ()
  (interactive)
  (counsel-ag "" (guess-directory "ag") " --hidden "))


;; keys

(bind-keys* ("<f1>" . toggle-repl-window)
            ("<f2>" . dired-jump)
            ("<f3>" . counsel-fzf-in-project)
            ("<f4>" . counsel-ag-in-project)
            ("<f5>" . previous-buffer)
            ("<f6>" . next-buffer)
            ("<f7>" . recentf-open-files)
            ("<f8>" . exit-minibuffer)
            ("<f9>" . kill-current-buffer)
            ("<f10>" . delete-window)
            ("<f11>" . kill-buffer-and-window)
            ("<f12>" . universal-argument))

(use-package lisp-mode
  :mode "\\.cl\\'")

(use-package cider)
