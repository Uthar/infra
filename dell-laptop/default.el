(eval-when-compile
  (require 'use-package))

(use-package diminish)

(use-package restart-emacs
  :config
  (defun restart-emacs--get-emacs-binary ()
    (shell-command-to-string "readlink -n `which emacs`"))) ; hack for ~/.nix-profile

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
    (let ((n (1+ i)))
      (define-key diff-mode-map (kbd (format "M-%i" n)) nil)
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

(defun state-dir (dir)
  (expand-file-name (concat user-emacs-directory dir "/")))

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
  (backup-directory-alist `((".*" . ,(state-dir "backups"))))
  (auto-save-file-name-transforms `((".*" ,(state-dir "auto-save") t)))
  (auto-save-list-file-prefix (state-dir "auto-save"))
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
  (global-set-key (kbd "C-;") 'my/comment-or-uncomment-region)
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
  :hook
  (before-save . delete-trailing-whitespace)
  (after-save . executable-make-buffer-file-executable-if-script-p)
  ;(find-file . recentf-save-list)
  (prog-mode . display-line-numbers-mode)
  (dired-mode . dired-hide-details-mode)
  (after-init . (lambda () (set-cursor-color "#999"))))

(use-package evil
  :custom
  (evil-want-C-u-scroll t)
  (evil-kill-on-visual-paste nil)
  :config (evil-mode 1))

(use-package evil-surround
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

(use-package evil-anzu)

(use-package company
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-minimum-prefix-length 1)
  :diminish
  :hook (after-init . global-company-mode))

(use-package which-key
  :diminish
  :custom (which-key-dont-use-unicode t)
  :config (which-key-mode))

(use-package editorconfig
  :diminish
  :config (editorconfig-mode t))

;; remove this ? figure out recent files a differrent way
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

;; Remove this?
(use-package lsp-ui
  :custom (lsp-ui-doc-position 'top))

(use-package doom-themes
  :hook
  (after-init
   . (lambda ()
       (load-theme 'doom-gruvbox t))))

(use-package slime
  :custom
  (slime-truncate-lines nil)
  (slime-net-coding-system 'utf-8-unix)
  (slime-lisp-implementations
   '((sbcl ("sbcl" "--dynamic-space-size" "512"))
     (abcl ("abcl"))
     (ecl ("ecl"))))
  (slime-contribs
   '(slime-asdf
     slime-company
     slime-quicklisp
     ;; slime-fancy except slime-c-p-c, which breaks completion
     slime-autodoc
     slime-editing-commands
     slime-fancy-inspector
     slime-fancy-trace
     slime-fontifying-fu
     slime-fuzzy
     slime-indentation
     slime-macrostep
     slime-mdot-fu
     slime-package-fu
     slime-presentations
     slime-references
     slime-repl
     slime-scratch
     slime-trace-dialog))
  (slime-complete-symbol*-fancy t)
  (slime-repl-auto-right-margin t)
  (slime-repl-history-size 10000)
  (common-lisp-hyperspec-root
   (expand-file-name "~/archive/www/HyperSpec/HyperSpec/"))
  (common-lisp-hyperspec-symbol-table
   (expand-file-name "~/archive/www/HyperSpec/HyperSpec/Data/Map_Sym.txt"))
  (common-lisp-style-default
   (progn
     (require 'slime-cl-indent)
     (define-common-lisp-style "kpg"
       "Fix the indentation of some Clojure-like macros."
       (:inherit "modern")
       (:indentation
        (for (as handler-case))))))
  :bind ("C-c s" . 'slime-selector))

;; give keyword colors to symbols in cl-keywords
(let ((cl-keywords `("fn" "for" "defclass*")))
  (add-to-list
   'lisp-cl-font-lock-keywords-2
   `(,(concat "(" `,(regexp-opt cl-keywords t) "\\_>") . 1)))

(use-package slime-company
  :custom
  (slime-company-completion 'fuzzy))

(use-package lisp-mode
  :mode "\\.cl\\'")

(use-package cider)
