{ pkgs ? (import (builtins.fetchTarball {
    url    = "https://github.com/Uthar/nixpkgs/archive/316fdae6dcf20a344fd0501bd961b686c7f41733.tar.gz";
    sha256 = "04y6fjpz4blkl2h376qi5yb7l2ir2nfpxydcj3n0bg287sbv574a";
  }) {}) }:

with pkgs; with emacsPackagesNg;

  let

    defaultEl = (runCommand "default.el" {} ''
      mkdir -p $out/share/emacs/site-lisp
      cp ${./default.el} $out/share/emacs/site-lisp/default.el
    '');

    emacsWithPackages = (emacsPackagesNgGen emacs).emacsWithPackages;

  in emacsWithPackages(epkgs:

    [ defaultEl ]

    ++

    (with epkgs.melpaPackages; [
      anzu
      browse-kill-ring
      cider
      company
      counsel
      dap-mode
      dashboard
      diminish
      direnv
      doom-themes
      editorconfig
      evil
      evil-anzu
      evil-magit
      evil-matchit
      evil-surround
      flycheck
      gcmh
      git-timemachine
      go-mode
      hl-todo
      ivy
      lsp-mode
      lsp-ui
      magit
      neotree
      nix-mode
      page-break-lines
      projectile
      ranger
      restart-emacs
      rg
      slime
      slime-company
      smartparens
      use-package
      wgrep
      which-key
      winum
    ])

  )
