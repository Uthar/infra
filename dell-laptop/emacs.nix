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
      use-package
      diminish
      restart-emacs
      gcmh
      direnv
      rg
      wgrep
      git-timemachine
      neotree
      hl-todo
      projectile
      smartparens
      magit
      evil-magit
      winum
      ivy
      counsel
      nix-mode
      ranger
      evil
      evil-surround
      anzu
      evil-anzu
      company
      which-key
      editorconfig
      dashboard
      flycheck
      go-mode
      dap-mode
      lsp-mode
      lsp-ui
      doom-themes
      slime
      slime-company
      browse-kill-ring
      page-break-lines
      evil-matchit
      cider
    ])

  )
