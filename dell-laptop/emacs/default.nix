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

    slime = pkgs.emacsPackages.slime.overrideAttrs (old: rec {
      src = fetchFromGitHub {
        owner = "uthar";
        repo = "slime";
        rev = "236662f406c5e3fa004b9f7f301a84c8620d1602";
        sha256 = "05s6xqys8yw3hy0jjz8f439pjlwg5wnh7sf4acy8xcdcvqwv820l";
      };
    });

    emacsWithPackages = (emacsPackagesNgGen emacs).emacsWithPackages;

  in emacsWithPackages(epkgs:

    [
      defaultEl
      slime
    ]

    ++

    (with epkgs.melpaPackages; [
      anzu
      browse-kill-ring
      cider
      company
      counsel
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
      git-timemachine
      go-mode
      hl-todo
      ivy
      lsp-mode
      lsp-ui
      magit
      nix-mode
      page-break-lines
      projectile
      restart-emacs
      rg
      slime-company
      smartparens
      use-package
      wgrep
      which-key
      winum
    ])

  )
