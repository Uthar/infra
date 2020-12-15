{ pkgs ? (import <nixpkgs> {}) }:

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
      epl
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
      hc-zenburn-theme
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
      dap-mode
      lsp-mode
      lsp-ui
      company-lsp
      lsp-python-ms
      doom-themes
      browse-kill-ring
    ])

  )
