{ pkgs ? (import (builtins.fetchTarball {
    url    = "https://github.com/Uthar/nixpkgs/archive/316fdae6dcf20a344fd0501bd961b686c7f41733.tar.gz";
    sha256 = "04y6fjpz4blkl2h376qi5yb7l2ir2nfpxydcj3n0bg287sbv574a";
  }) {}) }:

with pkgs; with emacsPackagesNg;

  let

    clhs = runCommand
      "clhs"
      { src = fetchTarball {
          url = http://ftp.lispworks.com/pub/software_tools/reference/HyperSpec-7-0.tar.gz;
          sha256 = "1248sws9yk2wy1ajvn88m3cl8lii77961gax4mlka2899d69bkzs"; };}
      "cp -Tr $src $out";

    defaultEl = (runCommand "default.el" { inherit clhs; } ''
      mkdir -p $out/share/emacs/site-lisp
      substitute ${./default.el} $out/share/emacs/site-lisp/default.el \
        --replace xsel ${xsel}/bin/xsel
      substituteAllInPlace $out/share/emacs/site-lisp/default.el
    '');

    slime = pkgs.emacsPackages.slime.overrideAttrs (old: rec {
      src = fetchFromGitHub {
        owner = "uthar";
        repo = "slime";
        rev = "236662f406c5e3fa004b9f7f301a84c8620d1602";
        sha256 = "05s6xqys8yw3hy0jjz8f439pjlwg5wnh7sf4acy8xcdcvqwv820l";
      };
    });

    emacs' = emacs.overrideAttrs (o: {
      configureFlags = o.configureFlags ++ [ "CFLAGS=-g3" ];
      dontStrip = true;
    });

    emacsWithPackages = (emacsPackagesNgGen emacs').emacsWithPackages;

  in emacsWithPackages(epkgs:

    [
      defaultEl
      slime
    ]

    ++

    (with epkgs.melpaPackages; [
      ag
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
      magit
      nix-mode
      page-break-lines
      projectile
      rg
      ripgrep
      slime-company
      use-package
      wgrep
      which-key
      winum
    ])

  )
