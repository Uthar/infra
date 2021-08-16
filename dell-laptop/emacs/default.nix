{ pkgs ? (import (builtins.fetchTarball {
    url    = "https://github.com/nixos/nixpkgs/archive/c06613c25df3fe1dd26243847a3c105cf6770627.tar.gz";
    sha256 = "16si1436wf3fcx91p6cy3qxaib8kr78qivbi69lq4m63n96gglkv";
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

    emacs' = emacs.overrideAttrs (o: {
      configureFlags = o.configureFlags ++ [ "CFLAGS=-g3" ];
      dontStrip = true;
    });

    emacsWithPackages = (emacsPackagesNgGen emacs').emacsWithPackages;

  in emacsWithPackages(epkgs:

    [
      defaultEl
    ]

    ++

    (with epkgs.elpaPackages; [
      company
      counsel
      ivy
      undo-tree
      which-key
    ])

    ++

    (with epkgs.melpaPackages; [
      ag
      anzu
      browse-kill-ring
      cider
      diminish
      (direnv.overrideAttrs(o:{ patches = [ ./direnv-el-message-not-warning.patch ]; }))
      doom-themes
      editorconfig
      evil
      evil-anzu
      evil-collection
      evil-matchit
      evil-surround
      flycheck
      git-timemachine
      go-mode
      hl-todo
      lsp-mode
      magit
      nix-mode
      page-break-lines
      projectile
      rg
      ripgrep
      slime
      slime-company
      use-package
      vc-fossil
      wgrep
      winum
    ])

  )
