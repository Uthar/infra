{ pkgs ? import <nixpkgs> {}, ... }:

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


    emacs' =
      (pkgs.emacs.override { nativeComp = true; srcRepo = true; })
      .overrideAttrs (o: {
        CFLAGS="-g3";
        dontStrip = true;
        src = fetchgit {
          url = https://galkowski.xyz/emacs;
          rev = "9c1bbad907575987054b8d81ac2d09bfabe6214b";
          sha256 = "0zacdwb5xfljnbbijf9z4qbsgr3xrknz7r8zvwzfm2z1ch3i6r9j";
        };
        version = "28.0.50";
        patches = [ ./tramp-detect-wrapped-gvfsd.patch ] ;
      });

    build-elisp-package = { name, src }:
      runCommand name {} ''
        mkdir -p $out/share/emacs/site-lisp
        cp -Tr ${src} $out/share/emacs/site-lisp
      '';

    modus-themes = build-elisp-package {
      name = "modus-themes";
      src = fetchTarball {
        url = http://elpa.gnu.org/packages/modus-themes-2.0.0.tar;
        sha256 = "122xg6wk2mn1c69kaqkqkgqkbw61n13x3ylwf5q2b2kr60skn1zh";
      };
    };

    withPatches = drv: patches: drv.overrideAttrs (o: { inherit patches; });

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
      modus-themes
      undo-tree
      which-key
    ])

    ++

    (with epkgs.melpaPackages; [
      ag
      anzu
      browse-kill-ring
      (withPatches cider [ ./cider-return-buffer-in-switch-to-repl-buffer.patch ])
      company-terraform
      diminish
      (withPatches direnv [ ./direnv-el-message-not-warning.patch ])
      editorconfig
      evil
      evil-anzu
      evil-collection
      evil-matchit
      evil-surround
      (withPatches flycheck [ ./flycheck-dont-message-suspicious.patch ])
      glsl-mode
      go-mode
      groovy-mode
      hl-todo
      lsp-mode
      lsp-python-ms
      magit
      nix-mode
      page-break-lines
      projectile
      rg
      ripgrep
      (withPatches slime [ ./slime-cl-indent-other-braces.patch ])
      slime-company
      terraform-mode
      use-package
      vc-fossil
      wgrep
      winum
      yaml-mode
    ])

  )
