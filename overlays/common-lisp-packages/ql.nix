{ pkgs, ... }:

with pkgs;
with lib;
with lib.lists;
with lib.strings;

let

  # FIXME: automatically add nativeLibs based on conditions signalled

  extras = {
    "cl+ssl" = {
      nativeLibs = [ openssl ];
    };
    cffi-libffi = {
      nativeBuildInputs = [ libffi ];
      nativeLibs = [ libffi ];
    };
    cl-rabbit = {
      nativeBuildInputs = [ rabbitmq-c ];
      nativeLibs = [ rabbitmq-c ];
    };
    trivial-ssh-libssh2 = {
      nativeLibs = [ libssh2 ];
    };
    sqlite = {
      nativeLibs = [ sqlite ];
    };
    cl-libuv = {
      nativeBuildInputs = [ libuv ];
      nativeLibs = [ libuv ];
    };
    cl-liballegro = {
      # build doesnt fail without this, but fails on runtime
      # weird...
      nativeLibs = [ allegro5 ];
    };
    sdl2 = {
      nativeLibs = [ SDL2 ];
    };
  };

  # NOTE:
  # You might need https://github.com/NixOS/nix/commit/cd44c0af71ace2eb8056c2b26b9249a5aa102b41
  # and a couple gigs of ram, as some derivations are HUGE
  broken = [
    # broken packaging in quicklisp: clml.data.r-datasets depends on
    # a non-existing system clml.data.r-datasets-package
    # FIXME: patch the clml.data.r-datasets-package system manually in
    # systems.txt before running nix.lisp
    (x: hasPrefix "clml" x)

    # FIXME: for cl-unicode (and other cases where the build process
    # generates new source files), try to rebuild any failing library
    # again with source translations into pwd, then rebuild normallny
    # with the previous one as source
  ];

  qlpkgs = filterAttrs (n: v: any (check: !(check n)) broken) (import ./from-quicklisp.nix { inherit pkgs; });
  # qlpkgs = (import ./from-quicklisp.nix { inherit pkgs; });

  build = pkg:
    (commonLispPackages.build-asdf-system
      (pkg // {
        lisp = "${sbcl}/bin/sbcl --script";
        lispLibs = map build pkg.lispLibs;
      }
      // (optionalAttrs (hasAttr pkg.pname extras) extras.${pkg.pname})));

in mapAttrs (n: v: build v) qlpkgs
