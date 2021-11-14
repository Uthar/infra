{ pkgs, build-asdf-system, flattenedDeps, ... }:

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
    cl-opengl = {
      nativeLibs = [ libGL ];
    };
    cl-glu = {
      nativeLibs = [ libGLU ];
    };
    cl-glut = {
      nativeLibs = [ freeglut ];
    };
    lev = {
      nativeLibs = [ libev ];
    };
    cl-rdkafka = {
      nativeBuildInputs = [ rdkafka ];
      nativeLibs = [ rdkafka ];
    };
    cl-async-ssl = {
      nativeLibs = [ openssl ];
    };
  };

  # NOTE:
  # You might need https://github.com/NixOS/nix/commit/cd44c0af71ace2eb8056c2b26b9249a5aa102b41
  # and a couple gigs of ram, as some derivations are HUGE
  #
  # So, if you get stack overflow with nix <=2.3, try again with `ulimit -s 65536`
  #
  # FIXME: mark those broken on nix.lisp level for speed
  broken = [
    # broken packaging in quicklisp: clml.data.r-datasets depends on
    # a non-existing system clml.data.r-datasets-package
    # FIXME: patch the clml.data.r-datasets-package system manually in
    # systems.txt before running nix.lisp
    # (n: v: hasPrefix "clml" n)

    # Broken upstream: see https://gitlab.common-lisp.net/antik/antik/-/issues/4
    # (n: v: hasPrefix "antik" n)
    # (n: v: any (dep: hasPrefix "antik" dep.pname) (flattenedDeps v.lispLibs))

    # FIXME: for cl-unicode (and other cases where the build process
    # generates new source files), try to rebuild any failing library
    # again with source translations into pwd, then rebuild normallny
    # with the previous one as source
  ];

  qlpkgs =
    if builtins.pathExists ./from-quicklisp.nix
    then filterAttrs (n: v: all (check: !(check n v)) broken) (import ./from-quicklisp.nix { inherit pkgs; })
    else {};

  build = pkg:
    (build-asdf-system
      (pkg // {
        lispLibs = map build pkg.lispLibs;
      }
      // (optionalAttrs (hasAttr pkg.pname extras) extras.${pkg.pname})));

in mapAttrs (n: v: build v) qlpkgs
