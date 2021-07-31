{ pkgs
, lib
, stdenv
, sbclStatic
, pname ? "app"
, version ? "0.0.0"
, src ? null
, meta ? null
, ... }:

let
  dump-linkage-info =
    "${sbclStatic}/tools-for-build/dump-linkage-info.lisp";
  create-linkage-table-prelink-info-override =
    "${sbclStatic}/tools-for-build/create-linkage-table-prelink-info-override.lisp";
in

stdenv.mkDerivation rec {
  inherit pname version src meta;
  inherit sbclStatic;

  ## strip seems to remove the actual lisp core from the executable
  ## produced by save-lisp-and-die, so disable it here
  dontFixup = true;

  buildPhase = ''

    export ASDF_OUTPUT_TRANSLATIONS=${src}:$(pwd)

    # Add lisp deps and dynamic libs here

    ${sbclStatic}/bin/sbcl \
      --non-interactive --no-sysinit --no-userinit \
      --eval '(require :asdf)' \
      --eval '(pushnew "${src}/" asdf:*central-registry*)' \
      --eval '(asdf:load-system :${pname})' \
      --load ${dump-linkage-info} \
      --eval '(sb-dump-linkage-info:dump-to-file "linkage-info.sexp")' \
      --eval '(sb-ext:save-lisp-and-die "core")'

    ${sbclStatic}/bin/sbcl --no-sysinit --no-userinit \
      --script ${create-linkage-table-prelink-info-override} \
      linkage-info.sexp linkage-table-prelink-info-override.c

    ${sbclStatic.stdenv.cc}/bin/gcc -Wno-builtin-declaration-mismatch \
      -o linkage-table-prelink-info-override.o \
      -c linkage-table-prelink-info-override.c

    # Add static libs here

    ${sbclStatic.stdenv.cc}/bin/gcc -no-pie -static \
      -o static-sbcl  \
      ${sbclStatic}/lib/sbcl/sbcl.o  \
      linkage-table-prelink-info-override.o \
      -L${pkgs.zlib.static}/lib/ \
      -ldl -lpthread -lz  -lm

    ./static-sbcl --core ./core \
      --eval '(sb-ext:save-lisp-and-die "${pname}-static" :executable t :toplevel (lambda () (${pname}:main)) :compression t)'

  '';

  installPhase = ''
    mkdir -pv $out/bin
    cp -v ${pname}-static $out/bin
    cp -v static-sbcl $out/bin
  '';

}
