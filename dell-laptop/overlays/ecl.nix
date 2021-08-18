

{ pkgs, lib, stdenv, ... }:

with pkgs.pkgsMusl;
with lib.lists;

# These 2 don't matter right, but leaving it around for playing with
# static linking
let
  gmp-static = gmp.overrideAttrs(o:{
    dontDisableStatic = true;
    postInstall = ''
    rm -v $out/lib/libgmp.so*
    '';
  });
  libffi-static = libffi.overrideAttrs(o:{
    dontDisableStatic = true;
    postInstall = ''
    rm -v $out/lib64/libffi.so*
    '';
  });
in

# Use included libgmp, because when using an .so from /nix/store,
# (compile (defun foo ()())) fails unless in nix-shell --pure
# because it can't find libgmp

ecl.overrideAttrs (o: {

  dontDisableStatic = true;

  propagatedBuildInputs =
    subtractLists [ gmp libffi ] o.propagatedBuildInputs;

  configureFlags =
    (subtractLists
      [ "--with-gmp-prefix=${gmp.dev}"
        "--with-libffi-prefix=${libffi.dev}" ]
      o.configureFlags)
    ++ [ "--enable-gmp=included" ];

    postInstall = ''
    sed -e 's/@[-a-zA-Z_]*@//g' -i $out/bin/ecl-config
    wrapProgram "$out/bin/ecl" \
      --prefix PATH ':' "${gcc}/bin" \
      --prefix PATH ':' "${binutils-unwrapped}/bin"
  '';


})
