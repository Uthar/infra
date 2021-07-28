import "${<nixos>}/pkgs/development/compilers/sbcl/common.nix" {
  version = "2.1.6";
  sha256 = "sha256:1vydq6mmssla2vn0plpsrm50n3b42a464zxw72j6qihas9fhq8cb";
}

# { pkgs, lib, stdenv, ... }:

# with pkgs;
# with pkgsMusl;

# let

#   sbcl =
#     pkgsMusl.sbcl.overrideAttrs(o: rec {

#       pname = "sbcl";
#       version = "2.1.6";

#       src = fetchurl {
#         url = "mirror://sourceforge/project/sbcl/sbcl/${version}/${pname}-${version}-source.tar.bz2";
#         sha256 = "sha256:1vydq6mmssla2vn0plpsrm50n3b42a464zxw72j6qihas9fhq8cb";
#       };

#       # src = fetchFromGitHub {
#       #   owner = "daewok";
#       #   repo = "sbcl";
#       #   rev = "13032a9043a6af9b049ef2a64011f1b7d92938ed";
#       #   sha256="0clr5asw936lbj0yg0n3a916q5hwls94ib19d5pq31yrqqyik2cj";
#       # };

#       buildInputs = o.buildInputs ++ [ zlib ];

#       buildPhase = ''
#         runHook preBuild;
#         sh make.sh --prefix=$out --xc-host=${pkgsMusl.ecl}/bin/ecl --fancy --with-sb-thread --with-sb-linkable-runtime --with-sb-prelink-linkage-table;
#         (cd doc/manual; make info);
#         runHook postBuild;
#       '';

#       postPatch = o.postPatch + ''
#         sed -i "s,..LD..*-r,ld.gold -no-pie -r,g" "src/runtime/GNUmakefile"
#         echo '"${version}"' > version.lisp-expr
#       '';
#     });

# in sbcl
