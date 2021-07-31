{ pkgs, lib, ... }:

let
  pkgs' = pkgs.pkgsMusl;
  ecl = "${pkgs'.ecl}/bin/ecl";

  buildSbcl = xc: deps: flags: src:
    (pkgs'.callPackage
      (import "${<nixos>}/pkgs/development/compilers/sbcl/common.nix" {
        version = "2.1.6";
        sha256 = "sha256:1vydq6mmssla2vn0plpsrm50n3b42a464zxw72j6qihas9fhq8cb";
      }) {})
      .overrideAttrs(o:
        {
          inherit src;
          buildInputs = o.buildInputs ++  deps ;
          postPatch = o.postPatch + ''
            echo '"${o.version}"' > version.lisp-expr
          '';
          buildPhase = ''
            runHook preBuild

            sh make.sh --prefix=$out --xc-host=${xc} ${flags}
            (cd doc/manual ; make info)

            runHook postBuild
          '';
        });

  srcUpstream =
    builtins.fetchurl {
      url = "mirror://sourceforge/project/sbcl/sbcl/2.1.6/sbcl-2.1.6-source.tar.bz2";
      sha256 = "sha256:1vydq6mmssla2vn0plpsrm50n3b42a464zxw72j6qihas9fhq8cb";
    };

  srcStaticFork =
    pkgs.fetchFromGitHub {
      owner ="daewok";
      repo = "sbcl";
      rev ="f6f8e194dc04aaf55fb4a4589073dc917d038624";
      sha256 = "sha256:1drjy3pbyh0iik9rwm7j2aq5dlr7wvb7kmaj3wdsfdwyhg1hkdl6";
    };

  sbclMinimal = buildSbcl ecl [ pkgs'.zlib ] (lib.strings.concatStringsSep " "
                              [ "--without-asdf"
                                "--without-sb-core-compression"
                                "--without-sb-ldb"
                                "--without-sb-thread"
                                "--without-sb-unicode" ]) srcUpstream;

  sbclStatic = (buildSbcl "${sbclMinimal}/bin/sbcl" [ pkgs'.zlib.static pkgs.zlib.dev ] "--fancy --without-sb-ldb --with-sb-linkable-runtime --with-sb-prelink-linkage-table" srcStaticFork)
    .overrideAttrs(o: {
      pname = "sbclStatic";
      version = "2.1.5";
      postPatch = o.postPatch + ''
        sed -i "s,..LD..*-r,ld.gold -no-pie -r,g" "src/runtime/GNUmakefile"
        echo '"2.1.5-static"' > version.lisp-expr
      '';

      installPhase = o.installPhase + ''
        mkdir $out/tools-for-build
        cp $src/tools-for-build/create-linkage-table-prelink-info-override.lisp $out/tools-for-build
        cp $src/tools-for-build/dump-linkage-info.lisp $out/tools-for-build
      '';
    });

  sbclFancy = buildSbcl "${sbclMinimal}/bin/sbcl" [ pkgs'.zlib.static pkgs.zlib.dev ] "--fancy --without-sb-ldb" srcUpstream;

in sbclStatic

# (pkgs.callPackageasdf
# (import "${<nixos>}/pkgs/development/compilers/sbcl/common.nix" {
#   version = "2.1.6";
#   sha256 = "sha256:1vydq6mmssla2vn0plpsrm50n3b42a464zxw72j6qihas9fhq8cb";
# }) {})
# .overrideAttrs(o:
# {
#   buildInputs = o.buildInputs ++ [ pkgs.zlib ];
#   postPatch = o.postPatch + ''
#     echo '"${o.version}"' > version.lisp-expr
#   '';
#   buildPhase = ''
#     runHook preBuild

#     sh make.sh --prefix=$out --xc-host=${lisp} --fancy
#     (cd doc/manual ; make info)

#     runHook postBuild
#   '';
# })

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
