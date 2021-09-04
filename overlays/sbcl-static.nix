{ pkgs, lib, ... }:

pkgs.pkgsMusl.sbcl.overrideAttrs(o: rec{

  pname = "sbcl-static";
  version = "2.1.5";

  buildInputs = with pkgs.pkgsMusl; o.buildInputs ++ [ zlib.static zlib.dev ];

  src =
    pkgs.fetchFromGitHub {
      owner ="daewok";
      repo = "sbcl";
      rev ="f6f8e194dc04aaf55fb4a4589073dc917d038624";
      sha256 = "sha256:1drjy3pbyh0iik9rwm7j2aq5dlr7wvb7kmaj3wdsfdwyhg1hkdl6";
    };

  postPatch = o.postPatch + ''
    echo '"${version}.nixos"' > version.lisp-expr
    sed -i "s,..LD..*-r,ld.gold -no-pie -r,g" "src/runtime/GNUmakefile"
  '';

  buildPhase = ''
    runHook preBuild

    sh make.sh --prefix=$out --xc-host=${pkgs.sbclBootstrap}/bin/sbcl --fancy --without-sb-ldb \
      --with-sb-linkable-runtime --with-sb-prelink-linkage-table

    (cd doc/manual ; make info)

    runHook postBuild
  '';

  installPhase = o.installPhase + ''
    mkdir $out/tools-for-build
    cp $src/tools-for-build/create-linkage-table-prelink-info-override.lisp $out/tools-for-build
    cp $src/tools-for-build/dump-linkage-info.lisp $out/tools-for-build
  '';

})
