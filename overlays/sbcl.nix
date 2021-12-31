{ super }:

super.sbcl.overrideAttrs(o: rec{

  version = "2.2.0";

  buildInputs = with super; o.buildInputs ++ [ zlib.static zlib.dev ];

  src = super.fetchurl {
    url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-source.tar.bz2";
    sha256 = "sha256:1im5v9c89pzc4iynbv03jhfb6k53cy695ac6lj69dsbam1z9axi2";
  };

  postPatch = o.postPatch + ''
    echo '"${version}.nixos"' > version.lisp-expr
  '';

  buildPhase = ''
    runHook preBuild

    sh make.sh --prefix=$out --xc-host=${super.sbclBootstrap}/bin/sbcl --fancy --without-sb-ldb
    (cd doc/manual ; make info)

    runHook postBuild
  '';

})
