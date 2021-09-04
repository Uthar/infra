{ super }:

super.sbcl.overrideAttrs(o: rec{

  version = "2.1.8";

  buildInputs = with super; o.buildInputs ++ [ zlib.static zlib.dev ];

  src = super.fetchurl {
    url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-source.tar.bz2";
    sha256 = "sha256:0ijmw8adyf6fgaq4ixdj9v5lfg3cs9wyxhb96wxhfld0rjpppsm3";
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
