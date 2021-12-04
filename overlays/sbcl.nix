{ super }:

super.sbcl.overrideAttrs(o: rec{

  version = "2.1.11";

  buildInputs = with super; o.buildInputs ++ [ zlib.static zlib.dev ];

  src = super.fetchurl {
    url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-source.tar.bz2";
    sha256 = "sha256:1zgypmn19c58pv7j33ga7m1l7lzghj70w3xbybpgmggxwwflihdz";
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
