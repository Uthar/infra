{ super }:

super.stdenv.mkDerivation rec {

  pname = "sbcl";

  version = "2.2.3";

  buildInputs = [ super.zlib.static super.zlib.dev ];

  src = super.fetchurl {
    url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-source.tar.bz2";
    sha256 = "sha256-3n9J4fd1D9LNiREe9wZBzFRxNV9iG3NzkqxoqpXzf58=";
  };

  postPatch = ''
    echo '"${version}.nix"' > version.lisp-expr
  '';

  buildPhase = ''
    sh make.sh --prefix=$out --xc-host=${super.ccl}/bin/ccl --fancy --without-sb-ldb
  '';

  installPhase = ''
    INSTALL_ROOT=$out sh install.sh
  '';

}
