{ super }:

super.stdenv.mkDerivation rec {

  pname = "sbcl";

  version = "2.2.4";

  buildInputs = [ super.zlib.static super.zlib.dev super.texinfo ];

  src = super.fetchurl {
    url = "mirror://sourceforge/project/sbcl/sbcl/${version}/sbcl-${version}-source.tar.bz2";
    hash = "sha256-/N0lHLxl9/gI7QrXckaEjRvhZqppoX90mWABhLelcgI=";
  };

  postPatch = ''
    echo '"${version}.nix"' > version.lisp-expr
  '';

  buildPhase = ''
    sh make.sh --prefix=$out --xc-host=${super.ccl}/bin/ccl --fancy --without-sb-ldb
   (cd doc/manual && make info)
  '';

  installPhase = ''
    INSTALL_ROOT=$out sh install.sh
  '';

}
