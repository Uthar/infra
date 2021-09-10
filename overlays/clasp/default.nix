

{ pkgs, stdenv, lib, ... }:

with pkgs;

let

  Cleavir = builtins.fetchTarball {
    url = https://github.com/s-expressionists/Cleavir/archive/dbb46cf1054d8d5e7222585a14eb121b62e62f90.tar.gz;
    sha256 = "0w3m0f8mm0p7kcv359y1af3adkjdlma2l50wp19d4pjwlmmqqnyl";
  };
  Concrete-Syntax-Tree = builtins.fetchTarball {
    url = https://github.com/s-expressionists/Concrete-Syntax-Tree/archive/4f01430c34f163356f3a2cfbf0a8a6963ff0e5ac.tar.gz;
    sha256 = "169ibaz1vv7pphib28443zzk3hf1mrcarhzfm8hnbdbk529cnxyi";
  };
  closer-mop = builtins.fetchTarball {
    url = https://github.com/pcostanza/closer-mop/archive/d4d1c7aa6aba9b4ac8b7bb78ff4902a52126633f.tar.gz;
    sha256 = "1amcv0f3vbsq0aqhai7ki5bi367giway1pbfxyc47r7q3hq5hw3c";
  };
  Acclimation = builtins.fetchTarball {
    url = https://github.com/robert-strandh/Acclimation/archive/dd15c86b0866fc5d8b474be0da15c58a3c04c45c.tar.gz;
    sha256 = "0ql224qs3zgflvdhfbca621v3byhhqfb71kzy70bslyczxv1bsh2";
  };
  Eclector = builtins.fetchTarball {
    url = https://github.com/s-expressionists/Eclector/archive/e92cf239783be90c97e80aff2a14d65778a38325.tar.gz;
    sha256 = "1srik177l76kf2hr5z7brp1fhzwfdqy3fkzrmyi9iyrfw547pl7y";
  };
  alexandria = builtins.fetchTarball {
    url = https://github.com/clasp-developers/alexandria/archive/e5c54bc30b0887c237bde2827036d17315f88737.tar.gz;
    sha256 = "14h7a9fwimiw9gqxjm2h47d95bfhrm7b81f6si7x8vy18d78fn4g";
  };
  mps = builtins.fetchTarball {
    url = https://github.com/Ravenbrook/mps/archive/b8a05a3846430bc36c8200f24d248c8293801503.tar.gz;
    sha256 = "1q2xqdw832jrp0w9yhgr8xihria01j4z132ac16lr9ssqznkprv6";
  };
  asdf = builtins.fetchTarball {
    url = https://gitlab.common-lisp.net/asdf/asdf/-/archive/3.3.3.5/asdf-3.3.3.5.tar.gz;
    sha256 = "0jqj9qwxzx81rpsab784akyxbv2xh60m5yx3w5gjh0agq0y5bdki";
  };

clasp =

  # Gotta use the right commit of llvm: 972b6a3a3471c2a742c5c5d8ec004ff640d544c4
  llvmPackages_clasp.stdenv.mkDerivation {
    pname = "clasp";
    version = "1.0-23bf6aa3dc";
    src = builtins.fetchTarball {
      url = https://github.com/clasp-developers/clasp/archive/8b749f632ee7dfd44b9d594997e60d696d4af72f.tar.gz;
      sha256 = "14dgy8haan9aij09v1iw1632rnhk1w30xr19zzx5fralky6zkvpd";
    };
    preConfigure = ''
    ./waf configure
  '';
    postPatch = ''
      substituteInPlace waf \
        --replace '/usr/bin/env python3' ${python3.interpreter}

      mkdir -pv src/lisp/kernel/contrib/Cleavir
      mkdir -pv src/lisp/kernel/contrib/Concrete-Syntax-Tree
      mkdir -pv src/lisp/kernel/contrib/closer-mop
      mkdir -pv src/lisp/kernel/contrib/Acclimation
      mkdir -pv src/lisp/kernel/contrib/Eclector
      mkdir -pv src/lisp/kernel/contrib/alexandria
      mkdir -pv src/mps
      mkdir -pv src/lisp/modules/asdf

      cp -rfT "${Cleavir}" src/lisp/kernel/contrib/Cleavir
      cp -rfT "${Concrete-Syntax-Tree}" src/lisp/kernel/contrib/Concrete-Syntax-Tree
      cp -rfT "${closer-mop}" src/lisp/kernel/contrib/closer-mop
      cp -rfT "${Acclimation}" src/lisp/kernel/contrib/Acclimation
      cp -rfT "${Eclector}" src/lisp/kernel/contrib/Eclector
      cp -rfT "${alexandria}" src/lisp/kernel/contrib/alexandria
      cp -rfT "${mps}" src/mps
      cp -rfT "${asdf}" src/lisp/modules/asdf

     substituteInPlace tools-for-build/fetch-git-revision.sh \
       --replace 'cd "$path" || exit $?' 'cd "$path" && exit 0'

      substituteInPlace include/clasp/core/character.h \
        --replace "en_US.UTF-8" ""

      echo "PREFIX = \"$out\"" > wscript.config
    '';
    buildInputs =
      [ python310 git sbcl gmp libffi boehmgc libelf libbsd
        boost175.dev boost175
        llvmPackages_clasp.llvm.dev
        llvmPackages_clasp.libclang
      ];
  };

fixup = clasp: stdenv.mkDerivation {
  inherit (clasp) pname version;
  src = clasp;
  buildPhase = ''
    mkdir -pv $out
    cp -r * $out
    rm -rf $out/lib/clasp/src/lisp/kernel/contrib/{alexandria,closer-mop}
    mv $out/bin/iclasp-boehm $out/bin/clasp
  '';
  dontInstall = true;
  dontFixup = true;
  dontStrip = true;
};

in fixup clasp
