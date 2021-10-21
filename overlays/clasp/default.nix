

{ pkgs, stdenv, lib, ... }:

with pkgs;

let

  Cleavir = builtins.fetchTarball {
    # other than upstream to pull in compiler fix
    url = https://github.com/s-expressionists/Cleavir/archive/ed8bdd47d3cea7d1ae80266e5def681519d7e8cd.tar.gz;
    sha256 = "077hj5h6lrkg07fzwqpsq0blnm5d14gxw1i8d28jaj4dvl5fqj3g";
  };
  Concrete-Syntax-Tree = builtins.fetchTarball {
    url = https://github.com/s-expressionists/Concrete-Syntax-Tree/archive/a56a5246fbaa90b98a29368c011a6616f2bcb482.tar.gz;
    sha256 = "015glkx1dx8dzlhm4hfzp0zzmydxakx48r12y01f55a8n6shxqn5";
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
    version = "dcaf594c1f";
    src = builtins.fetchTarball {
      # last commit not affected by https://github.com/clasp-developers/clasp/issues/1183
      url = https://github.com/clasp-developers/clasp/archive/dcaf594c1fe8158ac36e7fb634ad79b596911a75.tar.gz;
      sha256 = "1qzw3jx3vkj5xcp8llhx218qrhakigin76d2csrmkndfj94ip5dy";
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