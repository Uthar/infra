

{ pkgs, stdenv, lib, ... }:

let

  pkgs_unstable = import (builtins.fetchTarball {
    url = https://github.com/nixos/nixpkgs/archive/e16c267e4899b2d87655f0c45ec96235bfe6a258.tar.gz;
    sha256 = "1x4nx4hfcmhyib6c83sryyf33vjzxsh5kkr3mqwhfg7agwhcdhi4";
  }) {};

  Cleavir = pkgs.fetchgit {
    url = https://github.com/s-expressionists/Cleavir;
    rev = "715266bc877bf1c15d2c5e471a9e2d2d3ca5db6c";
    sha256 = "0aa0a5hsjqzpy1s7pr2lmyzbsyhfrqfa41z54js6vgxf56p92gdg";
    leaveDotGit = true;
  };
  Concrete-Syntax-Tree = pkgs.fetchgit {
    url = https://github.com/s-expressionists/Concrete-Syntax-Tree;
    rev = "a56a5246fbaa90b98a29368c011a6616f2bcb482";
    sha256 = "10nw9a0kxim87a8nrf3xpcqm70ahpcgmgpi0gxphmrr74iqx5n9q";
    leaveDotGit = true;
  };
  closer-mop = pkgs.fetchgit {
    url = https://github.com/pcostanza/closer-mop.git;
    rev = "d4d1c7aa6aba9b4ac8b7bb78ff4902a52126633f";
    sha256 = "1jvp1hv82bfvqzgvy5kd5rhryibd46nn0fsjn9ih337axi4p7cvc";
    leaveDotGit = true;
  };
  Acclimation = pkgs.fetchgit {
    url = https://github.com/robert-strandh/Acclimation.git;
    rev = "dd15c86b0866fc5d8b474be0da15c58a3c04c45c";
    sha256 = "0ndwk1jz9gswh7hws68diay1skkps6zdsm8fz9qg00idz10pl8c9";
    leaveDotGit = true;
  };
  Eclector = pkgs.fetchgit {
    url = https://github.com/s-expressionists/Eclector.git;
    rev = "e92cf239783be90c97e80aff2a14d65778a38325";
    sha256 = "1avf606n3hg8rhjvhhfqbhls9q9cxzf0vgyn8ibb7zdaf4zz539h";
    leaveDotGit = true;
  };
  alexandria = pkgs.fetchgit {
    url = https://github.com/clasp-developers/alexandria.git;
    rev = "e5c54bc30b0887c237bde2827036d17315f88737";
    sha256 = "07krnwavqfj1cfdbxincv6l4y5anqawa7jw4q7990dacbcszvxq1";
    leaveDotGit = true;
  };
  mps = pkgs.fetchgit {
    url = https://github.com/Ravenbrook/mps.git;
    rev = "b8a05a3846430bc36c8200f24d248c8293801503";
    sha256 = "0i53b7f0qyh05xxppfi05c5i8rqr7mcwh8p4kr3b28jb8nf6pkxz";
    leaveDotGit = true;
  };
  asdf = pkgs.fetchgit {
    url = https://gitlab.common-lisp.net/asdf/asdf.git;
    rev = "3.3.3.5";
    sha256 = "16k3dw25imr9sjl57yl1yzsqs4njfd9gba05vj914c1f9zzgkhj7";
    leaveDotGit = true;
  };

in

with pkgs;

llvmPackages_9.stdenv.mkDerivation {
  pname = "clasp";
  version = "0.9-b14e329f49";
  src = fetchgit {
    url = https://github.com/clasp-developers/clasp;
    rev = "b14e329f49998275579926da2a737885ceb2cea7";
    sha256 = "0sgh3r6vlik1g5zvc721if6nil1qk0rf5y65yp97q9y2xz4lb6sr";
  };
  preConfigure = ''
    ./waf configure
  '';
  postPatch = ''
    substituteInPlace waf \
      --replace '/usr/bin/env python' ${python3.interpreter}
    substituteInPlace wscript \
      --replace 'https://github.com/s-expressionists/Cleavir' ${Cleavir} \
      --replace 'https://github.com/s-expressionists/Concrete-Syntax-Tree.git' ${Concrete-Syntax-Tree} \
      --replace 'https://github.com/pcostanza/closer-mop.git' ${closer-mop} \
      --replace 'https://github.com/robert-strandh/Acclimation.git' ${Acclimation} \
      --replace 'https://github.com/s-expressionists/Eclector.git' ${Eclector} \
      --replace 'https://github.com/clasp-developers/alexandria.git' ${alexandria} \
      --replace 'https://github.com/Ravenbrook/mps.git' ${mps} \
      --replace 'https://gitlab.common-lisp.net/asdf/asdf.git' ${asdf}

    # Simply exit right after cloning the repo, revisions are set on nix side
    substituteInPlace tools-for-build/fetch-git-revision.sh \
      --replace 'cd "$path" || exit $?' 'cd "$path" && exit 0'
  '';
  buildInputs =
    [ python310 git sbcl gmp libffi boehmgc libelf libbsd
      boost175.dev boost175
      llvm_9.dev
      llvmPackages_9.libclang
    ];
}

# { pkgs, stdenv, lib, ... }:

# let

#   pkgs_unstable = import (builtins.fetchTarball {
#     url = https://github.com/nixos/nixpkgs/archive/e16c267e4899b2d87655f0c45ec96235bfe6a258.tar.gz;
#     sha256 = "1x4nx4hfcmhyib6c83sryyf33vjzxsh5kkr3mqwhfg7agwhcdhi4";
#   }) {};

#   Cleavir = pkgs.fetchgit {
#     url = https://github.com/s-expressionists/Cleavir;
#     rev = "3884e9325acf8db4d45d2a0e86378285ecbc2926";
#     sha256 = "10k4agbqp1dgcy6jd72lg02n47iafjr725cgc91fxv0bl1n7xdhl";
#     leaveDotGit = true;
#   };
#   Concrete-Syntax-Tree = pkgs.fetchgit {
#     url = https://github.com/s-expressionists/Concrete-Syntax-Tree;
#     rev = "4f01430c34f163356f3a2cfbf0a8a6963ff0e5ac";
#     sha256 = "0ak9n3xfghd3wzv4r4hgpjka1dlv2hhsqwzibvipbgjb6vpgf03x";
#     leaveDotGit = true;
#   };
#   closer-mop = pkgs.fetchgit {
#     url = https://github.com/pcostanza/closer-mop.git;
#     rev = "d4d1c7aa6aba9b4ac8b7bb78ff4902a52126633f";
#     sha256 = "1jvp1hv82bfvqzgvy5kd5rhryibd46nn0fsjn9ih337axi4p7cvc";
#     leaveDotGit = true;
#   };
#   Acclimation = pkgs.fetchgit {
#     url = https://github.com/robert-strandh/Acclimation.git;
#     rev = "dd15c86b0866fc5d8b474be0da15c58a3c04c45c";
#     sha256 = "0ndwk1jz9gswh7hws68diay1skkps6zdsm8fz9qg00idz10pl8c9";
#     leaveDotGit = true;
#   };
#   Eclector = pkgs.fetchgit {
#     url = https://github.com/s-expressionists/Eclector.git;
#     rev = "e92cf239783be90c97e80aff2a14d65778a38325";
#     sha256 = "1avf606n3hg8rhjvhhfqbhls9q9cxzf0vgyn8ibb7zdaf4zz539h";
#     leaveDotGit = true;
#   };
#   alexandria = pkgs.fetchgit {
#     url = https://github.com/clasp-developers/alexandria.git;
#     rev = "e5c54bc30b0887c237bde2827036d17315f88737";
#     sha256 = "07krnwavqfj1cfdbxincv6l4y5anqawa7jw4q7990dacbcszvxq1";
#     leaveDotGit = true;
#   };
#   mps = pkgs.fetchgit {
#     url = https://github.com/Ravenbrook/mps.git;
#     rev = "b8a05a3846430bc36c8200f24d248c8293801503";
#     sha256 = "0i53b7f0qyh05xxppfi05c5i8rqr7mcwh8p4kr3b28jb8nf6pkxz";
#     leaveDotGit = true;
#   };
#   asdf = pkgs.fetchgit {
#     url = https://gitlab.common-lisp.net/asdf/asdf.git;
#     rev = "3.3.3.5";
#     sha256 = "16k3dw25imr9sjl57yl1yzsqs4njfd9gba05vj914c1f9zzgkhj7";
#     leaveDotGit = true;
#   };

# in

# with pkgs;

# pkgs_unstable.llvmPackages_13.stdenv.mkDerivation {
#   pname = "clasp";
#   version = "0.9-23bf6aa3dc";
#   src = fetchgit {
#     url = https://github.com/clasp-developers/clasp;
#     rev = "23bf6aa3dcba5f8687cd22946f3a06e195552ce3";
#     sha256 = "1whc6jrqhy6jsin0g63yblczpp8j8vdfskr16b40332y687p2wmd";
#   };
#   preConfigure = ''
#     ./waf configure
#   '';
#   patches = [
#     ./TargetProcessControl-renamed-to-ExecutorProcessControl.patch
#     ./toString-fix-nargs.patch
#   ];
#   postPatch = ''
#     substituteInPlace waf \
#       --replace '/usr/bin/env python3' ${python3.interpreter}
#     substituteInPlace wscript \
#       --replace 'https://github.com/s-expressionists/Cleavir' ${Cleavir} \
#       --replace 'https://github.com/s-expressionists/Concrete-Syntax-Tree.git' ${Concrete-Syntax-Tree} \
#       --replace 'https://github.com/pcostanza/closer-mop.git' ${closer-mop} \
#       --replace 'https://github.com/robert-strandh/Acclimation.git' ${Acclimation} \
#       --replace 'https://github.com/s-expressionists/Eclector.git' ${Eclector} \
#       --replace 'https://github.com/clasp-developers/alexandria.git' ${alexandria} \
#       --replace 'https://github.com/Ravenbrook/mps.git' ${mps} \
#       --replace 'https://gitlab.common-lisp.net/asdf/asdf.git' ${asdf}

#     # Simply exit right after cloning the repo, revisions are set on nix side
#     substituteInPlace tools-for-build/fetch-git-revision.sh \
#       --replace 'cd "$path" || exit $?' 'cd "$path" && exit 0'
#   '';
#   buildInputs =
#     [ python310 git sbcl gmp libffi boehmgc libelf libbsd
#       pkgs_unstable.boost175.dev pkgs_unstable.boost175
#       pkgs_unstable.llvm_13.dev
#       pkgs_unstable.llvmPackages_13.libclang
#     ];
# }
