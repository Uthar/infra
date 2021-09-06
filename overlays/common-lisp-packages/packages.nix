{ build-asdf-system, pkgs, ... }:

rec {

  alexandria = build-asdf-system {
    pname = "alexandria";
    version = "1.4-20210903";
    src = builtins.fetchTarball {
      url = "https://gitlab.common-lisp.net/alexandria/alexandria/-/archive/v1.4/alexandria-v1.4.tar.gz";
      sha256 = "0r1adhvf98h0104vq14q7y99h0hsa8wqwqw92h7ghrjxmsvz2z6l";
    };
  };

  queues = build-asdf-system {
    pname = "queues";
    version = "47d4da65e9";
    src = pkgs.fetchgit {
      url = "https://github.com/oconnore/queues";
      rev = "47d4da65e9ea20953b74aeeab7e89a831b66bc94";
      sha256 = "0wdhfnzi4v6d97pggzj2aw55si94w4327br94jrmyvwf351wqjvv";
    };
  };

  queues-simple-queue = build-asdf-system {
    pname = "queues-simple-queue";
    inherit (queues) src version;
    buildInputs = [ queues ];
    systems = [ "queues.simple-queue" ];
  };

  queues-simple-cqueue = build-asdf-system {
    pname = "queues-simple-cqueue";
    inherit (queues) src version;
    buildInputs = [ queues queues-simple-queue bordeaux-threads ];
    systems = [ "queues.simple-cqueue" ];
  };

  cl-threadpool = build-asdf-system {
    pname = "cl-threadpool";
    version = "3.0.0";
    src = builtins.fetchTarball {
      url = https://github.com/Frechmatz/cl-threadpool/archive/refs/tags/v3.0.0.tar.gz;
      sha256 = "0yg09fpzqbmga1vx0p956vx6fyjmrgczb108fr0djswfn1mdiq3j";
    };
    buildInputs = [ bordeaux-threads queues-simple-cqueue ];
  };

  "babel" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/babel/2020-09-25/babel-20200925-git.tgz";
      sha256 = "04frn19mngvsh8bh7fb1rfjm8mqk8bgzx5c43dg7z02nfsxkqqak";
    };
    version = "20200925";
    pname = "babel";
    buildInputs = [
      alexandria
      trivial-features
    ];
  };

  "bordeaux-threads" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/bordeaux-threads/2020-06-10/bordeaux-threads-v0.8.8.tgz";
      sha256 = "19i443fz3488v1pbbr9x24y8h8vlyhny9vj6c9jk5prm702awrp6";
    };
    version = "0.8.8";
    pname = "bordeaux-threads";
    buildInputs = [
      alexandria
    ];
  };


  "cffi" = let
    jna = pkgs.fetchMavenArtifact {
      groupId = "net.java.dev.jna";
      artifactId = "jna";
      version = "5.9.0";
      sha256 = "0qbis8acv04fi902qzak1mbagqaxcsv2zyp7b8y4shs5nj0cgz7a";
    };
  in build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cffi/2021-04-11/cffi_0.24.1.tgz";
      sha256 = "17ryim4xilb1rzxydfr7595dnhqkk02lmrbkqrkvi9091shi4cj3";
    };
    version = "0.24.1";
    pname = "cffi";
    buildInputs = [
      alexandria
      babel
      trivial-features
    ];
    javaLibs = [ jna ];
    systems = [ "cffi" "cffi-grovel" "cffi-toolchain" ];
  };

  "cffi-grovel" = cffi;
  #   build-asdf-system {
  #   src =  builtins.fetchTarball {
  #     url = "http://beta.quicklisp.org/archive/cffi/2021-04-11/cffi_0.24.1.tgz";
  #     sha256 = "17ryim4xilb1rzxydfr7595dnhqkk02lmrbkqrkvi9091shi4cj3";
  #   };
  #   version = "0.24.1";
  #   pname = "cffi-grovel";
  #   buildInputs = [
  #     alexandria
  #     cffi-toolchain
  #     cffi
  #   ];
  # };

  "cffi-toolchain" = cffi;
  #   build-asdf-system {
  #   src =  builtins.fetchTarball {
  #     url = "http://beta.quicklisp.org/archive/cffi/2021-04-11/cffi_0.24.1.tgz";
  #     sha256 = "17ryim4xilb1rzxydfr7595dnhqkk02lmrbkqrkvi9091shi4cj3";
  #   };
  #   version = "0.24.1";
  #   pname = "cffi-toolchain";
  #   buildInputs = [
  #     cffi
  #   ];
  # };

  "chipz" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/chipz/2019-02-02/chipz-20190202-git.tgz";
      sha256 = "1l6cvks7slp5a0wag5vhbhn8972lfxamci59jd1ai4icv1vv1jsk";
    };
    version = "20190202";
    pname = "chipz";
    buildInputs = [
    ];
  };

  "chunga" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/chunga/2020-04-27/chunga-20200427-git.tgz";
      sha256 = "1m5mf0lks32k492gc1ys4ngy3vwgxlccg3966alrhy6q8m2jdcym";
    };
    version = "20200427";
    pname = "chunga";
    buildInputs = [
      trivial-gray-streams
    ];
  };

  "cl-ssl" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl+ssl/2021-04-11/cl+ssl-20210411-git.tgz";
      sha256 = "1501wg3353sd1fwichcbnk4lkmlsqdfk0ic9pnc13wlp22z2ffph";
    };
    version = "20210411";
    pname = "cl+ssl";
    buildInputs = [
      alexandria
      bordeaux-threads
      cffi
      flexi-streams
      trivial-features
      trivial-garbage
      trivial-gray-streams
      usocket
    ];
    nativeLibs = [ pkgs.openssl ];
  };

  "cl-base64" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-base64/2020-10-16/cl-base64-20201016-git.tgz";
      sha256 = "12jj54h0fs6n237cvnp8v6hn0imfksammq22ys6pi0gwz2w47rbj";
    };
    version = "20201016";
    pname = "cl-base64";
  };

  "cl-cookie" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-cookie/2019-10-07/cl-cookie-20191007-git.tgz";
      sha256 = "1kphfjbh9hzjc95ad7mpfsb0x7d8f7xznlaskr8awymspbmck8cz";
    };
    version = "20191007";
    pname = "cl-cookie";
    buildInputs = [
      alexandria
      cl-ppcre
      local-time
      proc-parse
      quri
    ];
  };

  "cl-json" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-json/2014-12-17/cl-json-20141217-git.tgz";
      sha256 = "0fx3m3x3s5ji950yzpazz4s0img3l6b3d6l3jrfjv0lr702496lh";
    };
    version = "20141217";
    pname = "cl-json";
  };

  "cl-unicode" = with builtins; let
    version = "0.1.6";
    # cl-unicode generates lisp source files during compilation.
    #
    # Normally this fails because of an attempt to write to
    # storeDir. A workaround is to run the compilation first with
    # CL_SOURCE_REGISTRY set to pwd, discard the fasls, then use $out
    # of that as the $src of the next compilation
    cl-unicode-build = (build-asdf-system {
      inherit version;
      pname = "cl-unicode-build";

      src =  builtins.fetchTarball {
        url = "https://github.com/edicl/cl-unicode/archive/refs/tags/v0.1.6.tar.gz";
        sha256 = "0ykx2s9lqfl74p1px0ik3l2izd1fc9jd1b4ra68s5x34rvjy0hza";
      };

      systems = [ "cl-unicode" ];

      buildInputs = [
        cl-ppcre
        flexi-streams
      ];
    }).overrideAttrs(o: {
      buildPhase = ''
        mkdir __fasls
        export CL_SOURCE_REGISTRY=$CL_SOURCE_REGISTRY:$(pwd)//
        export ASDF_OUTPUT_TRANSLATIONS="$(pwd):$(pwd)/__fasls:${storeDir}:${storeDir}"
        ${o.lisp} ${o.buildScript}
      '';
      installPhase = ''
        mkdir -pv $out
        rm -rf __fasls
        cp -r * $out
      '';
    });

  in build-asdf-system {
    inherit version;
    pname = "cl-unicode";
    src = cl-unicode-build.out;
    buildInputs = [
      cl-ppcre
      # flexi-streams is only needed for cl-unicode/build
    ];
  };

  "cl-ppcre" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-ppcre/2019-05-21/cl-ppcre-20190521-git.tgz";
      sha256 = "0dwvr29diqzcg5n6jvbk2rnd90i05l7n828hhw99khmqd0kz7xsi";
    };
    version = "20190521";
    pname = "cl-ppcre";
    systems = [
      "cl-ppcre"
    ];
  };

  "cl-ppcre-unicode" = build-asdf-system {
    inherit (cl-ppcre) src version;
    pname = "cl-ppcre-unicode";
    buildInputs = [
      cl-unicode
      cl-ppcre
    ];
    systems = [
      "cl-ppcre-unicode"
    ];
  };

  "cl-reexport" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-reexport/2021-02-28/cl-reexport-20210228-git.tgz";
      sha256 = "02la6z3ickhmh2m87ymm2ijh9nkn7l6slskj99l8a1rhps394qqc";
    };
    version = "20210228";
    pname = "cl-reexport";
    buildInputs = [
      alexandria
    ];
  };

  "cl-selenium" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-selenium-webdriver/2018-03-28/cl-selenium-webdriver-20180328-git.tgz";
      sha256 = "0216vqg1ax5gcqahclii7ifqpc92rbi86rfcf1qn8bdahmfjccbb";
    };
    version = "20180328";
    pname = "cl-selenium";
    buildInputs = [
      alexandria
      cl-json
      dexador
      quri
      split-sequence
    ];
  };

  "cl-utilities" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/cl-utilities/2010-10-06/cl-utilities-1.2.4.tgz";
      sha256 = "1dmbkdr8xm2jw5yx1makqbf1ypqbm0hpkd7zyknxv3cblvz0a87w";
    };
    version = "1.2.4";
    pname = "cl-utilities";
  };

  "dexador" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/dexador/2021-04-11/dexador-20210411-git.tgz";
      sha256 = "0xcbsn29rcvnihkjy2gzykbim0484crvgx62j41g2ps7m8ihp9g1";
    };
    version = "20210411";
    pname = "dexador";
    buildInputs = [
      alexandria
      babel
      bordeaux-threads
      chipz
      chunga
      cl-ssl
      cl-base64
      cl-cookie
      cl-ppcre
      cl-reexport
      fast-http
      fast-io
      quri
      trivial-features
      trivial-gray-streams
      trivial-mimes
      usocket
    ];
  };

  "fast-http" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/fast-http/2019-10-07/fast-http-20191007-git.tgz";
    };
    version = "20191007";
    pname = "fast-http";
    buildInputs = [
      alexandria
      babel
      cl-utilities
      proc-parse
      smart-buffer
      xsubseq
    ];
  };

  "fast-io" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/fast-io/2020-09-25/fast-io-20200925-git.tgz";
      sha256 = "131cqlf84md6kgw2slrpgmksz2j3w1rx4a0cwfrkd8kdvwbh16rd";
    };
    version = "20200925";
    pname = "fast-io";
    buildInputs = [
      alexandria
      static-vectors
      trivial-gray-streams
    ];
  };

  "flexi-streams" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/flexi-streams/2020-09-25/flexi-streams-20200925-git.tgz";
      sha256 = "18r78sb19wwg6zywq3yj202zix2m2ghpm12sa8z8z4vxd7918k6y";
    };
    version = "20200925";
    pname = "flexi-streams";
    buildInputs = [
      trivial-gray-streams
    ];
  };

  "local-time" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/local-time/2021-01-24/local-time-20210124-git.tgz";
      sha256 = "0wld28xx20k0ysgg6akic5lg4vkjd0iyhv86m388xfrv8xh87wii";
    };
    version = "20210124";
    pname = "local-time";
  };

  "proc-parse" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/proc-parse/2019-08-13/proc-parse-20190813-git.tgz";
      sha256 = "07vbj26bfq4ywlcmamsqyac29rsdsa8lamjqx1ycla1bcvgmi4w2";
    };
    version = "20190813";
    pname = "proc-parse";
    buildInputs = [
      alexandria
      babel
    ];
  };

  "quri" = build-asdf-system {
    src = pkgs.stdenv.mkDerivation {
      pname = "patched";
      version = "source";
      src =  builtins.fetchTarball {
        url = "http://beta.quicklisp.org/archive/quri/2021-04-11/quri-20210411-git.tgz";
        sha256 = "1pkvpiwwhx2fcknr7x47h7036ypkg8xzsskqbl5z315ipfmi8s2m";
      };

      # fix build with ABCL
      buildPhase = ''
        sed -i "s,[#][.](asdf.*,#P\"$out/data/effective_tld_names.dat\")," src/etld.lisp
      '';
      installPhase = ''
        mkdir -pv $out
        cp -r * $out
      '';
    };
    version = "20210411";
    pname = "quri";
    buildInputs = [
      alexandria
      babel
      cl-utilities
      split-sequence
    ];
  };

  "smart-buffer" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/smart-buffer/2016-06-28/smart-buffer-20160628-git.tgz";
      sha256 = "0qz1zzxx0wm5ff7gpgsq550a59p0qj594zfmm2rglj97dahj54l7";
    };
    version = "20160628";
    pname = "smart-buffer";
    buildInputs = [
      flexi-streams
      xsubseq
    ];
  };

  "split-sequence" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/split-sequence/2019-05-21/split-sequence-v2.0.0.tgz";
      sha256 = "0jcpnx21hkfwqj5fvp7kc6pn1qcz9hk7g2s5x8h0349x1j2irln0";
    };
    version = "2.0.0";
    pname = "split-sequence";
  };

  "static-vectors" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "https://github.com/sionescu/static-vectors/archive/refs/tags/v1.8.9.tar.gz";
      sha256 = "079qa20lhanzsz1qf4iags91n0ziylbjgbcymm5a5qj7yryas4fw";
    };
    version = "1.8.9";
    pname = "static-vectors";
    buildInputs = [
      alexandria
      cffi-grovel
      cffi
    ];
  };

  "trivial-features" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/trivial-features/2021-04-11/trivial-features-20210411-git.tgz";
      sha256 = "0jsqah1znzqilxnw5vannb083ayk0d7phkackqzwwqkyg5hpn6pq";
    };
    version = "20210411";
    pname = "trivial-features";
  };

  "trivial-garbage" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/trivial-garbage/2020-09-25/trivial-garbage-20200925-git.tgz";
      sha256 = "0kr2nck3n2krsvlcxsxl92n5rh6qs13fnbwr3pw9p5w6yzb4w6pi";
    };
    version = "20200925";
    pname = "trivial-garbage";
  };

  "trivial-gray-streams" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/trivial-gray-streams/2021-01-24/trivial-gray-streams-20210124-git.tgz";
      sha256 = "1hipqwwd5ylskybd173rvlsk7ds4w4nq1cmh9952ivm6dgh7pwzn";
    };
    version = "20210124";
    pname = "trivial-gray-streams";
  };

  "trivial-mimes" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/trivial-mimes/2020-07-15/trivial-mimes-20200715-git.tgz";
      sha256 = "00kcm17q5plpzdj1qwg83ldhxksilgpcdkf3m9azxcdr968xs9di";
    };
    version = "20200715";
    pname = "trivial-mimes";
  };

  "usocket" = build-asdf-system {
    src =  builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/usocket/2019-12-27/usocket-0.8.3.tgz";
      sha256 = "0x746wr2324l6bn7skqzgkzcbj5kd0zp2ck0c8rldrw0rzabg826";
    };
    version = "0.8.3";
    pname = "usocket";
    buildInputs = [
      split-sequence
    ];
  };

  "xsubseq" = build-asdf-system {
    src =   builtins.fetchTarball {
      url = "http://beta.quicklisp.org/archive/xsubseq/2017-08-30/xsubseq-20170830-git.tgz";
      sha256 = "1xz79q0p2mclf3sqjiwf6izdpb6xrsr350bv4mlmdlm6rg5r99px";
    };
    version = "20170830";
    pname = "xsubseq";
    buildInputs = [
    ];
  };

  cl-change-case = build-asdf-system {
    src = builtins.fetchTarball {
      url = "https://github.com/rudolfochrist/cl-change-case/archive/refs/tags/0.2.0.tar.gz";
      sha256 = "0qh2a1igm8dx1q965jkzd3v26s0c2d6kr965hmx7p0s59ix69x4q";
    };
    version = "0.2.0";
    pname = "cl-change-case";
    buildInputs = [
      cl-ppcre-unicode
    ];
  };

  str = build-asdf-system {
    src = builtins.fetchTarball {
      url = "https://github.com/vindarel/cl-str/archive/a833fa23bfaeadffdca46776bb67a562c7bb77d3.tar.gz";
      sha256 = "1cn4qdi7paicgbq6xrz9cxcm9dpvgzf2l1dzbjczzzh3hz5i0xnc";
    };
    version = "0.19.1-a833fa23bf";
    pname = "str";
    buildInputs = [
      cl-ppcre
      cl-change-case
    ];
  };

  closer-mop = build-asdf-system {
    src = builtins.fetchTarball {
      url = "https://github.com/pcostanza/closer-mop/archive/72d6c27f550997fac2492bb5da5097ea33f58771.tar.gz";
      sha256 = "0zj43clfrnp79qr3jrzknr81ml3azmqcsnhbl2zkrs75mxf027vw";
    };
    version = "1.0.0-72d6c27f55";
    pname = "closer-mop";
  };

  jzon = build-asdf-system {
    src = builtins.fetchTarball {
      url = "https://github.com/Zulu-Inuoe/jzon/archive/6b201d4208ac3f9721c461105b282c94139bed29.tar.gz";
      sha256 = "01d4a78pjb1amx5amdb966qwwk9vblysm1li94n3g26mxy5zc2k3";
    };
    version = "0.0.0-20210905-6b201d4208";
    pname = "jzon";
    buildInputs = [
      closer-mop
    ];
    systems = [ "com.inuoe.jzon" ];
  };

  cl-fad = build-asdf-system {
    pname = "cl-fad";
    src = builtins.fetchTarball {
      url = "https://github.com/edicl/cl-fad/archive/refs/tags/v0.7.6.tar.gz";
      sha256 = "1gc8i82v6gks7g0lnm54r4prk2mklidv2flm5fvbr0a7rsys0vpa";
    };
    version = "0.7.6";
    buildInputs = [
      bordeaux-threads
      alexandria
    ];
  };

  rfc2388 = build-asdf-system {
    pname = "rfc2388";
    src = builtins.fetchTarball {
      url = "https://github.com/jdz/rfc2388/archive/591bcf7e77f2c222c43953a80f8c297751dc0c4e.tar.gz";
      sha256 = "0phh5n3clhl9ji8jaxrajidn22d3f0aq87mlbfkkxlnx2pnw694k";
    };
    version = "1.5-591bcf7e77";
  };


  md5 = build-asdf-system {
    pname = "md5";
    src = pkgs.fetchzip {
      url = "https://pmsf.eu/pub/download/md5-2.0.3.zip";
      sha256 = "10sc6226x725sm1w6na1ilbqmm3x8rphqy7m1fhcm9siqnrg8nnj";
    };
    version = "2.0.3";
    buildInputs = [
      flexi-streams
    ];
  };

  trivial-backtrace = build-asdf-system {
    pname = "trivial-backtrace";
    src = pkgs.fetchgit {
      url = "http://common-lisp.net/project/trivial-backtrace/trivial-backtrace.git";
      rev = "43ef7d947f4b4de767d0f91f28b50d9c03ad29d6";
      sha256 = "0jv5iz61i8gvsrll8bawy0j9mrz6f4zw187ni2xvcl2z54adgsfi";
    };
    version = "1.1.0-43ef7d947f";
  };

  hunchentoot = build-asdf-system {
    pname = "hunchentoot";
    version = "1.3.0";
    src = builtins.fetchTarball {
      url = "https://github.com/edicl/hunchentoot/archive/v1.3.0.tar.gz";
      sha256 = "1z0m45lp6rv59g69l44gj3q3d2bmjlhqzpii0vgkniam21dcimy9";
    };
    buildInputs = [
      chunga
      cl-base64
      cl-fad
      cl-ppcre
      flexi-streams
      cl-ssl
      md5
      alexandria
      rfc2388
      trivial-backtrace
      usocket
      bordeaux-threads
    ];
  };

  iterate = build-asdf-system {
    pname = "iterate";
    version = "1.5.3-b12ed59941";
    src = builtins.fetchTarball {
      url = "https://gitlab.common-lisp.net/iterate/iterate/-/archive/b12ed5994137a67e15c46e6fd6f1ffd38d6bac81/iterate-master.tar.gz";
      sha256 = "0v09598pm8frj61qzcsh654ij3l0fmqzfx727z38qnjigg16q5p3";
    };
  };

  cl-sqlite = build-asdf-system {
    pname = "cl-sqlite";
    version = "0.2.1";
    src = builtins.fetchTarball {
      url = "https://github.com/TeMPOraL/cl-sqlite/archive/refs/tags/0.2.1.tar.gz";
      sha256 = "08iv7b4m0hh7qx2cvq4f510nrgdld0vicnvmqsh9w0fgrcgmyg4k";
    };
    buildInputs = [ iterate cffi ];
    systems = [ "sqlite" ];
    nativeLibs = [ pkgs.sqlite ];
  };

  cl-murmurhash = build-asdf-system {
    pname = "cl-murmurhash";
    version = "5433f5e95f";
    src = builtins.fetchTarball {
      url = https://github.com/ruricolist/cl-murmurhash/archive/5433f5e95f1cce63a81259a471150834c6a59364.tar.gz;
      sha256 = "0251r0mpjm0y3qsm4lm7ncvrkxvgwc53spdm1p2mpayhvkkqqsws";
    };
    buildInputs = [ babel ];
  };

}