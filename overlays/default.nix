[
  (self: super: rec {

    nix = super.nixStable.overrideAttrs(o:{
      patches =
        o.patches
        ++ [ ./add-gzip-decompression.patch
             ./add-zstd-decompression.patch
           ];
      buildInputs = o.buildInputs ++ [ super.zstd ];
      doCheck = false;
      doInstallCheck = false;
    });

    nixopsWithPlugins = super.nixopsUnstable.override {
      overrides = (self: super: {
        nixops = super.nixops.overridePythonAttrs (o:{
            src = super.pkgs.fetchgit {
              url = "https://galkowski.xyz/nixops";
              rev = "97e30efa54e75d4b5028f31d2f75ce39a72fa534";
              sha256 = "0gbxcz6kfkhqnbh2havsxmfmjzp2sbj5wa9rzq38s6h49n7l11vr";
            };
            version = "20210901-97e30efa54";
          });
        nixopsvbox = super.nixopsvbox.overridePythonAttrs (o:{
            src = super.pkgs.fetchgit {
              url = "https://galkowski.xyz/nixops-vbox";
              rev = "5c1cd81de9568f37a9302b3b9a2314255419961e";
              sha256 = "0a8lpcmadbpgpnqc8ksx7ds9dljgkr52bv0y0ghfrfhlsic0dmlv";
            };
            version = "20210823-5c1cd81de9";
        });
        nixops-aws = super.nixops-aws.overridePythonAttrs (o:{
            src = super.pkgs.fetchgit {
              url = "https://galkowski.xyz/nixops-aws";
              rev = "371aedeb7fd53b8978a60dd7c37d3a6c38101c48";
              sha256 = "15jz9x3ra3hsh6xj4cbri1fvvjk2rplnnhnccz7qc6f176b5r01j";
            };
            version = "20210902-371aedeb7f";
        });
      });
    };

    nixops = builtins.head nixopsWithPlugins.plugins;

    vlc = super.vlc.override { jackSupport = true; };

    bcache = super.callPackage ./bcache.nix {};

    buildASDF = import ./save-lisp-and-die-static.nix;

    commonLispPackages = super.callPackage ./common-lisp-packages {};
    inherit (commonLispPackages)
      commonLispPackagesFor
      sbclPackages
      eclPackages
      abclPackages
      cclPackages
      claspPackages
      lispWithPackages
      sbclWithPackages
      eclWithPackages
      abclWithPackages
      cclWithPackages
      claspWithPackages;


    fsl = super.callPackage ./fsl.nix { inherit fossil; };

    fossil = let
      rev = "e3066edea3d0cef2cff8baaddf2cb7008c3367e27cab209dddec4db99eb692fc";
    in
      with super.lib.lists;
      super.fossil.overrideAttrs (old: rec {
        pname = "fossil";
        version = "2.17";
        configureFlags = remove "--disable-internal-sqlite" old.configureFlags;
        buildInputs = remove super.sqlite old.buildInputs;
        src = super.fetchurl {
          url = "https://fossil-scm.org/home/tarball/${rev}/${pname}-${rev}.tar.gz";
          sha256 = "sha256:0kpk80vdp87myvzky1idrns1rvjawvnn8wc0vkgmpkp1ympxgzk0";
        };
        doCheck = false;
      });

    sbcl = import ./sbcl.nix { inherit super; };
    sbcl-static = super.callPackage ./sbcl-static.nix {};

    eclGlibc = import ./ecl { pkgs = super; };
    eclMusl = import ./ecl { pkgs = super.pkgsMusl; };
    eclGlibcStatic = import ./ecl/static.nix { ecl = eclGlibc; };
    eclMuslStatic = import ./ecl/static.nix { ecl = eclMusl; };
    ecl = eclGlibc;

    kawa = super.callPackage ./kawa {};

    clasp = super.callPackage ./clasp {};

    lzlib = super.callPackage ./lzlib.nix {} ;

    guile_3_0 = super.callPackage ./guile {} ;

    guix = super.callPackage ./guix { guile = guile_3_0; } ;

    udiskie = super.udiskie.overridePythonAttrs (old: rec {
      version = "2.3.3";
      src = super.fetchFromGitHub {
        owner = "coldfix";
        repo = "udiskie";
        rev = "v${version}";
        sha256 = "08y3swazx0w4dagjhzzi7kzcdpz02xnca1jm7nic2xz3prq41qrr";
      };
    });

    tabbed = super.tabbed.overrideAttrs (old: rec {
      src = super.fetchFromGitHub {
        owner = "uthar";
        repo = "tabbed";
        rev = "60fd4a0c6afe9f627923cc60c4476ac1a7c2eb7f";
        sha256 = "0i0f55zg3m4m3iv99i8grykg6i0xq7yimiw73xddd9j5p1bnh108";
      };
    });

    libfm = super.libfm.overrideAttrs (old: rec {
      version = "1.3.2";
      src = super.fetchurl {
        url = "mirror://sourceforge/pcmanfm/libfm-${version}.tar.xz";
        sha256 = "1rfira3lx8v6scz1aq69925j4vslpp36bmgrrzcfby2c60q2c155";
      };
    });

    pcmanfm = super.pcmanfm.overrideAttrs (old: rec {
      name = "pcmanfm-1.3.2";
      src = super.fetchurl {
        url = "mirror://sourceforge/pcmanfm/${name}.tar.xz";
        sha256 = "1xqc2k2jh165mm81xg0ghxx0ml1s3rhh4ndvbzkcri4kfhj7pjql";
      };
    });

    togglemonitor = super.writeShellScriptBin "togglemonitor" ''
    if ! [ -f /tmp/togglemonitor ]; then
        echo "" > /tmp/togglemonitor
    fi
    toggle=`cat /tmp/togglemonitor`
    if [ $toggle ]; then
        ${super.xorg.xrandr}/bin/xrandr --output VGA1 --auto --above LVDS1
        echo "" > /tmp/togglemonitor
    else
        ${super.xorg.xrandr}/bin/xrandr --output VGA1 --off
        echo 1 > /tmp/togglemonitor
    fi
    '';

  })
]
