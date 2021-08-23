[
  (self: super: rec {

    nix = super.nix.overrideAttrs(o:{
      pname = "nix";
      version = "2.3.15";
      src = fetchTarball {
        url = "https://galkowski.xyz/nix-2.3.15.tar.xz";
        sha256 = "0jyw5alb189zcpllp48w54bxwcz62q84n20vprf2kqy1acf62hag";
      };
    });

    vlc = super.vlc.override { jackSupport = true; };

    bcache = super.callPackage ./bcache.nix {};

    buildASDF = import ./save-lisp-and-die-static.nix;

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

    ecl = super.callPackage ./ecl {};
    eclStatic = import ./ecl/static.nix { inherit ecl; };

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
