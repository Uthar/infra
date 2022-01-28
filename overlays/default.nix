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
              rev = "eff444990351b462e4ba22a1d2d8ffd241cd2e2c";
              sha256 = "0lvamz2y8hbx653slbscq85qgfqi49r1jsvx3fqh4ql5cz5kkz5m";
            };
            version = "20210901-eff4449903";
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
              rev = "67f405dcee8754c5e4d7d371f4362cec422b83cb";
              sha256 = "19ajpy25vyxqlgvv71s796x94jqk8b6sw1iwmrqh03qbf7534zim";
            };
            version = "20210917-67f405dcee";
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
      rev = "f48180f2ff3169651a725396d4f7d667c99a92873b9c3df7eee2f144be7a0721";
    in
      with super.lib.lists;
      super.fossil.overrideAttrs (old: rec {
        pname = "fossil";
        version = "2.17";
        configureFlags = remove "--disable-internal-sqlite" old.configureFlags;
        buildInputs = remove super.sqlite old.buildInputs;
        src = super.fetchurl {
          url = "https://fossil-scm.org/home/tarball/${rev}/${pname}-${rev}.tar.gz";
          sha256 = "sha256:1gjmc2fy2mxhgy7bqnixgp4pfq7wdg04llrsy0qi1bc6cap8zdjs";
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

    rofi-passmenu = super.writeShellScriptBin "passmenu" "
      prefix=$\{PASSWORD_STORE_DIR-~/.password-store}
      password_files=($(${super.fd}/bin/fd .gpg$ $prefix))
      password_files=( \"$\{password_files[@]#\"$prefix\"/}\" )
      password_files=( \"$\{password_files[@]%.gpg}\" )

      password=$(printf '%s\\n' \"$\{password_files[@]}\" | ${super.rofi}/bin/rofi -dmenu -p pass)

      [[ -n $password ]] || exit

      ${super.pass}/bin/pass show -c \"$password\" 2>/dev/null
    ";

  })
]
