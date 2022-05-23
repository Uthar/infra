  (self: super: rec {

    vlc = super.vlc.override { jackSupport = true; };

    bcache = super.callPackage ./bcache.nix {};

    buildASDF = import ./save-lisp-and-die-static.nix;

    sqlite338 = super.sqlite.overrideAttrs (o: {
      version = "3.38.2";
      src = super.fetchurl {
        url = "https://sqlite.org/2022/sqlite-autoconf-3380200.tar.gz";
        sha256 = "sha256-55dKoUMLrWkKXp95pu5chJKtqCadxnWHWtD7dH18raQ=";
      };
    });

    fossil = let
      rev = "7c2bb7a2b05e504e8a4d9115f79585c156f447328c0f5a6fd49330af5dd050ad";
    in
      with super.lib.lists;
      super.fossil.overrideAttrs (old: rec {
        pname = "fossil";
        version = "2.19";
        configureFlags = remove "--disable-internal-sqlite" old.configureFlags;
        buildInputs = remove super.sqlite old.buildInputs;
        src = super.fetchurl {
          url = "https://fossil.galkowski.xyz/${pname}/tarball/${rev}/${pname}-${rev}.tar.gz";
          sha256 = "sha256-a5ADrgpQHPw0pBhaxm1RYJt5bqtMs0fbVu9bSUzi6w4=";
        };
        doCheck = false;
      });

    abcl = (super.abcl.override {
      jdk = super.jdk17;
      jre = super.jdk17;
    }).overrideAttrs (o: {
      # Pull in fix for https://github.com/armedbear/abcl/issues/473
      version = "1.8.1-dev";
      src = super.fetchFromGitHub {
        repo = "abcl";
        owner = "uthar";
        rev = "aba4f90ca75935886512dba79dd3565bf58cac76";
        hash = "sha256-k1apwbsGlKjbrRfhbSSqlM4xxxzvW0PB3DvuiTqEy+U=";
      };
      # Fix for https://github.com/armedbear/abcl/issues/484
      installPhase = super.lib.replaceStrings
        ["${super.jdk17}/bin/java"]
        ["${super.jdk17}/bin/java --add-opens=java.base/java.util.jar=ALL-UNNAMED"]
        o.installPhase;
    });

    sbcl = import ./sbcl.nix { inherit super; };
    sbcl-static = super.callPackage ./sbcl-static.nix {};

    eclGlibc = import ./ecl { pkgs = super; };
    eclMusl = import ./ecl { pkgs = super.pkgsMusl; };
    eclGlibcStatic = import ./ecl/static.nix { ecl = eclGlibc; };
    eclMuslStatic = import ./ecl/static.nix { ecl = eclMusl; };
    ecl = eclGlibc;

    kawa = super.callPackage ./kawa {};

    lzlib = super.callPackage ./lzlib.nix {} ;

    ode = super.ode.overrideAttrs (o: { 
      configureFlags = [ "--enable-shared" "--enable-double-precision"]; 
    });

    drawstuff = ode.overrideAttrs (o: {
      buildInputs = with super; [ xorg.libX11 libGL libGLU glew ];
      nativeBuildInputs = with super; [ pkg-config xorg.libX11 libGL libGLU glew ];
    });

    guile_3_0 = super.callPackage ./guile {} ;

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
