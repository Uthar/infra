
{ pkgs, lib, stdenv, ... }:

with lib.lists;
with lib.strings;

let

  flattenedDeps = buildInputs:
    let
      walk = acc: node:
        if length node.buildInputs == 0
        then acc
        else foldl walk (acc ++ node.buildInputs) node.buildInputs;
    in unique (walk [] { inherit buildInputs; });

  #
  # Wrapper around stdenv.mkDerivation for building ASDF systems.
  #
  build-asdf-system =
    { pname,
      version,
      src ? null,

      # Native libraries, will be appended to the library path
      nativeLibs ? [],

      # Lisp dependencies
      buildInputs ? [],

      # Lisp command to run build script
      lisp,

      # Some libraries have multiple systems under one project, for
      # example, cffi has cffi-grovel, cffi-toolchain etc.  By
      # default, only the `pname` system is build.
      #
      # This prevents asdf referring to uncompiled systems on run time.
      #
      # Also useful when the pname is differrent than the system name,
      # such as when using reverse domain naming.
      systems ? [ pname ],

      # Other args to mkDerivation
      ...
    } @ args:

    stdenv.mkDerivation rec {
      inherit pname version src nativeLibs buildInputs lisp systems;

      dontUnpack = src == null;

      # Tell asdf where to find system definitions of lisp dependencies.
      #
      # The "/" is important as it makes asdf recurse into
      # subdirectories when searching for .asd's. This is to support
      # projects where .asd's aren't in the root directory.
      CL_SOURCE_REGISTRY = makeSearchPath "/" (flattenedDeps buildInputs);

      # Tell lisp where to find native dependencies
      LD_LIBRARY_PATH = makeLibraryPath (concatMap (x: x.nativeLibs) (flattenedDeps buildInputs));

      # FIXME for abcl
      CLASSPATH = makeSearchPath "" (flattenedDeps buildInputs);

      # Portable script to build the systems.
      #
      # `lisp` must evaluate this file then exit immediately. For
      # example, SBCL's --script flag does that.
      buildScript = pkgs.writeText "build-${pname}.lisp" ''
        (require :asdf)
        (dolist (s '(${concatStringsSep " " systems}))
          (asdf:compile-system s))
      '';

      buildPhase = ''
        # In addition to lisp dependencies, make asdf see the .asd's
        # of the systems being built
        export CL_SOURCE_REGISTRY=$(pwd)//:$CL_SOURCE_REGISTRY

        # Similiarily for native deps
        export LD_LIBRARY_PATH=${makeLibraryPath nativeLibs}:$LD_LIBRARY_PATH

        # Make asdf save newly compiled files to the nix build
        # directory. Already compiled code of the lisp dependencies
        # should be read from the nix store.
        export ASDF_OUTPUT_TRANSLATIONS="$(pwd):$(pwd):${storeDir}:${storeDir}"

        # Remove all .asd files except for those in `systems`.
        find . -name "*.asd" \
        | grep -v "${escapeRegex (concatMapStringsSep "|" (x: escapeNixString (x+".asd")) systems)}"\
        | xargs rm -fv || true

        # Finally, compile the systems
        ${lisp} ${buildScript}
      '';

      # Copy compiled files to store
      installPhase = ''
        mkdir -pv $out
        cp -r * $out
      '';

    };


  commonLispPackagesFor = lisp:
  let
    build-asdf-system' = body: build-asdf-system (body // { inherit lisp; });
  in { inherit lisp; }
     //
     (import ./packages.nix {
       inherit pkgs;
       build-asdf-system = build-asdf-system';
     });

  # Creates a lisp wrapper with `packages` installed
  #
  # `packages` is a function that takes `clpkgs` as argument and
  # returns the list of packages to be installed
  #
  # Example:
  #
  # sbclPackages = commonLispPackagesFor sbcl;
  # sbclWithPackages = lispWithPackages sbclPackages;
  # sbclWithPackages (clpkgs: with clpkgs; [ alexandria cffi str ]);
  #
  lispWithPackages = clpkgs: packages:

    # FIXME just use flattenedDeps instead
    (build-asdf-system rec {
      inherit (clpkgs) lisp;
      pname = baseNameOf (head (split " " lisp));
      version = "with-packages";
      buildInputs = packages clpkgs;
      systems = [];
    }).overrideAttrs(o: {
      installPhase = ''
        mkdir -pv $out/bin
        source ${pkgs.dieHook}/nix-support/setup-hook
        source ${pkgs.makeWrapper}/nix-support/setup-hook
        makeWrapper \
          ${head (split " " o.lisp)} \
          $out/bin/${baseNameOf (head (split " " o.lisp))} \
          --prefix CL_SOURCE_REGISTRY : "${o.CL_SOURCE_REGISTRY}" \
          --prefix ASDF_OUTPUT_TRANSLATIONS : ${storeDir}:${storeDir} \
          --prefix LD_LIBRARY_PATH : "${o.LD_LIBRARY_PATH}" \
          --prefix LD_LIBRARY_PATH : "${makeLibraryPath o.nativeLibs}"
      '';
    });


  commonLispPackages = rec {
    inherit commonLispPackagesFor build-asdf-system lispWithPackages;

    sbclCmd = "${pkgs.sbcl}/bin/sbcl --script";
    ecCmd = "${pkgs.ecl}/bin/ecl --shell";
    abcCmd = ''${pkgs.abcl}/bin/abcl --batch --eval "(load \"$buildScript\")"'';

    sbclPackages = commonLispPackagesFor sbclCmd;
    eclPackages = commonLispPackagesFor eclCmd;
    abclPackages = commonLispPackagesFor abclCmd;

    sbclWithPackages = lispWithPackages sbclPackages;
    eclWithPackages = lispWithPackages eclPackages;
    abclWithPackages = lispWithPackages abclPackages;
  };

in commonLispPackages
