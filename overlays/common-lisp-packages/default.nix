# TODO:
# - faster build by using lisp with preloaded asdf?
# - dont include java libs unless abcl?
# - dont use build-asdf-system to build lispWithPackages?
# - make the lisp packages overridable? (e.g. buildInputs glibc->musl)
# - build asdf with nix and use that instead of one shipped with impls
#   (e.g. to fix build with clisp - does anyone use clisp?)
# - claspPackages ? (gotta package clasp with nix first)
# - hard one: remove unrelated sources ( of systems not being built)
# - figure out a less awkward way to patch sources
#   (have to build from src directly for SLIME to work, so can't just patch sources in place)

{ pkgs, lib, stdenv, ... }:

with lib.lists;
with lib.strings;

let

  # Returns a flattened dependency tree without duplicates
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

      # Java libraries for ABCL, will be appended to the class path
      javaLibs ? [],

      # Lisp dependencies
      buildInputs ? [],

      # Lisp command to run buildScript
      lisp,

      # Some libraries have multiple systems under one project, for
      # example, cffi has cffi-grovel, cffi-toolchain etc.  By
      # default, only the `pname` system is build.
      #
      # .asd's not listed in `systems` are removed in
      # installPhase. This prevents asdf from referring to uncompiled
      # systems on run time.
      #
      # Also useful when the pname is differrent than the system name,
      # such as when using reverse domain naming.
      systems ? [ pname ],

      # Other args to mkDerivation
      ...
    } @ args:

    stdenv.mkDerivation (rec {
      inherit pname version src nativeLibs javaLibs buildInputs lisp systems;

      # When src is null, we are building a lispWithPackages and only
      # want to make use of the dependency environment variables
      # generated by build-asdf-system
      dontUnpack = src == null;

      # Tell asdf where to find system definitions of lisp dependencies.
      #
      # The "//" ending is important as it makes asdf recurse into
      # subdirectories when searching for .asd's. This is to support
      # projects where .asd's aren't in the root directory.
      CL_SOURCE_REGISTRY = makeSearchPath "/" (flattenedDeps buildInputs);

      # Tell lisp where to find native dependencies
      LD_LIBRARY_PATH = makeLibraryPath (concatMap (x: x.nativeLibs) (flattenedDeps buildInputs));

      # Java libraries For ABCL
      CLASSPATH = makeSearchPath "share/java/*" (concatMap (x: x.javaLibs) (flattenedDeps buildInputs));

      # Portable script to build the systems.
      #
      # `lisp` must evaluate this file then exit immediately. For
      # example, SBCL's --script flag does just that.
      buildScript = pkgs.writeText "build-${pname}.lisp" ''
        (require :asdf)
        (dolist (s '(${concatStringsSep " " systems}))
          (asdf:compile-system s))
      '';

      buildPhase = optionalString (src != null) ''
        # In addition to lisp dependencies, make asdf see the .asd's
        # of the systems being built
        #
        # *Append* src since `buildInputs` can provide .asd's that are
        # also in `src` but are not in `systems` (that is, the .asd's
        # that will be deleted in installPhase). We don't want to
        # rebuild them, but to load them from buildInputs.
        #
        # NOTE: It's important to read files from `src` instead of
        # from pwd to get go-to-definition working with SLIME
        export CL_SOURCE_REGISTRY=$CL_SOURCE_REGISTRY:${src}//

        # Similiarily for native deps
        export LD_LIBRARY_PATH=${makeLibraryPath nativeLibs}:$LD_LIBRARY_PATH
        export CLASSPATH=${makeSearchPath "share/java/*" javaLibs}:$CLASSPATH

        # Make asdf compile from `src` to pwd and load `buildInputs`
        # from storeDir. Otherwise it could try to recompile lisp deps.
        export ASDF_OUTPUT_TRANSLATIONS="${src}:$(pwd):${storeDir}:${storeDir}"

        # Finally, compile the systems
        ${lisp} ${buildScript}
      '';

      # Copy compiled files to store
      installPhase = ''
        mkdir -pv $out
        cp -r * $out

        # Remove all .asd files except for those in `systems`.
        find $out -name "*.asd" \
        | grep -v "${escapeRegex (concatMapStringsSep "|" (x: escapeNixString (x+".asd")) systems)}"\
        | xargs rm -fv || true
      '';

      # Not sure if it's needed, but caused problems with SBCL
      # save-lisp-and-die binaries in the past
      dontStrip = true;
      dontFixup = true;

    };


  # Build the set of lisp packages using `lisp`
  commonLispPackagesFor = lisp:
    let
      build-asdf-system' = body: build-asdf-system (body // { inherit lisp; });
    in import ./packages.nix {
      inherit pkgs;
      build-asdf-system = build-asdf-system';
    };

  # Creates a lisp wrapper with `packages` installed
  #
  # `packages` is a function that takes `clpkgs` - a set of lisp
  # packages - as argument and returns the list of packages to be
  # installed
  #
  # Example:
  #
  # sbclPackages = commonLispPackagesFor sbcl;
  # sbclWithPackages = lispWithPackages sbclPackages;
  # sbclWithPackages (clpkgs: with clpkgs; [ alexandria cffi str ]);
  lispWithPackages = clpkgs: packages:
    # FIXME just use flattenedDeps instead
    (build-asdf-system rec {
      lisp = (head (lib.attrValues clpkgs)).lisp;
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
          --prefix LD_LIBRARY_PATH : "${makeLibraryPath o.nativeLibs}" \
          --prefix CLASSPATH : "${o.CLASSPATH}" \
          --prefix CLASSPATH : "${makeSearchPath "share/java/*" o.javaLibs}"
      '';
    });


  commonLispPackages = rec {
    inherit commonLispPackagesFor build-asdf-system lispWithPackages;

    sbcl = "${pkgs.sbcl}/bin/sbcl --script";
    ecl = "${pkgs.ecl}/bin/ecl --shell";
    abcl = ''${pkgs.abcl}/bin/abcl --batch --eval "(load \"$buildScript\")"'';
    ccl = ''${pkgs.ccl}/bin/ccl --batch --eval "(load \"$buildScript\")" --'';

    sbclPackages = commonLispPackagesFor sbcl;
    eclPackages = commonLispPackagesFor ecl;
    abclPackages = commonLispPackagesFor abcl;
    cclPackages = commonLispPackagesFor ccl;

    sbclWithPackages = lispWithPackages sbclPackages;
    eclWithPackages = lispWithPackages eclPackages;
    abclWithPackages = lispWithPackages abclPackages;
    cclWithPackages = lispWithPackages cclPackages;
  };

in commonLispPackages
