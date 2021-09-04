{pkgs}:

with pkgs;

pkgs.guile.overrideAttrs(o: rec {
    pname = "guile";
    version = "3.0.7";
    name="${pname}-${version}";

    src = fetchurl {
      url = "mirror://gnu/guile/${pname}-${version}.tar.xz";
      sha256 = "1dwiwsrpm4f96alfnz6wibq378242z4f16vsxgy1n9r00v3qczgm";
    };

    # Could be removed, keeping to avoid rebuild
    postInstall = ''
      wrapProgram $out/bin/guile-snarf --prefix PATH : "${gawk}/bin"
      substituteInPlace "$out/lib/pkgconfig/guile-3.0.pc" \
        --replace "-lunistring" "-L${libunistring}/lib -lunistring" \
        --replace "^Cflags:\(.*\)$" "Cflags: -I${libunistring}/include \1" \
        --replace "-lltdl" "-L${libtool.lib}/lib -lltdl" \
        --replace "includedir=$out" "includedir=$dev"

    '';

    setupHook = ./setup-hook-3.0.sh;

})
