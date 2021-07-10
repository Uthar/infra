{ pkgs, lib, fossil, expect }:

pkgs.stdenv.mkDerivation rec {

  name = "fsl";

  src = pkgs.fetchfossil {
    url = "http://fossil.0branch.com/${name}";
    rev = "06a31756cc58f2c991c35f4bae3e918f8b6ea624";
    sha256 = "15ma3rf34aj4malvdb0pyhxhhkx8f1zfdi7v11djlanpfxs00914";
  };

  nativeBuildInputs = [ pkgs.makeWrapper ];

  installPhase = ''
    mkdir -p $out/bin
    cp $src/fsl $out/bin
    wrapProgram $out/bin/fsl \
      --prefix PATH : ${expect}/bin \
      --prefix PATH : ${fossil}/bin

    mkdir -p $out/share/bash-completion/completions
    substitute \
      ${fossil}/share/bash-completion/completions/fossil.bash \
      $out/share/bash-completion/completions/fsl.bash \
      --replace fossil fsl
  '';

  meta = with lib; {
    description = "A Fossil wrapper";
    homepage    = "http://fossil.0branch.com/fsl/";
    license     = licenses.isc;
    maintainers = with maintainers; [ uthar ];
    platforms   = platforms.all;

    longDescription = ''
      This project provides a script (fsl) written in Tcl/Expect with
      the following features:

      - Command aliasing and pre-processing
      - Output filtering
      - A number of preconfigured aliases, filters for colouring output

      Though still inchoate, you should be able to use it as you would
      fossil on the command line.
    '';
  };

}
