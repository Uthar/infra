{pkgs, lib, stdenv }:

with pkgs;

stdenv.mkDerivation {
  pname = "kawa";
  version = "3.1.1";
  src = pkgs.fetchurl {
    url = mirror://gnu/kawa/kawa-3.1.1.tar.gz;
    sha256 = "06g015zjlfgsx0n4lb326czkbf1grlx0n6dx074m808hdg6m16lc";
  };
  buildInputs = [ jdk8 makeWrapper ];
  postInstall = ''
    wrapProgram $out/bin/kawa \
      --prefix PATH : ${jdk8}/bin
  '';
}
