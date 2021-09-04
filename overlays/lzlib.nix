{ pkgs, stdenv, lib, ... }:

stdenv.mkDerivation {
  pname = "lzlib";
  version = "1.9";
  src = builtins.fetchTarball {
    url = http://download.savannah.gnu.org/releases/lzip/lzlib/lzlib-1.9.tar.gz;
    sha256 = "17imvbs999q0cd8yrglshcgs813s0cwykgblfcwsjrxfy6lk27pr";
  };
  configureFlags = [
    "--enable-shared"
  ];
}
