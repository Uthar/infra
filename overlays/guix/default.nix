
{ pkgs, lib, stdenv, guile, ... }:

with pkgs;

let

  repo = fetchgit {
    url = https://galkowski.xyz/nixos-guix;
    rev = "3727214e6c91c0689ac8bf90ef5140c85050d8a3";
    sha256 = "0rh6j7nzsj1xx6c4s4l016glmdn3sqni0kjn77jgg81qamdz8w26";
  };

  guilePackages = callPackage "${repo}/guile" { inherit guile; };

  guix = callPackage "${repo}/package" { inherit guile guilePackages; };

in guix
