{ config, lib, pkgs, ... }:

with pkgs;

{
  nix.package = nixUnstable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
