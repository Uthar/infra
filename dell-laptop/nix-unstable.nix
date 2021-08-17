{ config, lib, pkgs, ... }:

# not necessary for gzipped binary cache since nix is overlayed now in
# all-overlays.nix with a patch adding gzip decompression

with pkgs;

{
  nix.package = nixUnstable;
  nix.extraOptions = ''
    experimental-features = nix-command flakes
  '';
}
