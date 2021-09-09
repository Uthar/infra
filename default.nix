let
  nixpkgs = import ./nixpkgs;
  overlays = import ./overlays/default.nix;
in
{...}: import nixpkgs { inherit overlays; }
