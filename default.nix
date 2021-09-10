let
  nixpkgs = import ./nixpkgs;
  overlays = import ./overlays;
in
{...}: import nixpkgs { inherit overlays; }
