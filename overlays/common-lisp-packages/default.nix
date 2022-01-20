{ pkgs, ... }:

import (builtins.fetchTarball {
  url = "https://github.com/uthar/nix-cl/archive/55c5d5af3e63f76e1080ea4a12cf62b80e1eb0a5.tar.gz";
  sha256 = "0w35ams0ibsx8z2x8qqxbnfj5b8mb084hb6nskdh3xazrdhg7j1i";
}) { inherit pkgs; }
