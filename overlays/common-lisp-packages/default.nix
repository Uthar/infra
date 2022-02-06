{ pkgs, ... }:

import (builtins.fetchTarball {
  url = "https://github.com/uthar/nix-cl/archive/c6c087de2802812bc9d5baa4c72955009050c1aa.tar.gz";
  sha256 = "19hyhg55jj3w6d71l9c5xp4gik5ykjqbdbzd0y1f5xhbv68ysb0b";
}) { inherit pkgs; }
