#!/bin/sh
NIX_PATH="nixpkgs=https://github.com/nixos/nixpkgs/archive/master.tar.gz" nix-env -f '<nixpkgs>' -u
