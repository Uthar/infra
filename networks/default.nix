# to inspect the network in nix repl, run something like:
#
#   nets = import ./. {}
#   nets.nodes.<TAB>
#
# currently you can inspect the options and config of each node
# i dont know how to actually build the machines in pure nix, without
# running python

{ pkgs ? import <nixpkgs> {}, ... }:

with pkgs.lib.strings;
with pkgs.lib.lists;

let

  nixops = pkgs.nixops;

  plugins =
    filter (x: x.pname != "nixops-gcp")
      (builtins.tail pkgs.nixopsWithPlugins.plugins);

  evalMachineInfo = import "${nixops}/lib/python3.8/site-packages/nix/eval-machine-info.nix";

  pluginNixExpr = p: "${p}/lib/python3.8/site-packages/${replaceChars ["-"] ["_"] p.pname}/nix";

  pluginNixExprs = map pluginNixExpr plugins;

  networkExprs = [
    ./production/nixops.nix
    ./home/nixops.nix
  ];

  # Dont care about this outside of nixops operations
  # i.e. just wanna see the nodes themselves when introspecting in
  # something like nix repl
  fakeArgs = { uuid = ""; deploymentName = ""; args = {}; };

in evalMachineInfo ({ inherit networkExprs pluginNixExprs; } // fakeArgs )
