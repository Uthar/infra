# FIXME doesnt work yet

{ pkgs, ... }:

with pkgs.lib.strings;

let

  nixops = pkgs.nixops;

  plugins = builtins.tail pkgs.nixopsWithPlugins.plugins;

  evalMachineInfo = import "${nixops}/lib/python3.8/site-packages/nix/eval-machine-info.nix";

  pluginNixExpr = p: "${p}/lib/python3.8/site-packages/${replaceChars ["-"] ["_"] p.pname}/nix";

  pluginNixExprs = map pluginNixExpr (filter (x: x.pname != "nixops-gcp") plugins);

  networkExprs = [
    ./production/nixops.nix
    ./home/nixops.nix
  ];

  # Dont care about this outside of nixops operations
  # i.e. just wanna see the nodes themselves when introspecting in
  # something like nix repl
  fakeArgs = { uuid = ""; deploymentName = ""; args = {}; };

in evalMachineInfo ({ inherit networkExprs pluginNixExprs; } // fakeArgs )
