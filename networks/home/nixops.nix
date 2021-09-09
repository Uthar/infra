{

  network.description = "home network";
  network.storage.legacy = {};
  network.enableRollback = true;

  defaults =
    { config, pkgs, lib, ... }:
    { nix.autoOptimiseStore = true;
      environment.systemPackages = with pkgs; [ sqlite ranger htop file ];
      deployment.provisionSSHKey = false;
      deployment.hasFastConnection = true;

      # Dirty hack to make the same nixpkgs used for *building* the
      # systems available to nix cli tools *in* those systems
      environment.variables.NIX_PATH = lib.mkForce "nixpkgs=/run/current-system/current-nixpkgs";
      system.extraSystemBuilderCmds = ''
        ln -s ${../../.} $out/current-nixpkgs
      '';
    };

  main-pc =
    { config, pkgs, lib, nodes, ... }:
    { deployment.targetHost = "localhost";

      imports =
        [ ../../machines/e6330
          ../../systems/pc
        ];

    };

}
