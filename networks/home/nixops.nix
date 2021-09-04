{

  network.description = "home network";
  network.storage.legacy = {};
  network.enableRollback = true;

  defaults = let
    nixpkgs = import ../../nixpkgs;
    overlays = ../../overlays;
  in
    { config, pkgs, lib, ... }:
    { nix.autoOptimiseStore = true;
      environment.systemPackages = with pkgs; [ sqlite ranger htop file ];
      deployment.provisionSSHKey = false;
      deployment.hasFastConnection = true;

      # Dirty hack to make the same nixpkgs used for building the
      # systems available to nix cli tools
      environment.variables.NIX_PATH = lib.mkForce
        (lib.strings.concatStringsSep ":" [
          "nixpkgs=/run/current-system/current-nixpkgs"
          "nixpkgs-overlays=/run/current-system/current-overlays/default.nix"
        ]);

      system.extraSystemBuilderCmds = ''
          ln -s ${nixpkgs} $out/current-nixpkgs
          ln -s ${overlays} $out/current-overlays
          echo "nixops deployed" >> $out/configuration-name
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
