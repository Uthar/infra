{
  description = "Development environment configuration";

  inputs = {
    nixpkgs-21_11.url  = "github:NixOS/nixpkgs/21.11";
    nixos-21_11.url    = "github:NixOS/nixpkgs/nixos-21.11";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    emacs.url          = "github:uthar/nix-emacs";
    clasp.url          = "github:uthar/nix-clasp";
    nix.url            = "nix/2.7.0";
  };

  outputs = {
    self
    , nixpkgs-21_11
    , nixos-21_11
    , nixpkgs-master
    , nixos-hardware
    , emacs
    , nix
    , clasp
  }: {

    devShell.x86_64-linux = let
      pkgs = nixpkgs-21_11.outputs.legacyPackages.x86_64-linux;
    in pkgs.mkShell {
      buildInputs = with pkgs; [ ansible ];
    };

    nixosConfigurations = let

      system = "x86_64-linux";

      nixOverlay = [ (self: super: { nix = nix.defaultPackage.${system}; }) ];
      claspOverlay = [ (self: super: { clasp = clasp.defaultPackage.${system}; }) ];

      defaults = {
        system.configurationRevision = self.rev or "dirty";
        nixpkgs.overlays = import ./overlays/default.nix ++ nixOverlay;
        nix.package = nix.defaultPackage.${system};
        nix.extraOptions = ''
          experimental-features = nix-command flakes
        '';
      };

      workstationDefaults = defaults // {
        environment.systemPackages = [ emacs.defaultPackage.${system} ];
      };

      serverDefaults = defaults // {
      };

    in {

      e6330 = nixpkgs-21_11.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/e6330
          ./systems/pc
          workstationDefaults
        ];
      };

      l15-pix = nixos-21_11.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/l15-pix
          (nixos-hardware + "/lenovo/thinkpad/l14")
          ./systems/pc
          workstationDefaults
          { boot.supportedFilesystems = nixpkgs-21_11.lib.mkForce [ "ext4" ]; }
        ];
      };

      amalgam = nixpkgs-21_11.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./modules
          ./machines/buyvm-lu-512/104.244.74.41
          ./systems/amalgam
          serverDefaults
        ];
      };

    };

  };

}
