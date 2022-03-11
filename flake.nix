{
  description = "Development environment configuration";

  inputs = {
    nixpkgs-21_11.url  = "github:NixOS/nixpkgs/21.11";
    nixos-21_11.url    = "github:NixOS/nixpkgs/nixos-21.11";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
    nixos-hardware.url = "github:NixOS/nixos-hardware/master";
    emacs.url          = "github:uthar/nix-emacs";
  };

  outputs = {
    self
    , nixpkgs-21_11
    , nixos-21_11
    , nixpkgs-master
    , nixos-hardware
    , emacs
  }: {

    nixosConfigurations = let

      defaultsModule = {
        system.configurationRevision = self.rev;
        nixpkgs.overlays = import ./overlays/default.nix;
      };

      pcModule = import ./systems/pc { emacs = emacs.defaultPackage.x86_64-linux; };

    in {

      e6330-kpg = nixpkgs-21_11.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/e6330
          pcModule
          defaultsModule
        ];
      };

      l15-pix = nixos-21_11.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./machines/l15-pix
          (nixos-hardware + "/lenovo/thinkpad/l14")
          pcModule
          defaultsModule
          { boot.supportedFilesystems = nixpkgs-21_11.lib.mkForce [ "ext4" ]; }
        ];
      };

      amalgam = nixpkgs-21_11.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [
          ./modules
          ./machines/buyvm-lu-512/104.244.74.41
          ./systems/amalgam
          defaultsModule
        ];
      };

    };

  };

}
