{
  description = "Development environment configuration";

 inputs = {
    nixpkgs-21_11.url  = "github:NixOS/nixpkgs/21.11";
    nixos-21_11.url    = "github:NixOS/nixpkgs/nixos-21.11";
    nixpkgs-master.url = "github:NixOS/nixpkgs/master";
  };

 outputs = {
   self
   ,nixpkgs-21_11
   ,nixos-21_11
   ,nixpkgs-master
 }: {

   nixosConfigurations = {

     e6330-kpg = nixpkgs-21_11.lib.nixosSystem {
       system = "x86_64-linux";
       modules = [
         ./machines/e6330
         ./systems/pc
         {
           # Let 'nixos-version --json' know about the Git revision of this flake.
           system.configurationRevision = nixpkgs-21_11.lib.mkIf (self ? rev) self.rev;
         }
         {
           nixpkgs.overlays = import ./overlays/default.nix;
         }
       ];
     };

   };

 };

}
