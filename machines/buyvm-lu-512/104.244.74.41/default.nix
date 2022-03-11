{ config, pkgs, lib, ... }:

{

  imports = [ ./hardware-configuration.nix ];

  services.journald.extraConfig = "SystemMaxUse=1G";

  boot.loader.grub = { enable = true; version = 2; device = "/dev/vda"; };

  networking.useDHCP = false;

  networking.interfaces.ens3.ipv4.addresses = [
    { address =  "104.244.74.41"; prefixLength = 24; }
  ];

  networking.defaultGateway = { address = "104.244.74.1"; interface = "ens3"; };

  networking.nat.externalInterface = "ens3";

  networking.nameservers = [ "8.8.8.8" ];

  services.openssh.enable = true;

  # FIXME no root login
  users.users.root = {
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKTXPjr06iNRuC0VMRO7hKA9x3NMe5W02g4v/2OujcEe"
    ];
  };

}
