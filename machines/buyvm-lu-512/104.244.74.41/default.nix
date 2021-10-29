{ config, pkgs, lib, ... }:

{

  imports = [ ./hardware-configuration.nix ];

  services.journald.extraConfig = "SystemMaxUse=1G";

  boot.loader.grub = { enable = true; version = 2; device = "/dev/vda"; };

  networking.useDHCP = false;

  networking.publicIPv4 = "104.244.74.41";

  networking.interfaces.ens3.ipv4.addresses = [
    { address = config.networking.publicIPv4; prefixLength = 24; }
  ];

  networking.defaultGateway = { address = "104.244.74.1"; interface = "ens3"; };

  networking.nat.externalInterface = "ens3";

  networking.nameservers = [ "8.8.8.8" ];

  services.openssh.enable = true;

  # FIXME no root login
  users.users.root = {
    openssh.authorizedKeys.keyFiles = [ /home/kpg/.ssh/id_rsa.pub ];
  };

}
