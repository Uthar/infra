{ config, pkgs, lib, nodes, ... }:


{
  services.fossil = {
    enable = true;
    baseurl = "https://fossil.${config.networking.domain}";
  };

  systemd.services.fossil.path = [
    "/run/wrappers"
  ];

  environment.systemPackages = [ pkgs.fossil ];
}
