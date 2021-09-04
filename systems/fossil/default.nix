{ config, pkgs, lib, nodes, ... }:


{
  services.fossil = {
    enable = true;
    baseurl = "https://${config.networking.fqdn}";
  };

  environment.systemPackages = [ pkgs.fossil ];
}
