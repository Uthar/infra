{ config, pkgs, lib, nodes, ... }:


{
  services.fossil = {
    enable = true;
    baseurl = "https://fossil.${config.networking.domain}";
  };

  environment.systemPackages = [ pkgs.fossil ];
}
