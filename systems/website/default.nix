
{ config, pkgs, lib, nodes, ... }:

{
  services.httpd = {
    enable = true;
    virtualHosts.${config.networking.hostName} = {
      documentRoot = "/srv/www";
    };
  };
}
