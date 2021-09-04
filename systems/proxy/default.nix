{ config, pkgs, lib, nodes, ... }:

let
  domain = config.networking.domain;
  # website = nodes.website.config.networking.hostName;
  website = "website";
  # binary-cache = nodes.binary-cache.config.networking.hostName;
  binary-cache = "cache";
  # fossil = nodes.fossil.config.networking.hostName;
  fossil = "fossil";
  make-vhost = if config.security.acme.acceptTerms
               then (x: lib.attrsets.recursiveUpdate x {
                 locations."/.well-known".proxyPass = "!";
                 enableACME = true;
                 forceSSL = true;
               })
               else (x: x);

in
{

  networking.firewall.allowedTCPPorts = [ 80 443 ];

  services.httpd = {
    enable = true;
    extraConfig = ''
      ServerTokens Prod
      ServerSignature Off
    '';
    virtualHosts."${binary-cache}.${domain}" = make-vhost {
      locations."/".proxyPass = "http://${binary-cache}/";
    };
    virtualHosts."${fossil}.${domain}" = make-vhost {
      # locations."/".proxyPass = "http://${fossil}:${toString nodes.fossil.config.services.fossil.port}/";
      locations."/".proxyPass = "http://${fossil}:${toString config.services.fossil.port}/";
    };
    virtualHosts.${domain} = make-vhost {
      locations."/".proxyPass = "http://${website}/";
    };
  };
}
