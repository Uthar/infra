
let

  domain = "galkowski.xyz";

  layer-2-openvpn =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/layer-2-openvpn ];
    };

  layer-3-openvpn =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/layer-3-openvpn ];
    };

  binary-cache =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/binary-cache ];
    };

  fossil =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/fossil ];
    };

  mail =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/mail ];
    };

  git =
    { config, pkgs, lib, ... }:
    {

      users.users.git.openssh.authorizedKeys.keys = [
        "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKTXPjr06iNRuC0VMRO7hKA9x3NMe5W02g4v/2OujcEe"
      ];

      imports = [ ../../systems/git ];
    };

  murmur =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/murmur ];
    };

  website =
    { config, pkgs, lib, ... }:
    {
      services.httpd = {
        enable = true;
        virtualHosts.${config.networking.domain} = {
          documentRoot = "/srv/git";
        };
      };
    };

  proxy =
    { config, pkgs, lib, nodes, ... }:
    let
      make-vhost = x: lib.attrsets.recursiveUpdate x {
        locations."/.well-known".proxyPass = "!";
        enableACME = true;
        forceSSL = true;
      };
      bcache = config.services.bcache;
      fossil = config.services.fossil;
    in {
      security.acme.acceptTerms = true;
      security.acme.email = "k@${domain}";

      networking.firewall.allowedTCPPorts = [ 80 443 ];

      services.httpd = {
        enable = true;
        adminAddr = "k@${domain}";
        extraConfig = ''
          ServerTokens Prod
          ServerSignature Off
        '';
        virtualHosts."cache.${domain}" = make-vhost {
          locations."/".proxyPass = "unix:${bcache.socketPath}|http://localhost/";
        };
        virtualHosts."fossil.${domain}" = make-vhost {
          locations."/".proxyPass = "http://localhost:${toString fossil.port}/";
        };
        virtualHosts.${domain} = make-vhost {};
      };

    };

  defaults =
    { config, pkgs, lib, ... }:
    {
      nix.autoOptimiseStore = true;
      environment.systemPackages = with pkgs; [ sqlite ranger htop file ];
      networking.domain = domain;
    };

in {

  imports =
    [ defaults
      layer-2-openvpn
      layer-3-openvpn
      murmur
      git
      fossil
      website
      binary-cache
      mail
      proxy
    ];

  services.fail2ban.enable = true;

}
