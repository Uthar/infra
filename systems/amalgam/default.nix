
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
      users.users.nix.openssh.authorizedKeys.keys = [
        "command=\"nix-store --serve --write\",no-port-forwarding,no-x11-forwarding,no-agent-forwarding ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHCPTLr1g7bOKSj0tCFbVz0VVY3mOl6mXOTuHaRFVGad"
      ];
      users.groups.ci = {};
      users.users.nix.group = "ci";
      users.users.nix.useDefaultShell = true;
      users.users.nix.isSystemUser = true;
      nix.trustedUsers = [ "nix" "root" ];
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

      systemd.tmpfiles.rules = [
        "d /run/mailserverKeys 0750 dovecot2 dovecot2"
        "z /run/mailserverKeys 0750 dovecot2 dovecot2"
      ];

      boot.specialFileSystems = {
        "/run/mailserverKeys" = { fsType = "ramfs"; options = [ "nosuid" "nodev" "mode=750" ]; };
      };

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
        extraModules = [ "deflate" "filter" ];
        extraConfig = ''
          ServerTokens Prod
          ServerSignature Off
          <IfModule mod_deflate.c>
            SetOutputFilter DEFLATE
            SetEnvIfNoCase Request_URI "\.(?:gif|jpe?g|png|gz|tgz|ogg|mkv|webm|bz2|xz|zip)$" no-gzip
          </IfModule>
        '';
        virtualHosts."cache.${domain}" = make-vhost {
          locations."/".proxyPass = "unix:${bcache.socketPath}|http://localhost/";
        };
        virtualHosts."fossil.${domain}" = make-vhost {
          locations."/".proxyPass = "http://localhost:${toString fossil.port}/";
        };
        virtualHosts."blog.${domain}" = make-vhost {
          documentRoot = "/srv/blog";
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
  services.openssh.banner = ''
    ;; OSTRZEŻENIE:
    ;; 
    ;; Wszystkie czynności wykonywane na tym systemie są monitorowane
    ;; i rejestrowane.
    ;;
    ;; Nieupoważniony dostęp do systemu jest wykrywany w sposób
    ;; automatyczny i NATYCHMIAST zgłaszany organom ścigania.
  '';

}
