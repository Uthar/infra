let
  nixpkgs = import (builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/82151321eeaef290b8345803e0b217a261b7c4e1.tar.gz";
    sha256 = "1n53jn8793midwdwiqk74l4vf0g94wg1bypab4cx1ydlbs20x882";
  }) { overlays = import ../dell-laptop/overlays/all-overlays.nix; };
in
{ hardened ? true }:
{

  # TODO Make it a function and pass in the details

  network.description = "Jazajuk Machine Farm";

  network.storage.legacy = {};

  network.enableRollback = false;
  network.nixpkgs = nixpkgs;

  jazajuk = { config, pkgs, lib, ... }:

    let
      publicAddress = "104.244.74.41";
      publicInterface = "ens3";
      domainName = "galkowski.xyz";
      baseConfig = {
        environment.systemPackages = with pkgs; [ ranger htop file ];
        services.journald.extraConfig = "SystemMaxUse=10M";
        #boot.kernelParams = [ "cgroup_no_v1=all" "systemd.unified_cgroup_hierarchy=yes" ];
      };
      waitForServices = services: { after = services; wants = services; };
    in {

      nixpkgs.pkgs = nixpkgs;

      imports = [ baseConfig ./hardware-configuration.nix ./fossil-server.nix ./bcache.nix ];

    virtualisation.hypervGuest.enable = false;

    deployment.targetHost = publicAddress;
    networking.publicIPv4 = publicAddress;

    deployment.provisionSSHKey = false;

    deployment.keys = with lib; {

      ovpnCa.text = readFile ./pki/ca.crt;
      ovpnCert.text = readFile ./pki/issued/server.crt;
      ovpnKey.text = readFile ./pki/private/server.key;
      ovpnDh.text = readFile ./pki/dh.pem;
      ovpnTauth.text = readFile ./pki/ta.key;
      ovpnCrl.text = readFile ./pki/crl.pem;

      totalaCa.text = readFile ./pki/ca.crt;
      totalaCert.text = readFile ./pki/issued/server.crt;
      totalaKey.text = readFile ./pki/private/server.key;
      totalaDh.text = readFile ./pki/dh.pem;
      totalaTauth.text = readFile ./pki/ta.key;

      murmurPassword.user = "murmur";
      murmurPassword.text = "MURMUR_PASSWORD=${readFile ./keys/murmurPassword}";

      kMailAccount.destDir = "/run/mailserverKeys";
      kMailAccount.text = readFile ./keys/mailserver/k;

    };

    nix.autoOptimiseStore = true;

    boot.loader.grub = { enable = true; version = 2; device = "/dev/vda"; };

    environment.systemPackages = with pkgs; [ fossil sqlite git ];

    services.fail2ban.enable = hardened;

    networking.firewall.allowedTCPPorts = [ 64738 80 443 8554 ];
    networking.hostName = "jazajuk";
    networking.useDHCP = false;
    networking.interfaces.vpn.useDHCP = false;
    networking.interfaces.vpn.virtual = true;
    networking.interfaces.${publicInterface}.ipv4.addresses = [
      { address = publicAddress; prefixLength = 24; }
    ];
    networking.defaultGateway = { address = "104.244.74.1"; interface = publicInterface; };
    networking.nameservers = [ "8.8.8.8" ];
    networking.firewall.allowedUDPPorts = [ 64738 1194 1338 ];
    networking.firewall.trustedInterfaces = [ "tun0" "tap0" ];

    networking.nat = {
      enable = true;
      externalInterface = publicInterface;
      internalInterfaces  = [ "tun0" "tap0" "ve-mailserver" "ve-jitsi" ];
    };

    services.openssh.enable = true;

    networking.bridges.br0.interfaces = [ "vpn" "tap0" ];
    networking.interfaces.br0.ipv4.addresses = [ { address = "10.9.0.2"; prefixLength = 24; } ];

    services.openvpn.servers.ovpn.config = ''
    port 1194
    proto udp
    dev tun
    sndbuf 0
    rcvbuf 0
    ca /run/keys/ovpnCa
    cert /run/keys/ovpnCert
    key /run/keys/ovpnKey
    dh /run/keys/ovpnDh
    auth SHA512
    tls-auth /run/keys/ovpnTauth 0
    topology subnet
    server 10.43.0.0 255.255.255.0
    ifconfig-pool-persist ipp.txt
    push "redirect-gateway def1 bypass-dhcp"
    push "dhcp-option DNS 8.8.8.8"
    push "dhcp-option DNS 8.8.4.4"
    keepalive 10 120
    cipher AES-256-CBC
    user nobody
    group nogroup
    persist-key
    persist-tun
    status openvpn-status.log
    verb 3
    crl-verify /run/keys/ovpnCrl
    '';

    systemd.services.openvpn-ovpn =
      waitForServices [
        "ovpnCa-key.service"
        "ovpnCert-key.service"
        "ovpnKey-key.service"
        "ovpnDh-key.service"
        "ovpnTauth-key.service"
        "ovpnCrl-key.service"
      ];

    services.openvpn.servers.totala.config = ''
    port 1338
    proto udp
    dev tap0
    ca /run/keys/totalaCa
    cert /run/keys/totalaCert
    key /run/keys/totalaKey
    dh /run/keys/totalaDh
    topology subnet
    ifconfig-pool-persist ipp.txt
    server-bridge 10.9.0.10 255.255.255.0 10.9.0.50 10.9.0.100
    server-bridge
    client-to-client
    keepalive 10 120
    tls-auth /run/keys/totalaTauth 0
    auth SHA512
    cipher AES-256-CBC
    user nobody
    group nogroup
    persist-key
    persist-tun
    status openvpn-bridge-status.log
    log-append  openvpn-bridge.log
    verb 4
    mute 20
    explicit-exit-notify 1
    '';

    systemd.services.openvpn-totala =
      waitForServices [
          "totalaCa-key.service"
          "totalaCert-key.service"
          "totalaKey-key.service"
          "totalaDh-key.service"
          "totalaTauth-key.service"
        ];

    services.murmur = {
      enable = true;
      imgMsgLength = 0;
      logDays = -1;
      logFile = "/dev/null";
      sendVersion = false;
      textMsgLength = 0;
      welcometext = "Welcome to the CADMIUM server!";
      environmentFile = "/run/keys/murmurPassword";
      password="$MURMUR_PASSWORD";
    };

    systemd.services.murmur = waitForServices [ "murmurPassword-key.service" ];

    services.fossil = {
      enable = true;
      localhost = true;
      baseurl = "https://fossil.${domainName}";
      https = true;
    };

    services.bcache = {
      enable = true;
      group = "wwwrun";
      compressionType = "gzip";
      secretKeyFile = "/srv/bcache/secret-key-file";
    };

    users.users.git = {
      isSystemUser = true;
      createHome = false;
      home = "/srv/git";
      group = "wwwrun";
      shell = "${pkgs.git}/bin/git-shell";
      openssh.authorizedKeys.keyFiles = [ ./ssh/id_rsa.pub ];
    };

    security.acme.acceptTerms = true;
    security.acme.email = "k@demondust.xyz";
    services.httpd = {
      enable = true;
      extraConfig = ''
        ServerTokens Prod
        ServerSignature Off
      '';
      # extraModules =
      #   let
      #     mod_wsgi = pkgs.mod_wsgi.overrideAttrs (o:
      #       {
      #         buildInputs = (lib.lists.remove pkgs.python2 o.buildInputs)
      #                       ++ [ pkgs.mercurial.python pkgs.ncurses ];
      #       });
      #   in [
      #     { name = "wsgi"; path = "${mod_wsgi}/modules/mod_wsgi.so"; }
      #   ];
      virtualHosts.${domainName} = {
        addSSL = true;
        documentRoot = "/srv/git";
        enableACME = true;
        adminAddr = "k@demondust.xyz";
      };
      virtualHosts."cache.${domainName}" = {
        locations."/.well-known".proxyPass = "!";
        locations."/".proxyPass = "unix:${config.services.bcache.socketPath}|http://localhost/";
        adminAddr = "k@demondust.xyz";
        forceSSL = true;
        enableACME = true;
      };
      virtualHosts."jitsi.${domainName}" = {
        locations."/.well-known".proxyPass = "!";
        locations."/".proxyPass = "http://10.13.37.4/";
        adminAddr = "k@demondust.xyz";
        addSSL = true;
        enableACME = true;
      };
      virtualHosts."fossil.${domainName}" = {
        locations."/.well-known".proxyPass = "!";
        locations."/".proxyPass = "http://127.0.0.1:8080/";
        adminAddr = "k@demondust.xyz";
        addSSL = true;
        enableACME = true;
      };
      # virtualHosts."hg.${domainName}" = {
      #   adminAddr = "k@demondust.xyz";
      #   addSSL = true;
      #   enableACME = true;
      #   extraConfig =
      #     let
      #       hg = pkgs.mercurial;
      #       config = pkgs.writeTextFile { name="hgweb.config"; text = ''
      #       [paths]
      #       / = /srv/hg/repos/*
      #       ''; };
      #       hgweb = pkgs.runCommand "hgweb.wsgi" {} ''
      #         cp ${hg}/share/cgi-bin/hgweb.wsgi $out
      #         sed -i s,/path/to/python/lib,${hg}/lib/python3.8/site-packages, $out
      #         sed -i "s,#.*import sys,import sys," $out
      #         sed -i s,/path/to/repo/or/config,${config}, $out
      #       '';
      #     in ''
      #       # WSGIScriptAlias hangs, ScriptAlias is terribly slow
      #       WSGIScriptAlias / "${hgweb}/"
      #     '';
      # };
    };

    users.users.root = {
      openssh.authorizedKeys.keyFiles = [ ./ssh/id_rsa.pub ];
    };

    containers = let
        pkgsUnstable = import (builtins.fetchTarball {
          url    = "https://github.com/NixOS/nixpkgs/archive/f1f9a55fb4b1d5adeebfff6c5ec58ce445bf5e84.tar.gz";
          sha256 = "0iss3b7f4xc6czfwchs4qs3jy7y6l9cxmsyp77s4qksj0sn3mjnv";
        }) {};
        pkgs-21_05 = import (builtins.fetchTarball {
          url    = "https://github.com/NixOS/nixpkgs/archive/21.05.tar.gz";
          sha256 = "1ckzhh24mgz6jd1xhfgx0i9mijk6xjqxwsshnvq789xsavrmsc36";
        }) {};
    in {
      jitsi = {
        #pkgs = pkgsUnstable;
        autoStart = false;
        privateNetwork = true;
        hostAddress = "10.13.37.3";
        localAddress = "10.13.37.4";
        forwardPorts = [
          { containerPort = 10000; hostPort = 10000; }
          { containerPort = 4443; hostPort = 4443; }
        ];
        config = {
          imports = [ baseConfig ];
          services.jitsi-meet = {
            enable = true;
            hostName = "jitsi.${domainName}";
            config = {
              enableWelcomePage = false;
              prejoinPageEnabled = true;
              defaultLang = "fi";
            };
            interfaceConfig = {
              SHOW_JITSI_WATERMARK = false;
              SHOW_WATERMARK_FOR_GUESTS = false;
            };
          };
          services.jitsi-videobridge.nat.publicAddress = publicAddress;
          services.jitsi-videobridge.nat.localAddress = "10.13.37.4";
          services.jitsi-videobridge.openFirewall = true;
          networking.firewall.allowedTCPPorts = [ 80 443 ];
          services.nginx.virtualHosts."jitsi.${domainName}" = { enableACME = false; forceSSL = false; };
          #security.acme.email = "me@example.com";
          #security.acme.acceptTerms = true;
        };
      };

      mailserver = {
        #pkgs = pkgs-21_05;
        autoStart = true;
        privateNetwork = true;
        hostAddress = "10.13.37.1";
        localAddress = "10.13.37.2";
        bindMounts."/run/mailserverKeys" = {};
        forwardPorts = [
          { containerPort = 993; hostPort = 993; }
          { containerPort = 465; hostPort = 465; }
          { containerPort = 143; hostPort = 143; }
          { containerPort = 25; hostPort = 25; }
        ];
        config = let release = "7c06f610f15642e3664f01a51c08c64cc8835f51"; in {
          imports = [
            ./nixos-mailserver
            baseConfig
          ];

          networking.nameservers = [ "8.8.8.8" ];

          mailserver = {
            enable = true;
            fqdn = "mail.${domainName}";
            domains = [ domainName ];
            loginAccounts = {
              "k@${domainName}" = {
                # mkpasswd -m sha-512 "super secret password" > /hashed/password/file/location
                hashedPasswordFile = "/run/mailserverKeys/kMailAccount";

                aliases = [
                  "kasper@${domainName}"
                ];
              };
            };

            certificateScheme = 2;

            enableImap = false;
            enableImapSsl = true;
            enableSubmission = false;
            enableSubmissionSsl = true;

            enableManageSieve = true;
          };
        };
      };
    };
  };

}
