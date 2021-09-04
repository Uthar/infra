{ acmep ? false
, fail2banp ? true
}:

let

  layer-2-openvpn =
    { config, pkgs, lib, ... }:
    {
      deployment.keys = with lib; {
        totalaCa.keyCommand    = [ "pass" "infra/prod/layer-2-openvpn/ca.crt" ];
        totalaCert.keyCommand  = [ "pass" "infra/prod/layer-2-openvpn/server.crt" ];
        totalaKey.keyCommand   = [ "pass" "infra/prod/layer-2-openvpn/server.key" ];
        totalaDh.keyCommand    = [ "pass" "infra/prod/layer-2-openvpn/dh.pem" ];
        totalaTauth.keyCommand = [ "pass" "infra/prod/layer-2-openvpn/ta.key" ];
      };
      imports = [ ../../systems/layer-2-openvpn ];
    };


  layer-3-openvpn =
    { config, pkgs, lib, ... }:
    {
      deployment.keys = with lib; {
        ovpnCa.keyCommand    = [ "pass" "infra/prod/layer-3-openvpn/ca.crt" ];
        ovpnCert.keyCommand  = [ "pass" "infra/prod/layer-3-openvpn/server.crt" ];
        ovpnKey.keyCommand   = [ "pass" "infra/prod/layer-3-openvpn/server.key" ];
        ovpnDh.keyCommand    = [ "pass" "infra/prod/layer-3-openvpn/dh.pem" ];
        ovpnTauth.keyCommand = [ "pass" "infra/prod/layer-3-openvpn/ta.key" ];
        ovpnCrl.keyCommand   = [ "pass" "infra/prod/layer-3-openvpn/crl.pem" ];
      };
      imports = [ ../../systems/layer-3-openvpn ];
    };

  binary-cache =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/binary-cache ];
      services.httpd.virtualHosts."cache.${config.networking.domain}" = {
        locations."/".proxyPass = lib.mkForce "unix:${config.services.bcache.socketPath}|http://localhost/";
      };
    };

  fossil =
    { config, pkgs, lib, ... }:
    {
      imports = [ ../../systems/fossil ];
      networking.extraHosts = ''
        127.0.0.1 fossil
      '';
      services.fossil.baseurl = lib.mkForce "https://fossil.${config.networking.domain}";
      services.fossil.https = true;
    };

  mail =
    { config, pkgs, lib, ... }:
    {
      deployment.keys = with lib; {
        kMailAccount.destDir = "/run/mailserverKeys";
        kMailAccount.keyCommand = [ "pass" "infra/prod/mailserver/k" ];
      };

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
      users.users.git.openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDF6vChYUxR9N6zI3cSucBG5zMEfF3AQJzOtfcvIG23Xbb68pIdgVM188rhCanjchw1tLbgeO86QAa3K0oQcxsLMWntWTD3y4ERLgxxW5VEnB2G5ZQsX3BYlxLHoLNBEtqipJUN0Mjrczoe/xlNljeeiCG7VvwG5Uwl35+ZhLUo18C8VMLII9wErMXIi8Iaw3Hcvrl9WsGvF9urqaxJEczMPLmUu7rpe4i7qoA1s1v9P8lqHv6YtSxxa/bi5SpkRdcCdQ4t22fDPVkP2gyAet/AfgoBWKWp3FZ8XdTwPlQRXOLRA1os3hAsf4ZxvTJNNdyFZUTISnFXhKVL+qdMRUGJ" ];
      imports = [ ../../systems/git ];
    };

  murmur =
    { config, pkgs, lib, ... }:
    {
      deployment.keys = with lib; {
        murmurPassword.user = "murmur";
        murmurPassword.keyCommand = [ "pass" "infra/prod/murmur/password" ];
      };
      imports = [ ../../systems/murmur ];
    };

  website =
    { config, pkgs, lib, ... }:
    {
      services.httpd = {
        enable = true;
        virtualHosts.${config.networking.domain} = {
          locations = lib.mkForce {};
          documentRoot = "/srv/git";
        };
      };
    };

  proxy =
    { config, pkgs, lib, nodes, ... }:
    {
      services.httpd.adminAddr = "k@${config.networking.domain}";
      security.acme.acceptTerms = acmep;
      security.acme.email = "k@${config.networking.domain}";
      imports = [ ../../systems/proxy ];
    };

in {

  network.description = "production environment";

  network.storage.legacy = {};

  defaults =
    { config, pkgs, lib, ... }:
    { nix.autoOptimiseStore = true;
      environment.systemPackages = with pkgs; [ sqlite ranger htop file ];
      networking.domain = "galkowski.xyz";
      deployment.sshOptions = ["-oIdentitiesOnly=yes"];
    };


  #### Frantech nodes

  amalgam =
    { config, pkgs, lib, nodes, ... }:
    { deployment.targetHost = config.networking.publicIPv4;
      deployment.provisionSSHKey = false;

      imports =
        [ ../../machines/buyvm-lu-512/104.244.74.41
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

      services.fail2ban.enable = fail2banp;

    };


  #### AWS nodes

  resources.ec2SecurityGroups.nixopsGrp = {
    region = "eu-central-1";
    rules = [
      { fromPort = 22;
        toPort = 22;
        sourceIp = "104.244.74.41/32";
      }
      { fromPort = 80;
        toPort = 80;
        sourceIp = "0.0.0.0/0";
      }
    ];
  };

  resources.ec2KeyPairs.happetKey = {
    region = "eu-central-1";
  };

  happet =
    { config, pkgs, lib, nodes, resources, ... }:

    with resources.ec2KeyPairs;
    with resources.ec2SecurityGroups;
    {
      deployment.targetEnv = "ec2";
      deployment.ec2 = {
        keyPair = happetKey;
        securityGroups = [ nixopsGrp ];
        region = pkgs.lib.mkDefault "eu-central-1";
        instanceType = "t2.micro";
        ebsBoot = true;
        ebsInitialRootDiskSize = 10;
      };

      services.httpd.adminAddr = "k@galkowski,xyz";
      services.httpd.enable = true;

      services.selenium.enable = true;

      # Just in case i lose nixops.sqlite
      users.users.root = {
        openssh.authorizedKeys.keyFiles = [ /home/kpg/.ssh/id_rsa.pub ];
      };

      networking.firewall.allowedTCPPorts = [ 80 ];
    };

}
