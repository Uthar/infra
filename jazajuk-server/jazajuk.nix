{

  # TODO Make it a function and pass in the details

  network.description = "Jazajuk Machine Farm";

  network.enableRollback = false;
  network.nixpkgs = import (builtins.fetchTarball {
    url    = "https://github.com/Uthar/nixpkgs/archive/aab74d2b13235dbae3d40e78ae28ea9aaa3f2263.tar.gz";
    sha256 = "0di6di23bvdlfpqim3jk717mixs43swxfjjm6lilid8byq24rkiy";
  }) {};

  jazajuk = { config, pkgs, lib, ... }: {

    boot.initrd.availableKernelModules = [ "ata_piix" "uhci_hcd" "virtio_pci" "sr_mod" "virtio_blk" ];
    boot.initrd.kernelModules = [ "dm-snapshot" ];
    fileSystems."/" = { device = "/dev/nixos/main"; fsType = "ext4"; };
    swapDevices = [ { device = "/dev/nixos/swap"; } ]; 
    nix.maxJobs = lib.mkDefault 1;
    virtualisation.hypervGuest.enable = false;

    deployment.targetHost = "104.244.74.41";

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
      murmurPassword.text = ''
      MURMUR_PASSWORD=${readFile ./keys/murmurPassword}
      '';

    };

    nix.autoOptimiseStore = true;
    system.extraSystemBuilderCmds = ''
      ln -s /etc/nixos $out/current-configuration
    '';                                 

    boot.loader.grub = { enable = true; version = 2; device = "/dev/vda"; };

    environment.systemPackages = with pkgs; [
      bind.dnsutils
      htop
      jq
      openvpn
      git
      ranger
      htop
    ];

    networking.firewall.allowedTCPPorts = [ 64738 80 443 3000 ];
    networking.hostName = "jazajuk";
    networking.useDHCP = false;
    networking.interfaces.ens4.useDHCP = false;
    networking.interfaces.ens3.ipv4.addresses = [
      { address = "104.244.74.41"; prefixLength = 24; }
    ];
    networking.defaultGateway = { address = "104.244.74.1"; interface = "ens3"; };
    networking.nameservers = [ "8.8.8.8" ];
    networking.firewall.allowedUDPPorts = [ 64738 1194 1338 ];
    networking.firewall.trustedInterfaces = [ "tun0" "tap0" ];

    networking.nat = {
      enable = true;
      externalInterface = "ens3";
      internalInterfaces  = [ "tun0" "tap0" ];
    };

    services.openssh.enable = true;

    networking.bridges.br0.interfaces = [ "ens4" "tap0" ];
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
      let
        keyServices = [
          "ovpnCa-key.service"
          "ovpnCert-key.service"
          "ovpnKey-key.service"
          "ovpnDh-key.service"
          "ovpnTauth-key.service"
          "ovpnCrl-key.service"
        ];
      in { after = keyServices; wants = keyServices; };

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
      let
        keyServices = [ 
          "totalaCa-key.service"
          "totalaCert-key.service"
          "totalaKey-key.service"
          "totalaDh-key.service"
          "totalaTauth-key.service"
        ];
      in { after = keyServices; wants = keyServices; };

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

    systemd.services.murmur =
      let
        keyServices = [ "murmurPassword-key.service" ];
      in {
        after = keyServices;
        wants = keyServices;
      };

    services.fossil = {
      enable = true;
    };

    users.users.git = {
      createHome = true;
      home = "/srv/git";
      group = "wwwrun";
      shell = "${pkgs.git}/bin/git-shell";
      openssh.authorizedKeys.keyFiles = [ ./ssh/id_rsa.pub ];
    };

    security.acme.acceptTerms = true;
    services.httpd = {
      enable = true;
      virtualHosts."galkowski.xyz" = {
        addSSL = true;
        documentRoot = "/srv/git";
        enableACME = true;
        adminAddr = "k@demondust.xyz";
      };
    };

    users.users.root = {
      openssh.authorizedKeys.keyFiles = [ ./ssh/id_rsa.pub ];
    };

  };

}
