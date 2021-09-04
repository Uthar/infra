{ config, pkgs, lib, nodes, ... }:

{
  networking.interfaces.vpn.useDHCP = false;
  networking.interfaces.vpn.virtual = true;
  networking.firewall.trustedInterfaces = [ "tap0" ];
  networking.bridges.br0.interfaces = [ "vpn" "tap0" ];
  networking.interfaces.br0.ipv4.addresses = [ { address = "10.9.0.2"; prefixLength = 24; } ];
  networking.firewall.allowedUDPPorts = [ 1338 ];
  networking.nat = {
    enable = true;
    internalInterfaces  = [ "tap0" ];
  };

  services.openvpn.servers.layer-2.config = ''
    port 1338
    proto udp
    dev tap0
    ca /run/keys/totalaCa
    cert /run/keys/totalaCert
    key /run/keys/totalaKey
    dh /run/keys/totalaDh
    topology subnet
    ifconfig-pool-persist totala.txt
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
    verb 4
    mute 20
    explicit-exit-notify 1
  '';

}
