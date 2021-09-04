
{ config, pkgs, lib, nodes, ... }:

with builtins;

{
  networking.firewall.trustedInterfaces = [ "tun0" ];
  networking.firewall.allowedUDPPorts = [ 1194 ];
  networking.nat = {
    enable = true;
    internalInterfaces  = [ "tun0" ];
  };

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
    ifconfig-pool-persist ovpn.txt
    push "redirect-gateway def1 bypass-dhcp"
    push "dhcp-option DNS 8.8.8.8"
    push "dhcp-option DNS 8.8.4.4"
    keepalive 10 120
    cipher AES-256-CBC
    user nobody
    group nogroup
    persist-key
    persist-tun
    verb 3
    crl-verify /run/keys/ovpnCrl
  '';

}
