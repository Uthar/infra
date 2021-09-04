{ config, pkgs, lib, nodes, ... }:

{
  networking.firewall = {
    allowedUDPPorts = [ 64738 ];
    allowedTCPPorts = [ 64738 ];
  };

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
}
