{ config, pkgs, lib, ... }:

{

  imports = [ ./hardware-configuration.nix ];

  services.journald.extraConfig = "SystemMaxUse=1G";

  boot.loader.grub = { enable = true; version = 2; device = "/dev/vda"; };

  networking.useDHCP = false;

  networking.publicIPv4 = "104.244.74.41";

  networking.interfaces.ens3.ipv4.addresses = [
    { address = config.networking.publicIPv4; prefixLength = 24; }
  ];

  networking.defaultGateway = { address = "104.244.74.1"; interface = "ens3"; };

  networking.nat.externalInterface = "ens3";

  networking.nameservers = [ "8.8.8.8" ];

  services.openssh.enable = true;

  # FIXME no root login
  users.users.root = {
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDF6vChYUxR9N6zI3cSucBG5zMEfF3AQJzOtfcvIG23Xbb68pIdgVM188rhCanjchw1tLbgeO86QAa3K0oQcxsLMWntWTD3y4ERLgxxW5VEnB2G5ZQsX3BYlxLHoLNBEtqipJUN0Mjrczoe/xlNljeeiCG7VvwG5Uwl35+ZhLUo18C8VMLII9wErMXIi8Iaw3Hcvrl9WsGvF9urqaxJEczMPLmUu7rpe4i7qoA1s1v9P8lqHv6YtSxxa/bi5SpkRdcCdQ4t22fDPVkP2gyAet/AfgoBWKWp3FZ8XdTwPlQRXOLRA1os3hAsf4ZxvTJNNdyFZUTISnFXhKVL+qdMRUGJ" ];
  };

}
