{ config, ... }:

{
  boot.loader.grub.enableCryptodisk = true;

  boot.initrd.secrets = {
    "/root/key" = "/root/key";
  };

  boot.initrd.luks.devices.encrypted = {
    device = "/dev/disk/by-uuid/24c74e06-e54f-4750-9ae0-809c1bab6d4e";
    keyFile = "/root/key";
  };
}
