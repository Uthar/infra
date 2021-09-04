{ config, pkgs, lib, nodes, ... }:

{
  services.bcache = {
    enable = true;
    group = "wwwrun";
    compressionType = "gzip";
    secretKeyFile = "/srv/bcache/secret-key-file";
  };
}
