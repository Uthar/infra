
{ config, lib, pkgs, ... }:

{

  location = {
    latitude = 52.0;
    longitude = 14.0;
  };

  services.redshift = {
    enable = true;
    temperature = { day = 5500; night = 2500; };
  };

}
