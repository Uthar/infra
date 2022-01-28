
{ config, pkgs, lib, nodes, ... }:

{
  environment.systemPackages = [ pkgs.git ];

  users.users.git = {
    isSystemUser = true;
    createHome = false;
    home = "/srv/git";
    shell = "${pkgs.git}/bin/git-shell";
    group = "git";
  };
}
