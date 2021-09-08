
{ config, lib, pkgs,  ... }:

with lib;
let
  cfg = config.services.happet;
in
{

  options.services.happet = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        happet automation http server
      '';
    };

    user = mkOption {
      type = types.str;
      default = "happet";
      description = ''
        User to run as
      '';
    };

    group = mkOption {
      type = types.str;
      default = "happet";
      description = ''
        Group to run as
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.services.happet = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];

      serviceConfig = {
        Restart = "always";
        RestartSec = "5s";
        # Impure like this until I figure out how to fetch from
        # private github repo
        ExecStart = "/home/happet";
        User = cfg.user;
        Group = cfg.group;
        EnvironmentFile = [ "/run/keys/happet_password" "/run/keys/happet_login" ];
      };
    };

    users.users.${cfg.user} = {
      isSystemUser = true;
    };

    users.groups.${cfg.user} = {
      members = [ cfg.user ];
    };

  };
}
