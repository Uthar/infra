{ config, lib, pkgs,  ... }:

with lib;
let
  cfg = config.services.selenium;
in
{

  options.services.selenium = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description = ''
        standalone selenium server
      '';
    };

    port = mkOption {
      type = types.int;
      default = 4444;
      description = ''
          The port number the server will use
        '';
    };

    host = mkOption {
      type = types.str;
      default = "127.0.0.1";
      description = ''
        IP or hostname : usually determined automatically. Most
        commonly useful in exotic network configurations (e.g. network
        with VPN)
      '';
    };

    user = mkOption {
      type = types.str;
      default = "selenium";
      description = ''
        User to run as
      '';
    };

    group = mkOption {
      type = types.str;
      default = "selenium";
      description = ''
        Group to run as
      '';
    };
  };

  config = mkIf cfg.enable {
    systemd.services.selenium = {
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      path = [ pkgs.ungoogled-chromium ];

      serviceConfig = {
        Restart = "always";
        RestartSec = "5s";
        ExecStart = "${pkgs.selenium-server-standalone}/bin/selenium-server -port ${toString cfg.port} -host ${cfg.host}";
        User = cfg.user;
        Group = cfg.group;
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
