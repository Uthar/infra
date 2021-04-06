{ config, lib, pkgs, ... }:

let
  makeImmortalService = { description, command, environment ? {}}: {
    inherit description environment;
    wantedBy = [ "graphical-session.target" ];
    partOf = [ "graphical-session.target" ];
    serviceConfig = {
      ExecStart = command;
      Restart = "always";
    };
  };
in
{
  services.compton = {
    enable          = true;
    fade            = true;
    shadow          = true;
    fadeDelta       = 3;
    shadowExclude = [ "class_g = 'slop'" "class_g = 'locate-pointer'"];
  };

  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    configFile = with pkgs; runCommand "i3.conf" {} ''
        substitute ${./i3.conf} $out \
            --replace " i3status " " ${i3status}/bin/i3status " \
            --replace /etc/i3status.conf ${./i3status.conf} \
            --replace togglemonitor ${togglemonitor}/bin/togglemonitor \
            --replace gsimplecal ${gsimplecal}/bin/gsimplecal \
            --replace rofi ${rofi}/bin/rofi \
            --replace emacsclient ${import ../emacs {}}/bin/emacsclient
    '';

  };

  systemd.user.services =
    { tabbed-urxvt = makeImmortalService {
        description = "dropdown tabbed+urxvt";
        command =
          let
            tabbed = "${pkgs.tabbed}/bin/tabbed";
            urxvt = "${pkgs.urxvt}/bin/urxvt";
          in "${tabbed} -f -n dropdownterminal ${urxvt} -embed";
      };

      pcmanfm = makeImmortalService {
        description = "dropdown pcmanfm";
        command = "${pkgs.pcmanfm}/bin/pcmanfm -n --role=dropdownpcmanfm";
      };

      udiskie = makeImmortalService {
        description = "udiskie automount daemon";
        environment = { inherit (config.environment.variables) GDK_PIXBUF_MODULE_FILE;
                        XDG_DATA_DIRS = "${pkgs.gnome3.adwaita-icon-theme}/share"; };
        command =
          let
            udiskie = "${pkgs.udiskie}/bin/udiskie";
            pcmanfm = "${pkgs.pcmanfm}/bin/pcmanfm";
          in "${udiskie} -s --no-password-cache -f ${pcmanfm}";
      };

      dunst = makeImmortalService {
        description = "dunst notification daemon";
        command = "${pkgs.dunst}/bin/dunst -conf ${./dunstrc}";
      };
    };
}
