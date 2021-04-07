{ config, lib, pkgs, ... }:

let
  immortalize = pkgs.writeShellScript "immortalize" ''
  while [ "`ls $XDG_RUNTIME_DIR/i3/ipc-socket* | wc -l`" -ne 0 ]; do
    "$@";
    sleep 1;
  done
  '';
  immortals = with pkgs; ''
    exec --no-startup-id ${immortalize} ${rxvt-unicode}/bin/urxvt -name dropdownterminal
    exec --no-startup-id ${immortalize} ${pcmanfm}/bin/pcmanfm -n --role=dropdownpcmanfm
    exec --no-startup-id ${immortalize} ${udiskie}/bin/udiskie -s --no-password-cache -f ${pcmanfm}/bin/pcmanfm
    exec --no-startup-id ${immortalize} ${dunst}/bin/dunst -conf ${./dunstrc}
  '';
in
{
  services.compton = {
    enable          = true;
    fade            = true;
    shadow          = true;
    fadeDelta       = 3;
    shadowExclude = [ "class_g = 'slop'" "class_g = 'locate-pointer'"];
  };

  environment.variables.XENVIRONMENT = "${./Xresources}";

  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    configFile = with pkgs; runCommand "i3.conf" {} ''
        substitute ${./i3.conf} $out \
            --replace @immortals@ "${immortals}" \
            --replace " i3status " " ${i3status}/bin/i3status " \
            --replace /etc/i3status.conf ${./i3status.conf} \
            --replace togglemonitor ${togglemonitor}/bin/togglemonitor \
            --replace gsimplecal ${gsimplecal}/bin/gsimplecal \
            --replace rofi ${rofi}/bin/rofi \
            --replace emacsclient ${import ../emacs {}}/bin/emacsclient
    '';

  };

}
