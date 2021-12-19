{ config, lib, pkgs, ... }:

let
  restartAlways = pkgs.writeShellScript "loop-while-i3" ''
  while [ "`ls $XDG_RUNTIME_DIR/i3/ipc-socket* | wc -l`" -ne 0 ]; do
    "$@";
    sleep 0.2;
  done
  '';
   initCommands = with pkgs; ''
    exec --no-startup-id ${restartAlways} ${rxvt-unicode}/bin/urxvt -name dropdownterminal
    exec --no-startup-id ${restartAlways} ${pcmanfm}/bin/pcmanfm -n --role=dropdownpcmanfm
    exec --no-startup-id ${restartAlways} ${udiskie}/bin/udiskie -s --no-password-cache -f ${pcmanfm}/bin/pcmanfm
    exec --no-startup-id ${restartAlways} ${dunst}/bin/dunst -conf ${./dunstrc}

    exec_always --no-startup-id ${pkgs.killall}/bin/killall ${xcape}/bin/xcape
    exec_always --no-startup-id ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 23 = Alt_L Meta_L Alt_L Meta_L"
    exec_always --no-startup-id ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 222 = Tab"
    exec_always --no-startup-id ${pkgs.xcape}/bin/xcape -e "Alt_L=Tab"
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

  environment.variables.GDK_PIXBUF_MODULE_FILE =
    "${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";

  services.xserver.windowManager.i3 = {
    enable = true;
    package = pkgs.i3-gaps;
    configFile = with pkgs; runCommand "i3.conf" { inherit initCommands; } ''
        substitute ${./i3.conf} $out \
            --replace " i3status " " ${i3status}/bin/i3status " \
            --replace /etc/i3status.conf ${./i3status.conf} \
            --replace togglemonitor ${togglemonitor}/bin/togglemonitor \
            --replace gsimplecal ${gsimplecal}/bin/gsimplecal \
            --replace rofi ${rofi}/bin/rofi \
            --replace passmenu ${rofi-passmenu}/bin/passmenu \
            --replace emacsclient ${import ../emacs { inherit pkgs; }}/bin/emacsclient
        substituteAllInPlace $out
    '';

  };

}
