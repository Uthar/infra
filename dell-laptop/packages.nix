{ pkgs, config, ... }:

{environment.systemPackages = with pkgs; [

  # Desktop
  udiskie
  dunst
  pcmanfm

  # Xorg
  xsel
  xclip

  # urxvt
  rxvt-unicode
  tabbed
  w3m
  jq
  poppler_utils

  # Programming
  clojure
  ecl
  sbcl
  leiningen

  # Search utils
  ag
  fd
  fzf
  ripgrep

  # Version control
  fossil
  git
  mercurial

  # Shell utils
  curl
  direnv
  nix-direnv
  htop
  killall
  p7zip
  pass
  patchelf
  ranger
  tree
  wget

  # The rest
  baobab
  clang-tools
  gsimplecal
  rofi
  ungoogled-chromium
  vim
  vlc
  youtube-dl

  (writeShellScriptBin "togglemonitor" ''
    if ! [ -f /tmp/togglemonitor ]; then
        echo "" > /tmp/togglemonitor
    fi
    toggle=`cat /tmp/togglemonitor`
    if [ $toggle ]; then
        ${xorg.xrandr}/bin/xrandr --output VGA1 --auto --above LVDS1
        echo "" > /tmp/togglemonitor
    else
        ${xorg.xrandr}/bin/xrandr --output VGA1 --off
        echo 1 > /tmp/togglemonitor
    fi
  '')

];}
