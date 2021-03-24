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

];}
