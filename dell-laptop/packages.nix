{ pkgs, config, ... }:

{ environment.systemPackages = with pkgs; [

  # Xorg
  xclip
  xsel

  # urxvt
  rxvt-unicode
  w3m
  jq
  poppler_utils

  # Programming
  ecl
  sbcl

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
  vim
  vlc
  youtube-dl

];}
