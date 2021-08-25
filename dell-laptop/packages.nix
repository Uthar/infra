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
  guile_3_0
  sbcl

  # Development utils
  guix
  nixops

  # Search utils
  ag
  fd
  fzf
  ripgrep

  # Version control
  fsl fossil
  git
  mercurial

  # Shell utils
  curl
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
