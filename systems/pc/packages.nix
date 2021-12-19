{ pkgs, config, ... }:

{ environment.systemPackages = with pkgs; [

  # Xorg
  xcape
  xclip
  xorg.xbacklight
  xorg.xmodmap
  xsel

  # urxvt
  rxvt-unicode
  w3m
  jq
  poppler_utils

  # Programming
  ecl
  sbcl

  # Development utils
  binutils
  clang-tools  # contains clangd
  cloc
  colordiff
  gdb

  # Search utils
  ag
  fd
  fzf
  ripgrep

  # Version control
  cvs
  fsl fossil
  git
  mercurial
  subversion

  # Shell utils
  curl
  dos2unix
  file
  htop
  killall
  lsof
  lz4
  p7zip
  (pass.withExtensions (es: [ es.pass-otp ]))
  patchelf
  pwgen
  rlwrap
  ranger
  tree
  unzip
  wget
  zip
  zstd

  # Network utils
  nmap
  openvpn

  # Weird things
  flashrom
  graphviz
  pandoc
  sqlite

  # Desktop utils
  feh
  pavucontrol
  xournalpp
  xsensors
  zathura

  # Bad habits
  vim

];}
