{ pkgs, config, ... }:

{environment.systemPackages = with pkgs; [

  # desktop env stuff
  udiskie
  dunst
  pcmanfm

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
  bat
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
  st
  ungoogled-chromium
  vim
  vlc
  youtube-dl

];}
