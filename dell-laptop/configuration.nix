# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports =
  [ ./hardware-configuration.nix
    ./packages.nix
    ./luks.nix
    ./i3
  ];

  nixpkgs.pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/6e7f25001fe6874f7ae271891f709bbf50a22c45.tar.gz";
    sha256 = "1x04j4351pqiqbpkq6g308mxcvb5aqnwv8l2vmlxkgvq5phzky7z";
  }) {};

  nixpkgs.overlays = import ./overlays/all-overlays.nix;

  nix.maxJobs = lib.mkDefault 4;
  nix.autoOptimiseStore = true;
  nix.extraOptions = let mb = n: toString (n * 1024 * 1024); in ''
    min-free = ${mb 100}
    max-free = ${mb 500}
    keep-outputs = true
    keep-derivations = true
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  # Copy configuration directory to store. Make sure not to import anything from ../ in this file.
  system.extraSystemBuilderCmds = ''
    ln -s ${builtins.dirOf <nixos-config>} $out/current-configuration
  '';

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  networking.hostName = "nixos";
  networking.useDHCP = false;
  networking.networkmanager.enable = true;
  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 5555 ];
  networking.firewall.allowedUDPPorts = [ 5555 ];

  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/Amsterdam";

  environment.variables = {
    TERMINAL = "urxvt";
    BROWSER = "chromium";
    EDITOR = "emacsclient -nw -c";

    XDG_CONFIG_HOME = "$HOME/.config";
    XDG_CACHE_HOME = "$HOME/.cache";
    XDG_DATA_HOME = "$HOME/.local/share";

    GDK_PIXBUF_MODULE_FILE="${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  };

  environment.shellAliases = {
    l = "ls -lah --color=auto";
    c = "cd";
    ".." = "cd ..";
    mkd = "mkdir -pv";
    mkdir = "mkdir -pv";
    rm = "rm -v";
    cp = "cp -v";
    rsync = "rsync -v";
    mv = "mv -v";
    s = "sudo";
    n = "nix";
    h = "htop";
    f = "fossil";
    g = "git";
    e = "$EDITOR";
    vim = "$EDITOR";
    x = "exit";
    r = "ranger";
    nuke = "shred -zu";
  };

  programs.bash.interactiveShellInit = ''
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
  '';

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  programs.nm-applet.enable = true;

  services.printing.enable = false;

  location = {
    latitude = 52.0;
    longitude = 14.0;
  };

  services.redshift = {
    enable = true;
    temperature = { day = 5500; night = 2500; };
  };

  services.emacs = {
    enable = true;
    package = import ./emacs {};
  };

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "pl";
  services.xserver.xkbOptions = "altwin:swap_lalt_lwin,ctrl:nocaps";

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.lightdm.extraConfig = "user-authority-in-system-dir = true";
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 23 = Alt_L Meta_L Alt_L Meta_L"
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 222 = Tab"
    ${pkgs.xcape}/bin/xcape -e "Alt_L=Tab"
  '';

  services.xserver.videoDrivers = [ "intel" ];

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  fonts.fonts = with pkgs; [
    anonymousPro
    inconsolata
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups.kpg = { gid = 1000; members = [ "kpg" ]; };
  users.users.kpg = {
    isNormalUser = true;
    initialHashedPassword = "";
    extraGroups = [ "wheel" "networkmanager" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
