# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

{
  imports = 
  [ ./hardware-configuration.nix
    ./packages.nix
    ./luks.nix
  ];

  nixpkgs.overlays = import ./overlays/all-overlays.nix;

  nix.maxJobs = lib.mkDefault 4;
  nix.autoOptimiseStore = true;
  nix.extraOptions = let mb = n: toString (n * 1024 * 1024); in ''
    min-free = ${mb 100}
    max-free = ${mb 500}
  '';

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
  # networking.interfaces.eno1.useDHCP = true;
  # networking.interfaces.wlp2s0.useDHCP = true;

  networking.networkmanager.enable = true;
  networking.wireless.enable = false;

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 5555 ];
  networking.firewall.allowedUDPPorts = [ 5555 ];
  #networking.firewall.allowedTCPPortRanges = [];
  #networking.firewall.allowedUDPPortRanges = [];

  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  i18n.defaultLocale = "en_GB.UTF-8";
  time.timeZone = "Europe/Amsterdam";

  environment.variables = {
    TERMINAL = "st";
    BROWSER = "chromium";
  };

  environment.extraInit = ''
    # Use librsvg's gdk-pixbuf loader cache file as it enables gdk-pixbuf to load SVG files (important for icons)
    export GDK_PIXBUF_MODULE_FILE="$(echo ${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/*/loaders.cache)"
  '';

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
    f = "fossil";
    g = "git";
    e = "emacsclient -c";
    x = "exit";
    r = "ranger";
    nuke = "shred -zu";
  };

  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
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
    defaultEditor = true;
    package = import ./emacs.nix {};
  };

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  services.compton = {
    enable          = true;
    fade            = true;
    shadow          = true;
    fadeDelta       = 3;
    shadowExclude = [ "class_g = 'slop'" "class_g = 'locate-pointer'"];
  };

  # Enable touchpad support.
  services.xserver.libinput.enable = true;

  services.xserver.enable = true;
  services.xserver.layout = "pl";
  services.xserver.xkbOptions = "eurosign:e,altwin:swap_lalt_lwin,ctrl:nocaps";

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 23 = Alt_L Meta_L Alt_L Meta_L"
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 222 = Tab"
    ${pkgs.xcape}/bin/xcape -e "Alt_L=Tab"
  '';

  services.xserver.videoDrivers = [ "intel" ];

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.i3.package = pkgs.i3-gaps;
  services.xserver.windowManager.i3.configFile = ./i3.conf;
  environment.etc."i3status.conf".source = ./i3status.conf;
  environment.etc."dunstrc".source = ./dunstrc;

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

