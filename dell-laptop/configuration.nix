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
    ./binary-caches.nix
    ./direnv.nix
  ];

  nixpkgs.overlays = import ./overlays/all-overlays.nix;

  nix = {
    maxJobs = lib.mkDefault 4;
    autoOptimiseStore = true;
    extraOptions = let mb = n: toString (n * 1024 * 1024); in ''
      min-free = ${mb 100}
      max-free = ${mb 500}
    '';
  };

  environment.variables.NIX_AUTO_RUN = "1";

  # Copy current configuration directory to store
  # Make sure not to import anything from ../ in this file
  # Ignores symlinks
  system.extraSystemBuilderCmds =
    with builtins;
    with lib;
    with pkgs;
    let
      paths =
        map (x: { name = x; path = copyPathToStore "/etc/nixos/${x}"; })
          (attrNames (filterAttrs (n: v: v != "symlink") (readDir /etc/nixos)));
      configuration = linkFarm "configuration" paths;
    in ''
    ln -s ${configuration} $out/current-configuration
    ln -s ${<nixos>} $out/current-nixpkgs
  '';

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  networking = {
    hostName = "nixos";
    useDHCP = false;
    networkmanager.enable = true;
    firewall = {
      enable = true;
      allowedTCPPorts = [ 5555 8554 ];
      allowedUDPPorts = [ 5555 8554 ];
      trustedInterfaces = [ "tap0" ];
    };
  };

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

    # FIXME i3 specific
    GDK_PIXBUF_MODULE_FILE="${pkgs.librsvg.out}/lib/gdk-pixbuf-2.0/2.10.0/loaders.cache";
  };

  environment.shellAliases = {
    l = "ls -lah --color=auto";
    c = "cd";
    ".." = "cd ..";
    mkd = "mkdir -pv";
    mkdir = "mkdir -pv";
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

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  programs.nm-applet.enable = true;

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark;

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

  services.xserver.videoDrivers = [ "intel" ];

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  services.transmission = {
    enable = true;
    openFirewall = true;
    settings.download-dir = "/home/kpg/torrents";
  };

  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  virtualisation.virtualbox.host.enable = true;

  virtualisation.podman.enable = true;

  fonts.fonts = with pkgs; [
    anonymousPro
    inconsolata
  ];

  # FIXME put in doas.nix
  security.sudo.enable = false;
  security.doas.enable = true;
  security.doas.extraRules = [
    { users = [ "kpg" ];
      keepEnv = true;
      persist = true;
    }
    { users = [ "kpg" ];
      cmd = "nixos-rebuild";
      noPass = true;
      keepEnv = true;
    }
    { users = [ "kpg" ];
      cmd = "nix-channel";
      noPass = true;
      keepEnv = true;
    }];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups.kpg = { gid = 1000; members = [ "kpg" ]; };
  users.users.kpg = {
    isNormalUser = true;
    initialHashedPassword = "";
    extraGroups = [ "wheel" "networkmanager" "transmission" "wireshark" ];
  };

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "20.09"; # Did you read the comment?

}
