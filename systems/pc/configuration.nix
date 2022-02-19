# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let me = "kpg"; in
{
  imports =
  [ ./packages.nix
    ./i3.nix
    ./binary-caches.nix
    ./direnv.nix
    ./doas.nix
    ./redshift.nix
    ./zfs.nix
    ./nix-unstable.nix
  ];

  nix = {
    autoOptimiseStore = true;
    extraOptions = let mb = n: toString (n * 1024 * 1024); in ''
      min-free = ${mb 100}
      max-free = ${mb 500}
    '';
    trustedUsers = [ me ];
  };

  environment.variables.NIX_AUTO_RUN = "1";
  environment.variables.HISTCONTROL = "ignorespace";

  # FIXME move firewall config to network declaration
  networking = {
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

  environment.shellAliases = {
    l = "ls -lah --color=auto";
    c = "cd";
    ".." = "cd ..";
    mkdir = "mkdir -pv";
    n = "nix";
    h = "htop";
    f = "fossil";
    g = "git";
    e = "$EDITOR";
    x = "exit";
    r = "ranger";
    s = "systemctl";
    j = "journalctl";
  }
  //
  (let
    rlwrap = x: {name=x; value="rlwrap ${x}";};
    wrapped = map rlwrap [
      "ecl" "sbcl" "abcl" "ccl" "clasp"
      "sqlite3" "tclsh" "wish"
      "guile" "scheme" "racket" "clojure" "kawa"
    ];
  in lib.listToAttrs wrapped);

  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  programs.nm-applet.enable = true;

  programs.wireshark.enable = true;
  programs.wireshark.package = pkgs.wireshark;

  services.printing.enable = true;

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';

  services.xserver.enable = true;
  services.xserver.layout = "pl";
  services.xserver.xkbOptions = "altwin:swap_lalt_lwin,ctrl:nocaps";

  services.xserver.displayManager.lightdm.enable = true;

  services.xserver.deviceSection = ''
    Option "TearFree" "true"
  '';

  services.transmission = {
    enable = true;
    openFirewall = true;
    settings.download-dir = "/home/${me}/torrents";
  };

  virtualisation.virtualbox.host.enable = true;

  virtualisation.docker.enable = true;
  environment.systemPackages = [ pkgs.docker-compose ];

  fonts.fonts = with pkgs; [
    anonymousPro
    inconsolata
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups.${me} = { gid = 1000; members = [ me ]; };
  users.users.${me} = {
    isNormalUser = true;
    initialHashedPassword = "";
    extraGroups = [ "keys" "docker" "wheel" "networkmanager" "transmission" "wireshark" ];
  };

}
