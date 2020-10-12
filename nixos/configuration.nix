# Edit this configuration file to define what should be installed on
# your system.  Help is available in the configuration.nix(5) man page
# and in the NixOS manual (accessible by running ‘nixos-help’).

{ config, lib, pkgs, ... }:

let
  me = "kpg";
in
{

  ### Nix
  nix.maxJobs = lib.mkDefault 4;
  nix.autoOptimiseStore = true;
  nix.extraOptions = let mb = n: toString (n * 1024 * 1024); in ''
    min-free = ${mb 100}
    max-free = ${mb 500}
  '';

  # Copy configuration to store. Make sure not to import anything from ../ in this file.
  system.extraSystemBuilderCmds = ''
    ln -s \
    '${builtins.dirOf (lib.maybeEnv "NIXOS_CONFIG" <nixos-config>)}' \
    "$out/current-configuration"
  '';



  ### Boot
  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
    enableCryptodisk = true;
    extraInitrd = "/root/extra-initramfs.cpio.gz";
  };

 # TODO: add binary cache for this shit
 #boot.kernelPackages = with pkgs;
 #  recurseIntoAttrs (linuxPackagesFor (linux-libre.override {
 #    scripts = fetchsvn {
 #      url = "https://www.fsfla.org/svn/fsfla/software/linux-libre/releases/branches/";
 #      rev = "17624";
 #      sha256 = "0gs3mpiffny408l9kdrxpj48axarfb2fxvcw4w8zsz5wr7yig0n2";
 #    };
 #  }));

  boot.kernelPackages = (import (fetchTarball {
    url = https://github.com/NixOS/nixpkgs/archive/d447429cc2407f7e76b09299439991d777914f4e.tar.gz;
    sha256 = "1dhn94g6g2ay8vi3ihjl0cfb7xmazs671610c8xx04ss4ph69pzy";
  }) {}).linuxPackages-libre;

  boot.initrd.availableKernelModules = [ "ata_piix" "ohci_pci" "sd_mod" "sr_mod" "dm-snapshot" ];
  boot.kernelParams = ["root=/dev/disk/by-label/crypto-root"];

  boot.initrd.luks.devices."crypto-root" = {
    device = "/dev/sda1";
    keyFile = "/root/key";
  };

  fileSystems."/" = { 
    device = "/dev/disk/by-label/crypto-root";
    fsType = "ext4";
  };



  ### Net
  networking.hostName = "nixos";
  networking.interfaces.enp0s3.useDHCP = true;
  networking.networkmanager.enable = true;
  networking.networkmanager.dns = "none";
  networking.nameservers = [ "::1" ];
  networking.resolvconf.useLocalResolver = true;

  services.dnscrypt-proxy2 = {
    enable = true;
    settings = {
      ipv6_servers = true;
      require_dnssec = true;
      require_nolog = true;
      
      sources.public-resolvers = {
        urls = [
          "https://raw.githubusercontent.com/DNSCrypt/dnscrypt-resolvers/master/v3/public-resolvers.md"
          "https://download.dnscrypt.info/resolvers-list/v3/public-resolvers.md"
        ];
        cache_file = "/var/lib/dnscrypt-proxy2/public-resolvers.md";
        minisign_key = "RWQf6LRCGA9i53mlYecO4IzT51TGPpvWucNSCh1CBM0QTaLn73Y7GFO3";
      };
      
      # You can choose a specific set of servers from https://github.com/DNSCrypt/dnscrypt-resolvers/blob/master/v2/public-resolvers.md
      # server_names = [ ... ];
    };
  };
  
  systemd.services.dnscrypt-proxy2.serviceConfig.StateDirectory = "dnscrypt-proxy2";

  networking.firewall.enable = true;
  networking.firewall.allowedTCPPorts = [ 5555 ];
  networking.firewall.allowedUDPPorts = [ 5555 ];
  #networking.firewall.allowedTCPPortRanges = [];
  #networking.firewall.allowedUDPPortRanges = [];



  ### Time and date
  console = {
    font = "Lat2-Terminus16";
    useXkbConfig = true;
  };

  i18n.defaultLocale = "en_US.UTF-8";
  time.timeZone = "Europe/Amsterdam";



  ### Environment
  # List packages installed in system profile. To search, run:
  # $ nix search wget
  environment.systemPackages = (import ./packages.nix pkgs);

  environment.variables = {
    TERMINAL = "st";
    # Setting TERM here causes timeout in Emacs (tramp): M-x find-file ssh:
    #TERM = "xterm-256color";
    BROWSER = "firefox";
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
    f = "fossil";
    g = "git";
    e = "emacsclient -c";
    x = "exit";
    r = "ranger";
    nuke = "shred -zu";
  };



  ### Programs
  # Some programs need SUID wrappers, can be configured further or are
  # started in user sessions.
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
    pinentryFlavor = "gnome3";
  };

  programs.nm-applet.enable = true;



  ### Services
  services.openssh.enable = true;
  #services.openssh.ports = [ 1338 ];
  #services.openssh.extraConfig = ''
  #  PasswordAuthentication no
  #'';

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
    package = (import ./emacs.nix {});
  };

  services.devmon.enable = true;
  security.wrappers.udevil.source = "${pkgs.udevil}/bin/udevil";

  services.logind.extraConfig = ''
    HandlePowerKey=ignore
  '';



  ### X11
  services.xserver.enable = true;
  services.xserver.layout = "pl";
  services.xserver.xkbOptions = "eurosign:e,altwin:swap_lalt_lwin,ctrl:nocaps";

  # Enable touchpad support.
  # libinput.enable = true;

  services.xserver.displayManager.lightdm.enable = true;
  services.xserver.displayManager.sessionCommands = ''
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 23 = Alt_L Meta_L Alt_L Meta_L"
    ${pkgs.xorg.xmodmap}/bin/xmodmap -e "keycode 222 = Tab"
    ${pkgs.xcape}/bin/xcape -e "Alt_L=Tab"
  '';

  services.xserver.windowManager.i3.enable = true;
  services.xserver.windowManager.i3.package = pkgs.i3-gaps;
  services.xserver.windowManager.i3.configFile = ./i3.conf;



  ### System
  # Enable sound.
  sound.enable = true;
  hardware.pulseaudio.enable = true;

  fonts.fonts = with pkgs; [
    anonymousPro
  ];

  # Define a user account. Don't forget to set a password with ‘passwd’.
  users.groups.${me} = { gid = 1000; members = [ me ]; };
  users.users.${me} = {
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
  system.stateVersion = "20.03"; # Did you read the comment?

}

