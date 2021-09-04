{ config, pkgs, ...} :

{

  nix.extraOptions = ''
    keep-outputs = true
    keep-derivations = true
  '';

  environment.pathsToLink = [
    "/share/nix-direnv"
  ];

  programs.bash.interactiveShellInit = ''
    eval "$(${pkgs.direnv}/bin/direnv hook bash)"
  '';

  environment.systemPackages = with pkgs;
    [ direnv
      nix-direnv
    ];

}
