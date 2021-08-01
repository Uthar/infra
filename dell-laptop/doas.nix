{ config, ... }:

{

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

}
