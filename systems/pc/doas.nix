{ config, ... }:

{

  security.sudo.enable = false;
  security.doas.enable = true;
  security.doas.extraRules = [
    { users = [ "kpg" ];
      keepEnv = true;
      persist = true;
    }];

}
