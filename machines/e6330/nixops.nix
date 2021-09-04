{ ... }:

{

  services.openssh = {
    enable = true;
    listenAddresses = [ { addr = "127.0.0.1"; port = 22; } ];
  };

  users.users.root = {
    openssh.authorizedKeys.keyFiles = [ /home/kpg/.ssh/id_rsa.pub ];
  };

}
