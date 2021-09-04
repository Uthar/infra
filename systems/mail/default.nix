
{ config, pkgs, lib, nodes, ... }:

let

  domain = config.networking.domain;

in {

  mailserver = {
    enable = true;
    fqdn = "mail.${domain}";
    domains = [ domain ];
    loginAccounts = {
      "k@${domain}" = {
        # mkpasswd -m sha-512 "super secret password" > /hashed/password/file/location
        hashedPasswordFile = "/run/mailserverKeys/kMailAccount";
      };
    };

    certificateScheme = 2;

    enableImap = false;
    enableImapSsl = true;
    enableSubmission = false;
    enableSubmissionSsl = true;

    enableManageSieve = true;
  };
}
