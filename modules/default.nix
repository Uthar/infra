# This file exports all the modules from each subdirectory
#
# This is pulled up in NIXOS_EXTRA_MODULE_PATH and propagated through
# all networks. Don't have to duplicate the `import`s in every network
# this way.

{ ... }:
{
  imports =
    [ ./bcache
      ./fossil-server
      ./guix
      ./nixos-mailserver
      ./selenium
      ./happet
    ];
}
