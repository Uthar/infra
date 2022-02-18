# infra

This repo contains Nix config for NixOS systems and NixOps networks and home-manager environments.

## dependencies

Nix

## usage

NixOS:

```
nixos-rebuild --flake . switch
```

Home Manager:

```
home-manager switch --flake .
```

NixOps:

```
cd networks/foo
nixops deploy
```

## binary cache information

```nix
{
  nix.binaryCachePublicKeys = [ "cache.galkowski.xyz-1:8itwpvpPypcmgogbwtWf6+/EOFALY2BIrG0zF8LfMCM=" ];
  nix.trustedBinaryCaches = [ "https://cache.galkowski.xyz" ];
}
```
