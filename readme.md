# infra

This repo contains configuration of NixOS systems.

## dependencies

Nix

## usage

```
nixos-rebuild --flake . switch
```

## binary cache information

```nix
{
  nix.binaryCachePublicKeys = [ "cache.galkowski.xyz-1:8itwpvpPypcmgogbwtWf6+/EOFALY2BIrG0zF8LfMCM=" ];
  nix.trustedBinaryCaches = [ "https://cache.galkowski.xyz" ];
}
```
