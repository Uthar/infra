# infra

This repo contains declarative infrastructure as code configuration of networks of NixOS machines, deployed using NixOps.

## dependencies

You need direnv and nix. Everything else is provided by `.envrc`.

## deploy

To deploy a network `foo`:

```
cd networks/foo
nixops create (first time only)
nixops deploy
```

## binary cache

To avoid lengthy rebuilds, you can use the binary cache `https://cache.galkowski.xyz`. Its public key is `cache.galkowski.xyz-1:8itwpvpPypcmgogbwtWf6+/EOFALY2BIrG0zF8LfMCM=`.

It gzips, so you need either `nix >=2.4` or `nix 2.3` patched with `./overlays/add-gzip-decompression.patch`.
