{ config, ... } :

{
  nix = {
    binaryCachePublicKeys = [ "cache.galkowski.xyz-1:8itwpvpPypcmgogbwtWf6+/EOFALY2BIrG0zF8LfMCM=" ];
    trustedBinaryCaches = [ "https://cache.galkowski.xyz" ];
  };
}
