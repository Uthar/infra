{ config, ... } :

{
  nix = {
    binaryCaches = [ "https://cache.galkowski.xyz" ];
    binaryCachePublicKeys = [ "cache.galkowski.xyz-1:8itwpvpPypcmgogbwtWf6+/EOFALY2BIrG0zF8LfMCM=" ];
  };
}
