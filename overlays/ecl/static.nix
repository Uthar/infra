{ ecl }:

ecl.overrideAttrs(o: {
  configureFlags = o.configureFlags ++ [ "--disable-shared" ];
})
