{
  description = "my project description";


  inputs.nixops.url = "github:NixOS/nixops";

  outputs = { self, nixops }: {
          defaultPackage.x86_64-linux = nixops.defaultPackage.x86_64-linux;
  };
}
