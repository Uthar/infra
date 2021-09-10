with import ./. {};

mkShell {
  buildInputs = [ nix nixopsWithPlugins ];
}
