# source .envrc first, or just use direnv
with import <nixpkgs> {};

mkShell {
  buildInputs = [ nix nixopsWithPlugins ];
}
