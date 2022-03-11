with import ./. {};

mkShell {
  buildInputs = [ ansible ];
}
