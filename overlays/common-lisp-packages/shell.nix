with import <nixpkgs> {};

mkShell {
  buildInputs = [
    (sbclWithPackages (ps: with ps; [ str dexador cl-ppcre cl-sqlite ]))
  ];
}
