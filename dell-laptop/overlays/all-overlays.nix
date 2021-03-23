[
  (self: super: {

    udiskie = super.udiskie.overridePythonAttrs (old: rec {
      version = "2.3.3";
      src = super.fetchFromGitHub {
        owner = "coldfix";
        repo = "udiskie";
        rev = "v${version}";
        sha256 = "08y3swazx0w4dagjhzzi7kzcdpz02xnca1jm7nic2xz3prq41qrr";
      };
    });

    tabbed = super.tabbed.overrideAttrs (old: rec {
      src = super.fetchFromGitHub {
        owner = "uthar";
        repo = "tabbed";
        rev = "60fd4a0c6afe9f627923cc60c4476ac1a7c2eb7f";
        sha256 = "0i0f55zg3m4m3iv99i8grykg6i0xq7yimiw73xddd9j5p1bnh108";
      };
    });

  })
]
