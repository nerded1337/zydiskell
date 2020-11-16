with (import (builtins.fetchGit {
      name = "nixos-20.09";
      url = https://github.com/nixos/nixpkgs/;
      ref = "refs/heads/nixos-20.09";
    }) {});

haskell.lib.buildStackProject {
    name = "myEnv";
    ghc = haskell.compiler.ghc8102;
    buildInputs = [];
}
