let default_ghc_version = (import ./default.nix {}).ghc_version; in

{ ghc_version ? default_ghc_version }:

(import ./default.nix { inherit ghc_version; }).shell
