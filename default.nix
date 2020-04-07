{ pkgs ? import <nixpkgs> {}, ghc_version ? "ghc8101" }:

with pkgs;

let
  baseDeps = [
    haskell.compiler.${ghc_version}
    cabal-install
    cacert
    curl
    gitMinimal
    zlib
    redis
  ];
in

rec {
  inherit ghc_version;

  # This one is only useful after running `cabal install --installdir=$PWD/bin` so the binaries exist
  runtimeImage = dockerTools.buildLayeredImage {
    name = "maple";
    tag = "latest";
    contents = [
      bash
      coreutils  # gitlab executor needs this :-|
      zlib
      gmp
      libffi
      redis  # for tests
    ];
    config.Env = [
      "LD_LIBRARY_PATH=/lib"
    ];
    config.Entrypoint = ["/bin/sh"];
    config.WorkingDir = "/srv/maple";
    extraCommands = ''
      mkdir -p srv/maple
      cp -a ${./loot} srv/maple/loot
      cp -a ${./test_loot.csv} srv/maple/test_loot.csv
      cp -a ${./bin}/* srv/maple/
    '';
  };

  shell = pkgs.mkShell {
    buildInputs = baseDeps ++ [
      ghcid
    ];
    LIBRARY_PATH = "${pkgs.zlib}/lib";
  };

  client = stdenv.mkDerivation rec {
    name = "maple-client";
    srcs = ./client;
    buildInputs = [
      nodePackages.npm
      nodejs
    ];

    ## Override this to control which components get built:
    build_components = ["comic" "admin" "extension"];

    ## Override these to pass vars to the npm build:
    # MAPLE_ASSET_URL = "/.....";
    # MAPLE_ADMIN_ASSET_URL = "/....";
    # MAPLE_API_SERVER = "/....";
    # MAPLE_ITEM_IMG_URL = "/....";
    ## (see client/comic.js for a list of variables)

    buildPhase = ''
      ${clientShell.shellHook}
      ln -sf $NODE_PATH node_modules
      for c in $build_components; do
        npm run "build:$c"
      done
    '';
    installPhase = ''
      cp -a dist $out
    '';
  };

  clientShell = (callPackage ./client {}).shell;

  clientImage =
    dockerTools.buildLayeredImage {
      name = "maple-client";
      tag = "latest";
      contents = [
        bash
        coreutils
        nodePackages.npm
        nodejs
        (pkgs.writeScriptBin "entrypoint.sh" ''
          #!/bin/bash
          ${clientShell.shellHook}
          exec "$@"
        '')
      ];
      config.Args = ["bash"];
      config.Entrypoint = ["/bin/entrypoint.sh"];
      config.WorkingDir = "/srv/maple/client";
      extraCommands = ''
        mkdir -p srv/maple
        cp -a ${./client} srv/maple/client
        ${clientShell.shellHook}
        chmod +w srv/maple/client
        cd srv/maple/client
        ln -sf $NODE_PATH node_modules
        ${nodePackages.npm}/bin/npm run build
      '';
    };
}
