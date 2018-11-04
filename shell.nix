{ compiler ? "ghc844" }:

let
  release = (import ./release.nix {inherit compiler;});
  pkgs = release.pkgs;
  scripts = [
    (pkgs.writeScriptBin "rebuild-nix" ''
      #!/usr/bin/env bash
      cd $(${pkgs.git}/bin/git rev-parse --show-toplevel)/nix
      ${pkgs.haskellPackages.cabal2nix}/bin/cabal2nix .. > git-annex.nix
    '')
    (pkgs.writeScriptBin "ghcid-watch" ''
      #!/usr/bin/env bash
      ${pkgs.haskellPackages.ghcid}/bin/ghcid --command 'cabal new-repl all'
    '')
  ];
  shell = release.haskell-packages.shellFor { packages = p: [p.git-annex p.magic]; };
in pkgs.stdenv.lib.overrideDerivation shell (oldAttrs: rec {
  LD_LIBRARY_PATH = (oldAttrs.LD_LIBRARY_PATH or []) ++ [
    "${pkgs.file}/lib/"
  ];
  nativeBuildInputs = (oldAttrs.nativeBuildInputs or []) ++ [
    (pkgs.stdenv.mkDerivation {
      name = "scripts";
      phases = "installPhase";
      installPhase = ''
        mkdir -p $out/bin
      '' + (builtins.concatStringsSep "" (builtins.map (script: ''
        for f in $(ls -d ${script}/bin/*); do ln -s $f $out/bin; done
      '') scripts));
    })
    release.cabal
    pkgs.watchexec
    pkgs.haskellPackages.cabal2nix
  ];
})
