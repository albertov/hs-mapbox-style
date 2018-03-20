{ pkgsPath ? <nixpkgs> }:
(import pkgsPath {}).haskellPackages.callCabal2nix "shell" ./. {}
