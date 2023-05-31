{ pkgs ? import <nixpkgs> {}, minfo }:

let
  header = import ../header.nix   { inherit pkgs; };
  src    = import ./music-tag.nix { inherit pkgs header minfo; };
in
  pkgs.writers.writeBashBin "music-tag" src
