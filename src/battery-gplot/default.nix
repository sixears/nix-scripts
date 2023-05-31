{ nixpkgs ? import <nixpkgs> {} }:

let
  src    = nixpkgs.lib.strings.fileContents ./battery-gplot.hs;
in
nixpkgs.writers.writeHaskellBin "battery-gplot" { } src
