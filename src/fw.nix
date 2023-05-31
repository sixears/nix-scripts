{ pkgs, ... }:

pkgs.writers.writeBashBin "fw" ''
exec /run/wrappers/bin/sudo ${pkgs.footswitch}/bin/footswitch -1 -k pageup -2 -k pagedown -3 -k down
''
