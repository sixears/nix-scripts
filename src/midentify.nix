{ pkgs, ... }: pkgs.writers.writeBashBin "midentify" ''

for i in "$@"; do
  ${pkgs.mplayer}/bin/mplayer -vo null -ao null -frames 0 -identify "$i" |& \
    ${pkgs.gnugrep}/bin/grep ^ID_
done
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
