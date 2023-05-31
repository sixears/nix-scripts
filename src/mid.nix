{ pkgs, midentify, header, ... }: pkgs.writers.writeBashBin "mid" ''

dirname=''${CMD[dirname]}
midentify=${midentify}/bin/midentify
perl=${pkgs.perl}/bin/perl
if [ x-s == "x$1" ]; then
  $midentify "''${@:2}" | $perl -nlE 'say $1 if /^ID_LENGTH=(\d+)/'
else
  $midentify "$@" | $perl -nlE 'printf "\t\t%dx%d\t%2dh%02dm%02s\n",$w,$h,$1/3600,($1%3600)/60,$1%60 if /^ID_LENGTH=(\d+)/;printf "%-64s ", $1 if /^ID_FILENAME=(.*)$/;$w=$1 if /^ID_VIDEO_WIDTH=(\d+)$/;$h=$1 if /^ID_VIDEO_HEIGHT=(\d+)/'
fi
''

# Local Variables:
# mode: sh
# sh-basic-offset: 2
# End:
