{nixpkgs?import<nixpkgs>{}}: nixpkgs.writers.writePerlBin "git-config-url" {} ''
use 5.32.0;
use strict;
use warnings;

while (<>) {
  chomp;
  say $1 if /^\[remote "origin"\]$/.../^\[/ and /^\s*url\s*=\s*(\S+)/;
}
''
  
# Local Variables:
# mode: perl
# perl-basic-offset: 2
# End:
