{pkgs?import<nixpkgs>{}}: pkgs.writers.writePerlBin "flake-set-versions" {} ''

use strict;
use warnings;
use 5.20.0;

my $fn = shift @ARGV;
my %settings = @ARGV;

open my $fh, '<', $fn or die;
my $done=0;
while (<$fh>) {
  chomp;
  if ( m!^(\s+)([\w-]+)\.url(\s+)= ("?)github:sixears/([-\w]+)/r([\d.]+)\4;\s*$!
       && defined $settings{$5} && $6 ne $settings{$5} ) {
   printf qq{$1$2.url$3= github:sixears/$5/r%s;\n}, $settings{$5};
   $done=1;
 } else {
   say;
 }
}

exit ! $done;
''

# Local Variables:
# mode: CPerl
# End:
