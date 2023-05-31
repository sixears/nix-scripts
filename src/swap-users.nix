{pkgs}: pkgs.writers.writePerlBin "swap-users" {} ''
use 5.32.0;
use strict;
use warnings;

@ARGV = glob("/proc/*/status")
  unless @ARGV;

our %SwapUsers;

for my $fn (@ARGV) {
  if ( open my $fh, '<', $fn ) {
    my %vals;
    while (<$fh>) {
      chomp;
      my ($name,$value) = split /:/, $_, 2;
      $vals{$name} = $value;
    }
    if ( exists $vals{VmSwap} ) {
      (my $swap_kb = $vals{VmSwap}) =~ s/ kB//;
      $SwapUsers{$vals{Pid}} = { name => $vals{Name}, swap_kb => $swap_kb }
    }
    close $fh;
  } else {
    warn "failed to open '$fn' for reading\n";
  }
}

for my $pid (sort { $SwapUsers{$a}{swap_kb} <=> $SwapUsers{$b}{swap_kb} }
                  keys %SwapUsers) {
  my ($name,$swap_kb) = @{$SwapUsers{$pid}}{qw( name swap_kb )};
  printf "%8d\t%s\t%s\n", $pid, $name, $swap_kb;
}
''

# Local Variables:
# mode: perl
# perl-basic-offset: 2
# End:
