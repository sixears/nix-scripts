# -*- mode: perl -*-

{ pkgs, ... }:

let ps = with pkgs.perlPackages; [ IPCSystemSimple Readonly ];
 in pkgs.writers.writePerlBin "dezip" { libraries = ps; }

''
use strict;
use warnings;

use autodie              qw( unlink );
use File::Basename       qw( basename );
use Getopt::Long         qw( GetOptions :config bundling no_ignore_case
                                                prefix_pattern=--|-     );
use IPC::System::Simple  qw( systemx );
use Readonly             qw( Readonly );

Readonly my $UNZIP  => '${pkgs.unzip}/bin/unzip';
Readonly my $GTHUMB => '${pkgs.gthumb}/bin/gthumb';

my ($GQView, $CurDir) = (0) x 2;
GetOptions( 'g'        => \$GThumb
          , 'curdir|c' => \$CurDir
          )
  or die "options parsing failed\n";

for my $fn (@ARGV) {
  my $b = basename $fn, '.zip';
  my @cmd = ($UNZIP);
  push @cmd, -d => $b
    unless $CurDir;
  push @cmd, $fn;
  systemx @cmd;
  unlink $fn;
  if ( $GThumb ) {
    my @cmd2 = ($GThumb, $CurDir ? '.' : $b);
    systemx @cmd2;
  }
}
''
