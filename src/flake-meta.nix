{ pkgs ? import <nixpkgs> {} }: pkgs.writers.writePerlBin "flake-meta" { libraries = [ pkgs.perlPackages.JSON ]; } ''
# pipe the flake.lock into this to see packages with conflicting versions

use 5.20.0;
use strict;
use warnings;

use JSON          qw( decode_json );
use Data::Dumper  qw( Dumper );

undef $/;
my $x=<STDIN>;
my $y=decode_json $x
  or die "failed to decode json";

# original inputs to the flake
my @inputs;
# map from repo to map from narHash to { rev, ref, [nodes] }
my %input;
# map from node name to map of { repo, narHash, rev, ref }
my %node;
# map from node to pkgs that use that node
my %users;
# map from node to version ref
my %vers;

# this works with the output of nix flake metadata --json
# while (my ($node,$v) = each %{$y->{locks}->{nodes}}) {

# this works by reading flake.lock - and as such is much much faster
NODE:
while (my ($node,$v) = each %{$y->{nodes}}) {
  if ( 'root' eq $node ) {
    @inputs = values %{$v->{inputs}};
  }
  if ( 'root' ne $node ) {
    defined $v->{original}->{type} or die "no type!: " . Dumper { node=> $node, v=>$v };

    if ( 'path' eq $v->{original}->{type} ) {
      warn "Path found ($node): $v->{original}->{path}\n";
      next NODE;
    }
    for my $k (qw( type owner repo )) {
      if ( ! exists $v->{original}->{$k} ) {
        die "missing key $k: " . Dumper { node=> $node, v=>$v };
      }
    }
    my $repo = sprintf "%s:%s/%s", @{$v->{original}}{qw( type owner repo )};
    my $narHash = $v->{locked}->{narHash};
    my %v = ( narHash => $v->{locked}->{narHash}
            , rev     => $v->{locked}->{rev}
            , ref     => $v->{original}->{ref}
            );
    $input{$repo}{$narHash}{rev} = $v->{locked}->{rev};
    $input{$repo}{$narHash}{ref} = $v->{original}->{ref};
    push @{$input{$repo}{$narHash}{nodes}}, $node;

    $node{$node}   = { %v, repo => $repo };

    for my $i (values %{$v->{inputs}}) {
      push @{$users{$i}}, $node;
    }

    $vers{$node} = $v->{original}->{ref};
  }
}

# say Dumper \@inputs;
# say Dumper \%input;
# say Dumper \%node;
# say Dumper \%users;

sub xx {
  my ($name,$i) = @_;
  printf '-' x (2*$i);
  say $name;
  for my $u (@{$users{$name}}) {
    xx($u,$i+1);
  }
}

sub trail1 {
  my ($name) = @_;
  (my $n = $name) =~ s!_\d+$!!;
  $n = "$n-$vers{$name}";
  if ( 0 != (my @users = @{$users{$name} || []}) ) {
    my @trail = trail1($users[0]);
    return $n, @trail;
  } else {
    return $n;
  }
}

for my $u (@inputs) {
#  say "u: $u";
#  xx($u,2);
}

for my $repo (keys %input) {
  my @narHashes = keys %{$input{$repo}};
  if ( 1 != @narHashes ) {
    say "$repo";
    for my $hash (@narHashes) {
      my @users = @{$input{$repo}{$hash}{nodes}};
      printf "  %3d\t%40s\t%s\n", (0+@users), $hash, join '->', trail1($users[0]);
##      say "  $hash";
##      say ">", 0+@{$input{$repo}{$hash}{nodes}};
##      say "+", $input{$repo}{$hash}{nodes}[0];
##      xx($input{$repo}{$hash}{nodes}[0],2);
##      say join "->", trail1 $input{$repo}{$hash}{nodes}[0];
    }
  }
}
''

# Local Variables:
# mode: CPerl
# End:
