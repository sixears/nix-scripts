# -*- mode: perl -*-

{ pkgs, ... }:

pkgs.writers.writePerlBin "hb" { libraries = [ pkgs.perlPackages.CGI
                                               pkgs.perlPackages.HTMLTiny
                                             ]; }
''
# Pragma ------------------------------

use 5.30.0;
use strict;
use warnings;

# Utility -----------------------------

use constant HTML        => exists $ENV{GATEWAY_INTERFACE};

use CGI  qw( param );
# use CGI::Pretty qw( param header *html *form *td *Tr center caption b font hr h1 textfield submit Tr th td );
use CGI::Carp;
use File::Spec::Functions qw( catfile );
use FindBin qw( $Bin );
use Getopt::Long qw( GetOptions );
use HTML::Tiny   qw( );

# Constants ---------------------------

use constant MIRAS_LIMIT => 30_000; # in pounds
use constant MIRAS_RATE  => 0;      # %, between 0 & 1

# Globals ------------------------------

my $html = HTML ? HTML::Tiny->new : undef;

# Subrs ----------------------------------------------------------------------

sub max {
  my $x = $_[0];
  ($_ > $x) && ($x = $_)
    for @_[1..$#_];
  return $x;
}

# -------------------------------------

sub repymt {
  my ($LoanAmount, $Term, $Interest) = @_;

  my $Q = 1 + $Interest / 100;


  my $OutstandingAmount = $LoanAmount;
  my $NormalInstallment = (1/12) * ($OutstandingAmount * ($Q ** $Term) *
                                    ($Q - 1) / (($Q ** $Term) - 1));

  my @amounts;
    for (my $i = 1; $i <= $Term; $i++)
    {

	my $InterestRelief = ($OutstandingAmount > MIRAS_LIMIT) ?
	    $Interest / 100 * MIRAS_LIMIT * MIRAS_RATE / 12 :
		$Interest / 100 * $OutstandingAmount * MIRAS_RATE / 12 ;

	my $Installment = $NormalInstallment - $InterestRelief;

	$OutstandingAmount = ($OutstandingAmount * $Q) -
	    ($NormalInstallment * 12);

        push @amounts, [$Installment, $OutstandingAmount];
    }

  return(repayment => +{}, \@amounts, 'Outstanding');
}

# -------------------------------------

sub edwmt {
  my ($LoanAmount, $Term, $Interest) = @_;

  my $InterestRate = $Interest / 100;

  my $endowmentGrowth = 5.4;	# %
  my $endowmentGrowthRate = $endowmentGrowth / 100;
  my $EQ = 1 + $endowmentGrowthRate;

  my $GrossInterestPayment = $LoanAmount * $InterestRate / 12;
  my $MIRASInterestRelief = ($LoanAmount > MIRAS_LIMIT) ?
    ((MIRAS_LIMIT * MIRAS_RATE) * $InterestRate / 12) :
      ($LoanAmount * MIRAS_RATE * $InterestRate / 12);
  my $NetInterestPayment = $GrossInterestPayment - $MIRASInterestRelief;

  my $endowmentPayment =
    $LoanAmount * $endowmentGrowthRate / ($EQ ** $Term - 1) / 12;

  my $AccumAmount = 0;

  my @amounts;
  for (my $i = 1; $i <= $Term; $i++) {
    $AccumAmount = $AccumAmount * $EQ + $endowmentPayment * 12;

    push @amounts, [$NetInterestPayment + $endowmentPayment, $AccumAmount];
  }

  my %keys = ('Gross Interest Payment' => $GrossInterestPayment,
              'MIRAS Interest Relief'  => $MIRASInterestRelief,
              'Net Interest Payment'   => $NetInterestPayment,
              'Endowment Payment'      => $endowmentPayment,
             );

  return('endowment', \%keys,
         \@amounts,
         'Est. Accum. at ' . substr($endowmentGrowth, 0, 4) . '%');
}

# -------------------------------------

sub dump_schedule {
  my ($name, $info, $amounts, $title) = @_;

  my $Result = '';

  unless ( HTML ) {
    $Result .= ucfirst($name) . " Schedule\n" . '=' x length($name) . " ========\n\n";
  }

  $Result .= dump_values($info);

  if ( HTML ) {
    $Result .= <<"END";
<table>
  <tr>
    <td align="center">Year</td>
    <td align="center">Installment</td>
    <td align="center">$title</td>
  </tr>
END
    for (0..$#$amounts) {
      my @x = ($_+1, map sprintf('%4.2f',$_),@{$amounts->[$_]});
      $Result .= <<"END";
  <tr>
    <td align="right">$x[0]</td>
    <td align="right">$x[1]</td>
    <td align="right">$x[2]</td>
  </tr>
END
    }

    $Result .= "</table>\n";
  } else {
    $Result .= "Year  Installment  $title\n";
    $Result .= sprintf "%4d  %11.2f  %11.2f\n", $_+1, @{$amounts->[$_]}
      for 0..$#$amounts;
    $Result .= "\n\n";
  }

  $Result;
}

# -------------------------------------

sub commify ($) {
  (my $text = reverse $_[0]) =~ s/(\d{3})(?=\d)(?!\d*\.)/$1,/g;
  return scalar reverse $text;
}

# -------------------------------------

sub dump_values {
  my ($info) = @_;

  my $Result = '';

  if ( HTML ) {
    $Result .= "<table>\n";
    $Result .= $html->tr($html->td({-align=>'right'}, [$_, ' ', @{$info->{$_}}]))
      for keys %$info;
    $Result .= "</table>\n";
  } else {
    my $length = max map length($_), keys %$info;
    while ( my ($k, $v) = each %$info ) {
      my ($pre,$txt,$post) = @$v;
      $Result .= sprintf "%-${length}s: %s%s%s\n",
                         $k, map +($_ eq '&pound;' ? '£' : $_), @$v;
    }
    $Result .= "\n";
  }

  return $Result;
}

# -------------------------------------

sub table_label_field_ {
  my ($title, $default) = @_;
  my $name = lc ((split ' ', $title)[0]);

  return $html->td([$title,
                    CGI->textfield( -name     => $name
                             , -default  => $default
                             , -size     => 7
                             , -maxlength =>7
                             )
                   ]);
}

# -------------------------------------

sub table_label_field {
  my ($title, $name, $default) = @_;
  return td([$title,
             textfield(-name     => $name,
                       -default  => $default,
                       -size     => 7,
                       -maxlength =>7)
            ]);
}

# -------------------------------------

sub dump_form {
  my ($form_vars) = @_;

  my $Result = '';
  my @table_fields = map $html->tr(table_label_field_($_,$form_vars->{$_}))
                       , sort keys %$form_vars;
  my $submit = CGI->submit(-name => 'calculate');
  my $table = $html->table([@table_fields, $html->tr($html->td($submit))]);
  return $html->form({method => 'GET'},[$html->tag('center',[$table])]);
#  $Result .= start_form(-method => 'GET');
#  $Result .= "<center><table>\n";
#  for (sort keys %$form_vars) {
#    my $name = lc((split ' ', $_)[0]);
#    $Result .= Tr([table_label_field($_, $name, $form_vars->{$_}),
#                  ]);
#  }
#  $Result .= Tr(td(submit(-name => 'calculate')));
#  $Result .= "</table></center>\n";
#  $Result .= end_form;
#  $Result;
}

# -------------------------------------

sub import_css {
  my $Result = '';
  my $css_list = catfile((getpwuid($<))[7], "mortcalc-css");
  if ( -e $css_list ) {
    open my $css, '<', $css_list
      or die "Failed to open $css_list\n";
    local $/ = "\n";
    while ( <$css> ) {
      chomp;
      $Result .= "<!-- \@import url($_); -->\n";
    }
    close $css;
  }
  return $Result;
}

# -------------------------------------

sub import_links {
  my $Result = '';
  my $css_list = catfile((getpwuid($<))[7], "mortcalc-links");
  if ( -e $css_list ) {
    open my $css, '<', $css_list
      or die "Failed to open $css_list\n";
    local $/ = "\n";
    while ( <$css> ) {
      chomp;
      my ($name, $url) = split /\t/, $_, 2;
      $Result .= qq!<a href="$url">$name</a>\n!;
    }
    close $css;
  }
  return $Result;
}

# Main -----------------------------------------------------------------------

my $repayment = 1;
my $endowment = 0;

my %variables;

Getopt::Long::Configure(qw( no_auto_abbrev no_bundling
                            no_getopt_compat gnu_compat
                            no_ignore_case permute
                            prefix_pattern=(--|-)
                          ));

my %default;
$default{amount}   = 100_000;
$default{interest} = 5;
$default{term}     = 25;


if ( HTML ) {
  if ( param() ) {
    for (qw(amount interest term)) {
      my $x = param($_) // '';
      if ( $x =~ /^[ ,_]*\d[\d ,_]*(\.[ ,_\d]*)?$/ ) {
      $x =~ tr/ ,_//d;
      $variables{$_} = $x;
    } else {
        print $html->b($html->tag('font',(+{-color=>'red'},"Bad value for $_: should be a number")));
        print $html->hr;
        $variables{$_} = $default{$_};
      }
    }
  } else {
    @variables{qw(amount interest term)} = (100_000, 5, 25);
  }

} else {
  GetOptions('a=i'   => \$variables{amount},
             't=i'   => \$variables{term},     # in years
             'i=f'   => \$variables{interest}, # in %
             'e'     => \$endowment,
             'r'     => \$repayment,
             'R'     => sub { $repayment = 0 },
            )
    or die "options parsing failed\n";
  die "Must specify $_\n"
    for grep ! defined $variables{$_}, qw( amount term interest );
}

my %parameters = (# "M.I.R.A.S. Limit " => MIRAS_LIMIT,
                  # "M.I.R.A.S. Rate"   => MIRAS_RATE,
#                  "Interest Rate"     => "$variables{interest}%",
#                  "Loan Amount"       => "&pound;$variables{amount}",
#                  "Term"              => "$variables{term} years",
                   "Interest Rate"     => [ '', "$variables{interest}", '%' ]
                 , "Loan Amount"       => [ '&pound;', commify $variables{amount}, '' ]
                 , "Term"              => [ '', "$variables{term}", ' years' ]
                 );

print dump_values(\%parameters)
unless HTML;

unless ( HTML ) {
print dump_schedule(repymt(@variables{qw(amount term interest)}))
  if $repayment;

print dump_schedule(edwmt(@variables{qw(amount term interest)}))
  if $endowment;
}

if ( HTML ) {
  my %replace = (form       => sub {
                   dump_form(+{'Amount (&pound;)'    => $variables{amount},
                               'Term (years)'       => $variables{term},
                               'Interest (percent)' => $variables{interest}
                              });
                 },
                 schedule   => sub {
                   dump_schedule(repymt(@variables{qw(amount
                                                      term
                                                      interest)}))},
                 parameters => sub {
                   dump_values(\%parameters)
                 },
                 css        => \&import_css,
                 links      => \&import_links,
                ),
                 ;

  local $/ = "\n";
  for (<DATA>) {
    s/^( +)__([A-Z]+)__/my$y=$1;my$x=$replace{lc($2)}->();$x=~s!^!$y!gm;$x/e;
    print;
  }
}

__DATA__
Content-Type: text/html; charset=ISO-8859-1

<?xml version="1.0" encoding="iso-8859-1"?>
<!DOCTYPE html
        PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
         "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" lang="en-US">
<head>
  <title>Fluffy Mortgage Calculator</title>
  <style type="text/css" media="screen">
    __CSS__
  </style>
</head>
<body>
  <div id="links">
    __LINKS__
  </div>
  <div id="content">
    <h1>Fluffy Mortgage Calculator</h1>
    <center>
      <table>
        <tr>
          <td>
            __FORM__
          </td>
          <td rowspan="2">
            __SCHEDULE__
          </td>
        </tr>
        <tr>
          <td>
            __PARAMETERS__
          </td>
        </tr>
      </table>
    </center>
  </div>
</body>
</html>
''
