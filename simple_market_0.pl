#!/usr/bin/perl
require 5.008;
our $VERSION = "1.05"; #Time-stamp: <2012-12-25T10:48:50Z>

## License:
##
##   I in a provincial state made this program intended to be public-domain. 
##   But it might be better for you like me to treat this program as such 
##   under the BSD-License or under the Artistic License.
##
## Author's Link:
##
##   http://jrf.cocolog-nifty.com/society/2011/01/post.html
##   (The page is in Japanese.)
##

# BEGIN { srand(0); }

use strict;
use warnings;
use utf8; # Japanese

BEGIN {
  my @mypackage = qw(
		    Main
		    Main::_Simple
		    Main::_Player
		    Main::_Commodity
		    Main::_LuxuryCommodity
		    Main::_EssentialCommodity
		    Main::_Tactics
		    Main::_DumperTactics
		    Main::_DesireGreedConvertTactics
		    Main::_Market
		    Math::Aux
		 );
  for my $p (@mypackage) {
    eval <<"EOM"; # eval the string til "End Of Macro".
    {
      package $p;
      use POSIX;
      use Math::Trig; # for pi
      use Storable qw(dclone);
      use Data::Dumper;
      use Carp;
      use Encode;
    }
EOM
    die $@ if $@;
  }

  my @myfunction = qw(
		       Math::Aux::normal_rand
		       Math::Aux::powered_normal_rand
		       Math::Aux::pseudo_powered_normal_rand
		       Math::Aux::variance_for_mean_of_pnr
		       Math::Aux::gamma
		       Math::Aux::ngammainc
		       Math::Aux::sgn
		    );
  {
    no strict;
    foreach my $p (@mypackage) {
      for my $f (@myfunction) {
	my @f = split("::", $f);
	*{$p . "::" . $f[$#f]} = \&{$f};
      }
    }
  }
}


package Main;

use Pod::Usage;
use Getopt::Long;

our $DEBUG = 1;

our $CONSOLE_ENCODING = "utf8";
our $SYS_ENCODING = "utf8";
if ($^O =~ /^cygwin/i && `uname -r` =~ /^1\.[0-6]\.|^0/) {
  $CONSOLE_ENCODING = "cp932";
  $SYS_ENCODING = "cp932";
}
if ($CONSOLE_ENCODING =~ /jisx0213|jis2k/i) {
  eval {require Encode::JIS2K};
  $CONSOLE_ENCODING =~ s/jisx0213|jis2k/jis/i if $@;
}
our $FILENAME_ENCODER = Encode::find_encoding($SYS_ENCODING) or die;
our $CONSOLE_ENCODER = Encode::find_encoding($CONSOLE_ENCODING) or die;

our $TERM = 10;
our $PLAYERS = 5;
our $ESSENTIALS = 3;
our $LUXURIES = 2;
our $INITIAL_ASSET = 5;
our $INITIAL_LIABILITY = 10;
our $SRAND;
our $USE_PPNR = 0;
our $Pow = 0.5;
our @TACTICS;

BEGIN {
  $ENV{"PERLDOC"} = "" if ! exists $ENV{"PERLDOC"};
  $ENV{"PERLDOC"} .= " " if $ENV{"PERLDOC"} ne "";
  $ENV{"PERLDOC"} .= "-wcenter:'Simple Economy Simulation'";
}

Getopt::Long::Configure("bundling", "auto_version");
GetOptions(
	   "debug|d:1" => \$DEBUG,
	   "srand:0" => \$SRAND,

	   "term=i" => \$TERM,
	   "player=i" => \$PLAYERS,
	   "essential=i" => \$ESSENTIALS,
	   "luxury=i" => \$LUXURIES,
	   "initial-asset=i" => \$INITIAL_ASSET,
	   "initial-liability=i" => \$INITIAL_LIABILITY,

	   "ppnr:1" => \$USE_PPNR, ## OBSOLETE

	   "tactics-desire-greed-convert:s" => sub {
	     push(@TACTICS, ["Main::_DesireGreedConvertTactics", 
			     split(/,/, $_[1])]);
	   },
	   "tactics-dumper:s" => sub {
	     push(@TACTICS, ["Main::_DumperTactics", 
			     split(/,/, $_[1])]);
	   },

	   "filename-encoding|filename_encoding=s" => sub {
	     my ($obj, $enc) = @_;
	     $FILENAME_ENCODER = Encode::find_encoding($enc)
	       || $FILENAME_ENCODER;
	   },
	   "console-encoding|console_encoding=s" => sub {
	     my ($obj, $enc) = @_;
	     $CONSOLE_ENCODER = Encode::find_encoding($enc)
	       || $CONSOLE_ENCODER;
	   },
	   "man" => sub {pod2usage(-verbose => 2)},
	   "h|?" => sub {pod2usage(-verbose => 0, -output=>\*STDOUT, 
				   -exitval => 1)},
	   "help" => sub {pod2usage(1)},
	  ) or pod2usage(-verbose => 0);

{
  package Main::_Simple;

  my %template = ("Main::_Simple" => {});

  sub extend_template {
    my $class = shift;
    my @hash = @_;
    if (exists $template{$class}) {
      $template{$class} = {%{$template{$class}}, @hash} if @hash;
    } else {
      $template{$class} = 
	{(map {
	        $_->extend_template() if ! exists $template{$_};
	        %{$template{$_}};
	      } (eval '@{' . $class . '::ISA}')),
	 @hash
	};
    }
    return $template{$class};
  }

  sub get_template {
    my $class = shift;
    return $class->extend_template();
  }

  sub new {
    my $class = shift;
    my $obj = dclone($class->get_template());
    bless $obj, $class;
    return $obj;
  }
}

{
  package Main::_Player;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     id => undef,

     desire => {}, # current desire for luxury commodities.

     greed => {}, # current greed for essential commodities.

     possession => {}, # possession of annual essential production.
     		       # hash of cid => {quota => num、term => num}.

     asset => 0.0, # value of asset

     liability => 0.0, # value of liability

     current_income_essential => {},
     current_income_luxury => {},
     current_consumption_essential => {},
     current_consumption_luxury => {},
    );

  our $players = 0;

  sub new {
    my $class = shift;
    my $obj = $class->SUPER::new(@_);
    $obj->{id} = sprintf("P%03d", ++$players);

    return $obj;
  }
}

{
  package Main::_Commodity;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     id => undef,

     type => undef,
    );

  our $commodities = 0;

  sub new {
    my $class = shift;
    my $obj = $class->SUPER::new(@_);
    $obj->{id} = sprintf("C%03d", ++$commodities);

    return $obj;
  }
}

{
  package Main::_LuxuryCommodity;
  use base qw(Main::_Commodity);

  __PACKAGE__->extend_template
    (
     type => "luxury",
     difficulty => 0.5,
     simplicity => 0.7,
    );

  sub random_produce {
    my $self = shift;
    my ($asset) = @_;
    return 0 if rand(1) < $self->{difficulty};
#    my $v = variance_for_mean_of_pnr(sqrt($asset), $Pow);
    my $v = variance_for_mean_of_pnr(log(1 + $asset), $Pow);
    return powered_normal_rand(0, $v, $Pow);
  }

  sub random_desire {
    my $self = shift;
    my ($asset, $init_price, $tendency) = @_;

    return normal_rand($tendency * $init_price, sqrt($asset));
#    return normal_rand($tendency * $init_price, log(1 + $asset));
#    return normal_rand(log(1 + $tendency) * $init_price, log(1 + $asset));
#    return powered_normal_rand(log(1 + $tendency) * $init_price, $asset, 0.5);
  }
}

{
  package Main::_EssentialCommodity;
  use base qw(Main::_Commodity);

  __PACKAGE__->extend_template
    (
     type => "essential",
     need => 10,
    );

  our $ETERMMAX = 20;

  sub random_produce {
    my $self = shift;
    my ($quota) = @_;

    if ($USE_PPNR) {
      return pseudo_powered_normal_rand($quota, $Pow);
    } else {
      my $v = variance_for_mean_of_pnr($quota, $Pow);
      return powered_normal_rand(0, $v, $Pow);
    }
  }

  sub random_attach {
    my $self = shift;
    my ($newquota, $players) = @_;

    while ($newquota--) {
      my $newterm = rand($ETERMMAX);
      my @p = keys %$players;
      my $pid = $p[int(rand(scalar @p))];
      if (exists $players->{$pid}->{possession}->{$self->{id}}) {
	my $quota = $players->{$pid}->{possession}->{$self->{id}}->{quota};
	my $term = $players->{$pid}->{possession}->{$self->{id}}->{term};
	$newterm = $term + ($newterm / (1 + $quota));
	$players->{$pid}->{possession}->{$self->{id}}->{term} = $newterm;
	$players->{$pid}->{possession}->{$self->{id}}->{quota}++;
      } else {
	$players->{$pid}->{possession}->{$self->{id}} = {};
	$players->{$pid}->{possession}->{$self->{id}}->{quota} = 1;
	$players->{$pid}->{possession}->{$self->{id}}->{term} = $newterm;
      }
    }
  }
}

{
  package Main::_Tactics;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     id => undef,

     type => undef,
    );

  our $num_of_tactics = 0;

  sub new {
    my $class = shift;
    my $obj = $class->SUPER::new(@_);
    $obj->{id} = sprintf("T%03d", ++$num_of_tactics);

    return $obj;
  }

  sub preloop {}
  sub preterm {}
  sub preproduce {}
  sub predemand_essential {}
  sub preconsume_essential {}
  sub predemand_luxury {}
  sub preconsume_luxury {}
  sub preattach {}
  sub precleanup {}
  sub postloop {}
}

{
  package Main::_DumperTactics;
  use base qw(Main::_Tactics);

  __PACKAGE__->extend_template
    (
     type => "dumper",
    );

  $Data::Dumper::Sortkeys = 1;
  $Data::Dumper::Indent = 1;
  $Data::Dumper::Useqq = 1;
  $Data::Dumper::Quotekeys = 0;
  $Data::Dumper::Terse = 1;

  sub preloop {
    my $self = shift;
    my ($market) = @_;

    print sprintf("TERM%03d:\n", $market->{term});
    print "\$market = ";
    print Dumper $market;
    print "\n";
  }

  sub precleanup {
    my $self = shift;
    my ($market) = @_;

    print sprintf("TERM%03d:\n", $market->{term});
    print "\$market = ";
    print Dumper $market;
    print "\n";
  }
}

# {
#   package Main::_DesireGreedConvertTactics0;
#   use base qw(Main::_Tactics);

#   __PACKAGE__->extend_template
#     (
#      type => "desire_greed_convert",
#     );

#   sub preloop {
#     my $self = shift;
#     my ($market) = @_;
#     $market->{tactics_data}->{$self->{id}} = {}
#       if ! exists $market->{tactics_data}->{$self->{id}};
#     $market->{tactics_data}->{$self->{id}}->{name} = $self->{type};
#   }

#   sub preconsume_essential {
#     my $self = shift;
#     my ($market) = @_;

#     my $owe;
#     $owe = $market->{tactics_data}->{$self->{id}}->{owe}
#       if exists $market->{tactics_data}->{$self->{id}}->{owe};
#     return if ! defined $owe;
#     my %payed;

#     foreach my $cid (keys %{$market->{current_essential}}) {
#       my $prodmap = $market->{current_essential}->{$cid};
#       my ($buyer_price, $seller_price) 
# 	= $market->match_greed($cid, $prodmap);
#       my @producer = (keys %$prodmap);
#       @producer = sort {$market->{player}->{$a}->{greed}->{$cid}
# 			  <=> $market->{player}->{$b}->{greed}->{$cid}}
# 	@producer;
#       splice(@producer, int((scalar(@producer) + 1) / 2));
#       foreach my $pid (keys %$seller_price) {
# 	if (exists $owe->{$pid}) {
# 	  foreach my $to (keys %{$owe->{$pid}}) {
# 	    if ((grep {$_ eq $pid} @producer)
# 		&& ! exists $payed{"$to,$pid"}
# 		&& ! exists $payed{"$cid,$pid"}) {
# 	      print STDERR "discount prod\n" if $DEBUG;
# 	      my $right = $owe->{$pid}->{$to};
# 	      if ($right < 1) {
# 		$prodmap->{$pid} *= 0.9 * (1 - $right);
# 		delete $owe->{$pid}->{$to};
# 	      } else {
# 		$prodmap->{$pid} *= 0.9;
# 		$owe->{$pid}->{$to} -= 1;
# 	      }
# 	      $payed{"$to,$pid"} = 1;
# 	      $payed{"$cid,$pid"} = 1;
# 	    }
# 	  }
# 	}
#       }
#     }
#     return;
#   }

#   sub preconsume_luxury {
#     my $self = shift;
#     my ($market) = @_;

#     my $owe = {};
#     $owe = $market->{tactics_data}->{$self->{id}}->{owe}
#       if exists $market->{tactics_data}->{$self->{id}}->{owe};

#     foreach my $cid (keys %{$market->{current_luxury}}) {
#       my $pricemap = $market->{current_luxury}->{$cid};
#       my @match = $market->match_desire($cid, $pricemap);
#       next if ! @match;
#       my ($maxprice, $maxd, $maxp) = @{$match[$#match]};
#       my @overs = grep {$maxprice <= $pricemap->{$_} && $maxp ne $_}
# 	(keys %$pricemap);
#       my $newpricemap = dclone($pricemap);
#       foreach my $p (@overs) {
# 	$newpricemap->{$p} = $newpricemap->{$p} * 0.9;
#       }
#       my @newmatch = $market->match_desire($cid, $newpricemap);
#       next if @newmatch <= @match;

#       print STDERR "convert!\n" if $DEBUG;

#       my %seller;
#       my %buyer;
#       foreach my $m (@newmatch) {
# 	my ($price, $d, $p) = @$m;
# 	$seller{$p} = 1;
# 	$buyer{$d} = 1;
#       }
#       my @looser;
#       foreach my $m (@match) {
# 	my ($price, $d, $p) = @$m;
# 	if (! exists $seller{$p}) {
# 	  push(@looser, $p);
# 	} else {
# 	  delete $seller{$p};
# 	}
# 	delete $buyer{$d};
#       }
#       $newpricemap = dclone($pricemap);
#       foreach my $p (keys %seller) {
# 	$newpricemap->{$p} = $newpricemap->{$p} * 0.9;
#       }
#       $market->{current_luxury}->{$cid} = $newpricemap;

#       my @seller = keys %seller;
#       my @winner = keys %buyer;
#       my $owe_to_seller = scalar(keys %buyer) / scalar(keys %seller);
#       my $owe_to_looser = scalar(@looser) / scalar(keys %seller);

#       foreach my $to (keys %seller) {
# 	foreach my $p (keys %buyer) {
# 	  $owe->{$p} = {} if ! exists $owe->{$p};
# 	  $owe->{$p}->{$to} = 0 if ! exists $owe->{$p}->{$to};
# 	  $owe->{$p}->{$to} += $owe_to_seller;
# 	}
#       }

#       foreach my $to (@looser) {
# 	foreach my $p (keys %seller) {
# 	  $owe->{$p} = {} if ! exists $owe->{$p};
# 	  $owe->{$p}->{$to} = 0 if ! exists $owe->{$p}->{$to};
# 	  $owe->{$p}->{$to} += $owe_to_looser;
# 	}
#       }
#     }

#     $market->{tactics_data}->{$self->{id}}->{owe} = $owe;

#     return;
#   }
# }

{
  package Main::_DesireGreedConvertTactics;
  use base qw(Main::_Tactics);

  __PACKAGE__->extend_template
    (
     type => "desire_greed_convert",

     essential_convert_unit => 0.1,
    );

  sub preloop {
    my $self = shift;
    my ($market) = @_;
    $market->{tactics_data}->{$self->{id}} = {}
      if ! exists $market->{tactics_data}->{$self->{id}};
    $market->{tactics_data}->{$self->{id}}->{name} = $self->{type};
  }

  sub preconsume_essential {
    my $self = shift;
    my ($market) = @_;

    my $owe;
    $owe = $market->{tactics_data}->{$self->{id}}->{owe}
      if exists $market->{tactics_data}->{$self->{id}}->{owe};
    return if ! defined $owe;

    sub pay_owe {
      my ($self, $prodmap, $pid, $to, $ref_to_right) = @_;

      my $right = $$ref_to_right;

      if ($right * $self->{essential_convert_unit}
	  < $prodmap->{$pid}) {
	$prodmap->{$pid} -= $right * $self->{essential_convert_unit};
	print STDERR "pay owe $pid to $to : $right all\n" if $DEBUG;
	$right = 0;
      } else {
	print STDERR "pay owe $pid to $to : $right partially\n" if $DEBUG;
	$right -= $prodmap->{$pid} / $self->{essential_convert_unit};
	$prodmap->{$pid} = 0;
      }

      $$ref_to_right = $right;
    }

    foreach my $cid (keys %{$market->{current_essential}}) {
      my $prodmap = $market->{current_essential}->{$cid};
      my ($buyer_price, $seller_price) 
	= $market->match_greed($cid, $prodmap);

      foreach my $pid (keys %$owe) {
	foreach my $to (keys %{$owe->{$pid}}) {
	  if (exists $seller_price->{$pid} &&
	      exists $seller_price->{$to}) {
	    next if $market->{player}->{$to}->{greed}->{$cid}
	      <= $market->{player}->{$pid}->{greed}->{$cid};
	    print STDERR "discount prod by seller\n" if $DEBUG;
	    pay_owe($self, $prodmap, $pid, $to, \$owe->{$pid}->{$to});
	    delete $owe->{$pid}->{$to} if $owe->{$pid}->{$to} == 0.0;
	  }
	}
      }

      my @producer = (keys %$prodmap);
      @producer = sort {$market->{player}->{$a}->{greed}->{$cid}
			  <=> $market->{player}->{$b}->{greed}->{$cid}}
	@producer;
      my $stotal = 0;
      my $i = 0;
      while ($i < @producer
	     && ($stotal + $prodmap->{$producer[$i]}
		 < $market->{essential}->{$cid}->{need})) {
	$stotal += $prodmap->{$producer[$i++]};
      }
      if ($i + 1 >= @producer) {
	$i = $#producer;
	while ($i >= 0 && exists $owe->{$producer[$i]}) {
	  my $pid = $producer[$i--];
	  foreach my $to (keys %{$owe->{$pid}}) {
	    if (! exists $seller_price->{$to}) {
	      print STDERR "discount prod by buyer\n" if $DEBUG;
	      pay_owe($self, $prodmap, $pid, $to, \$owe->{$pid}->{$to});
	      delete $owe->{$pid}->{$to} if $owe->{$pid}->{$to} == 0.0;
	    }
	    last if $prodmap->{$pid} != 0.0;
	  }
	}
      }
    }

    foreach my $pid (keys %$owe) {
      if (0 == (keys %{$owe->{$pid}})) {
	delete $owe->{$pid};
      }
    }
    return;
  }

  sub preconsume_luxury {
    my $self = shift;
    my ($market) = @_;

    my $owe = {};
    $owe = $market->{tactics_data}->{$self->{id}}->{owe}
      if exists $market->{tactics_data}->{$self->{id}}->{owe};

    foreach my $cid (keys %{$market->{current_luxury}}) {
      my $pricemap = $market->{current_luxury}->{$cid};
      my @match = $market->match_desire($cid, $pricemap);
      next if ! @match;
      my ($maxprice, $maxd, $maxp) = @{$match[$#match]};
      my @overs = grep {$maxprice <= $pricemap->{$_} && $maxp ne $_}
	(keys %$pricemap);
      my $newpricemap = dclone($pricemap);
      foreach my $p (@overs) {
	$newpricemap->{$p} = $newpricemap->{$p} * 0.9;
      }
      my @newmatch = $market->match_desire($cid, $newpricemap);
      next if @newmatch <= @match;

      print STDERR "convert!\n" if $DEBUG;

      my %seller;
      my %buyer;
      foreach my $m (@newmatch) {
	my ($price, $d, $p) = @$m;
	$seller{$p} = 1;
	$buyer{$d} = 1;
      }
      my @looser;
      foreach my $m (@match) {
	my ($price, $d, $p) = @$m;
	if (! exists $seller{$p}) {
	  push(@looser, $p);
	} else {
	  delete $seller{$p};
	}
	delete $buyer{$d};
      }
      $newpricemap = dclone($pricemap);
      foreach my $p (keys %seller) {
	$newpricemap->{$p} = $newpricemap->{$p} * 0.9;
      }
      $market->{current_luxury}->{$cid} = $newpricemap;

      my @seller = keys %seller;
      my @winner = keys %buyer;
      my $owe_to_seller = scalar(keys %buyer) / scalar(keys %seller);
      my $owe_to_looser = scalar(@looser) / scalar(keys %seller);

      foreach my $to (keys %seller) {
	foreach my $p (keys %buyer) {
	  $owe->{$p} = {} if ! exists $owe->{$p};
	  $owe->{$p}->{$to} = 0 if ! exists $owe->{$p}->{$to};
	  $owe->{$p}->{$to} += $owe_to_seller;
	}
      }

      foreach my $to (@looser) {
	foreach my $p (keys %seller) {
	  $owe->{$p} = {} if ! exists $owe->{$p};
	  $owe->{$p}->{$to} = 0 if ! exists $owe->{$p}->{$to};
	  $owe->{$p}->{$to} += $owe_to_looser;
	}
      }
    }

    $market->{tactics_data}->{$self->{id}}->{owe} = $owe;

    return;
  }
}

{
  package Main::_Market;
  use base qw(Main::_Simple);

  __PACKAGE__->extend_template
    (
     player => {},
     luxury => {},
     essential => {},

     current_essential => {},
     current_luxury => {},
     current_gross_essential_consumption => 0,
     previous_gross_essential_consumption => 0,

     tactics => [],  # list of [$tid, $tactics].
     tactics_data => {},

     term => 0,
     term_rest => 0,
    );

  sub clone {
    my $market = shift;
    return dclone($market);
  }

  sub change_after_tactics {
    my $market = shift;
    my ($tid, $market2, %flag) = @_;

    if (exists $market2->{tactics_data}->{$tid}) {
      $market->{tactics_data}->{$tid} = $market2->{tactics_data}->{$tid};
    } elsif (exists $market->{tactics_data}->{$tid}) {
      delete $market->{tactics_data}->{$tid};
    }

    if (exists $flag{discountable_luxury}
	&& $flag{discountable_luxury}) {
      my $pricemap = $market->{current_luxury};
      my $pricemap2 = $market2->{current_luxury};

      foreach my $cid (keys %$pricemap) {
	if (exists $pricemap2->{$cid}) {
	  foreach my $pid (keys %{$pricemap->{$cid}}) {
	    if (exists $pricemap2->{$pid}) {
	      $pricemap->{$cid}->{$pid} = $pricemap2->{$cid}->{$pid}
		if $pricemap->{$cid}->{$pid} > $pricemap2->{$cid}->{$pid};
	      $pricemap->{$cid}->{$pid} = 0
		if $pricemap->{$cid}->{$pid} < 0;
	    }
	  }
	}
      }
    }

    if (exists $flag{discountable_essential}
	&& $flag{discountable_essential}) {
      my $prodmap = $market->{current_essential};
      my $prodmap2 = $market2->{current_essential};

      foreach my $cid (keys %$prodmap) {
	if (exists $prodmap2->{$cid}) {
	  foreach my $pid (keys %{$prodmap->{$cid}}) {
	    if (exists $prodmap2->{$pid}) {
	      $prodmap->{$cid}->{$pid} = $prodmap2->{$cid}->{$pid}
		if $prodmap->{$cid}->{$pid} > $prodmap2->{$cid}->{$pid};
	      $prodmap->{$cid}->{$pid} = 0
		if $prodmap->{$cid}->{$pid} < 0;
	    }
	  }
	}
      }
    }

    return;
  }

  sub annual_produce {
    my $market = shift;

    ## produce essentials.
    foreach my $pid (keys %{$market->{player}}) {
      my $p = $market->{player}->{$pid};
      foreach my $cid (keys %{$p->{possession}}) {
	my $quota = $p->{possession}->{$cid}->{quota};
	$market->{current_essential}->{$cid} = {}
	  if ! exists $market->{current_essential}->{$cid};
	my $current = $market->{essential}->{$cid}->random_produce($quota);
	$market->{current_essential}->{$cid}->{$pid} = $current;

      }
    }

    ## produce luxuries.
    foreach my $pid (keys %{$market->{player}}) {
      my $p = $market->{player}->{$pid};
      foreach my $cid (keys %{$market->{luxury}}) {
	my $c = $market->{luxury}->{$cid};
	my $current = $c->random_produce($p->{asset});
	if ($current) {
	  $market->{current_luxury}->{$cid} = {}
	    if ! exists $market->{current_luxury}->{$cid};
	  $market->{current_luxury}->{$cid}->{$pid} = $current;
	}
      }
    }

    return;
  }

  sub annual_demand_essential {
    my $market = shift;

    ## greed to sell essential commodities.
    foreach my $pid (keys %{$market->{player}}) {
      my $p = $market->{player}->{$pid};
      foreach my $cid (keys %{$market->{current_essential}}) {
	if (exists $market->{current_essential}->{$cid}->{$pid}) {
	  my $prod = $market->{current_essential}->{$cid}->{$pid};
	  my $quota = $p->{possession}->{$cid}->{quota};
	  my $greed = 0;
	  my $mag = exp(1 - pow($prod / $quota, 1/$Pow));
	  ##
	  ## With powered_normal_rand:
	  ##   Expected value of $mag
	  ##    == exp(1 + $k) * ngammainc(0.5, $k) / sqrt(pi)
	  ##     where $k = pow(sqrt(pi) / gamma(($Pow + 1) / 2), 2/$Pow) / 4
	  ##      for any $quota (i.e. variance);
	  ##    == 1.12896..... when $Pow == 0.5;
	  ##   The density function of $mag
	  ##    is the same for any $quota.
	  ##
	  ## With pseudo_powered_normal_rand:
	  ##   Expected value of $mag
	  ##    == 1.0/3 * $_PSPNR_A * exp(3) + 0.5 * $_PSPNR_B * exp(2);
	  ##   The density function of $mag
	  ##    == $_PSPNR_A * $x + $_PSPNR_B;
	  ##     for any $quota.
	  ##

	  ## The reason of approximation is why exp($p->{liablity})
	  ## reaches inf when $p->{liability} is large.
	  $greed = $mag
	    * sqrt(($p->{liability} > 100)? $p->{liability}
		   : log(1 + exp($p->{liability})));
	  $p->{greed}->{$cid} =  $greed;
	}
      }
    }

    return;
  }

  sub match_greed {
    my $market = shift;
    my ($cid, $prodmap) = @_;

    my @producer = keys %{$prodmap};
    @producer = sort {$market->{player}->{$a}->{greed}->{$cid}
			<=> $market->{player}->{$b}->{greed}->{$cid}}
      @producer;
    my $total_prod = 0;
    foreach my $pid (@producer) {
      $total_prod += $prodmap->{$pid};
    }
    my $total = 0;
    my $need = $market->{essential}->{$cid}->{need};
    my $i = 0;
    while ($need > 0 && $i < @producer) {
      my $pid = $producer[$i++];
      my $greed = $market->{player}->{$pid}->{greed}->{$cid};
      my $prod = $prodmap->{$pid};
      if ($need < $prod) {
	$total += $greed * $need;
	$need = 0;
      } else {
	$total += $greed * $prod;
	$need -= $prod;
      }
    }

    my $buyer_price = $total / scalar(keys %{$market->{player}});
    my $seller_price = {};

    foreach my $pid (@producer) {
      my $prod = $prodmap->{$pid};
      my $price = $prod * $total / $total_prod;
      $seller_price->{$pid} = $price;
    }

    return ($buyer_price, $seller_price);
  }

  sub annual_consume_essential {
    my $market = shift;

    foreach my $cid (keys %{$market->{current_essential}}) {
      my ($buyer_price, $seller_price) 
	= $market->match_greed($cid, 
			       $market->{current_essential}->{$cid});
      foreach my $pid (keys %$seller_price) {
	my $price = $seller_price->{$pid};
	$market->{player}->{$pid}->{liability} -= $price;
	$market->{player}->{$pid}->{current_income_essential}->{$cid} = $price;
      }
      foreach my $pid (keys %{$market->{player}}) {
	my $price = $buyer_price;
	$market->{player}->{$pid}->{liability} += $price;
	$market->{player}->{$pid}->{current_consumption_essential}->{$cid}
	  = $price;
      }
      $market->{current_gross_essential_consumption}
	= $buyer_price * scalar(keys %{$market->{player}});
    }

    return;
  }

  sub annual_demand_luxury {
    my $market = shift;

    ## initial price for luxury commodities.
    my $lnum = {};
    my $ltotal = {};
    my $lprice = {};
    foreach my $cid (keys %{$market->{current_luxury}}) {
      foreach my $pid (keys %{$market->{current_luxury}->{$cid}}) {
	my $price = $market->{current_luxury}->{$cid}->{$pid};
	$lnum->{$cid} = 0 if ! exists $lnum->{$cid};
	$ltotal->{$cid} = 0 if ! exists $ltotal->{$cid};
	$lnum->{$cid}++;
	$ltotal->{$cid} += $price;
      }
    }
    foreach my $cid (keys %$lnum) {
      $lprice->{$cid} = $market->{luxury}->{$cid}->{simplicity}
	* ($ltotal->{$cid} / $lnum->{$cid});
    }

    ## demand for luxuries.
    foreach my $pid (keys %{$market->{player}}) {
      my $p = $market->{player}->{$pid};
      foreach my $cid (keys %{$market->{current_luxury}}) {
	my $c = $market->{luxury}->{$cid};
	my $r = (4/pi)
	  * atan2($market->{current_gross_essential_consumption},
		  $market->{previous_gross_essential_consumption});
	my $current = $c->random_desire($p->{asset}, $lprice->{$cid}, $r);
	$market->{player}->{$pid}->{desire}->{$cid} = $current;
      }
    }

    return;
  }

  sub match_desire {
    my $market = shift;
    my ($cid, $pricemap) = @_;

    my @producer = keys %{$pricemap};
    @producer = sort {$pricemap->{$a} <=> $pricemap->{$b}} @producer;

    my @desire;
    foreach my $pid (keys %{$market->{player}}) {
      push(@desire, $pid) 
	if exists $market->{player}->{$pid}->{desire}->{$cid};
    }
    @desire = sort {$market->{player}->{$b}->{desire}->{$cid}
		      <=> $market->{player}->{$a}->{desire}->{$cid}}
      @desire;

    my @match;
    while (@producer && @desire) {
      my $p = $producer[0];
      my $d = shift(@desire);
      my $price = $pricemap->{$p};
      last if $market->{player}->{$d}->{desire}->{$cid} < $price;
      if (@desire) {
	my $rival = $desire[0];
	$price = $market->{player}->{$rival}->{desire}->{$cid}
	  if $market->{player}->{$rival}->{desire}->{$cid} > $price;
      }
      @producer = grep {$pricemap->{$_} <= $price} @producer;
      unshift(@match, [$price, $d, @producer]);
      pop(@producer) if @producer;
    }

    my $sold = 0;
    my $prev_price = 0;
    foreach my $m (@match) {
      my ($price, $d, @rest) = @$m;
      my $p = $rest[$sold++];
      my $seller_price = $pricemap->{$p};
      if (@rest > $sold) {
	print STDERR "discount match\n" if $DEBUG;
	$price = ($seller_price > $prev_price)?
	  $seller_price : $prev_price;
      }
      @$m = ($price, $d, $p);
      $prev_price = $price;
#      print STDERR "match $price $d $p\n" if $DEBUG;
    }
    return @match;
  }

  sub annual_consume_luxury {
    my $market = shift;

    for my $cid (keys %{$market->{current_luxury}}) {
      my @match = $market->match_desire($cid, 
					$market->{current_luxury}->{$cid});
      foreach my $m (@match) {
	my ($price, $d, $p) = @$m;
	$market->{player}->{$d}->{asset} += $price;
	$market->{player}->{$d}->{liability} += $price;
	$market->{player}->{$d}->{current_consumption_luxury}->{$cid} = $price;
	$market->{player}->{$p}->{liability} -= $price;
	$market->{player}->{$p}->{current_income_luxury}->{$cid} = $price;
      }
    }

    return;
  }

  sub annual_attach {
    my $market = shift;

    my $newquota = {};
    foreach my $pid (keys %{$market->{player}}) {
      my $p = $market->{player}->{$pid};
      foreach my $cid (keys %{$p->{possession}}) {
	my $quota = $p->{possession}->{$cid}->{quota};
	my $term = $p->{possession}->{$cid}->{term} - 1;
	if ($term <= 0) {
	  delete $p->{possession}->{$cid};
	  $newquota->{$cid} = 0 if ! exists $newquota->{$cid};
	  $newquota->{$cid} += $quota;
	} else {
	  $p->{possession}->{$cid}->{term} = $term;
	}
      }
    }
    foreach my $cid (keys %{$newquota}) {
      my $c = $market->{essential}->{$cid};
      $c->random_attach($newquota->{$cid}, $market->{player});
    }

    return;
  }

  sub annual_cleanup {
    my $market = shift;

    foreach my $pid (keys %{$market->{player}}) {
      my $p = $market->{player}->{$pid};
      $p->{desire} = {};
      $p->{greed} = {};

      $p->{current_income_essential} = {};
      $p->{current_income_luxury} = {};
      $p->{current_consumption_essential} = {};
      $p->{current_consumption_luxury} = {};
    }

    $market->{current_essential} = {};
    $market->{current_luxury} = {};

    $market->{previous_gross_essential_consumption} = 
      $market->{current_gross_essential_consumption};
    $market->{current_gross_essential_consumption} = 0;

    return;
  }

  sub main_loop {
    my $market = shift;
    my ($while) = @_;


    $market->{term} = 0;
    $market->{term_rest} = $while;
    if ($DEBUG) {
      map {$_->[1]->preloop($market)} @{$market->{tactics}};
    } else {
      map {
	my $m2 = $market->clone();
	my ($tid, $tactics) = @$_;
	$tactics->preloop($m2);
	$market->change_after_tactics($tid, $m2);
      } @{$market->{tactics}};
    }

    for (my $term = 0; $term < $while; $term++) {
      $market->{term} = $term + 1;
      $market->{term_rest} = $while - $term;
      print STDERR sprintf("TERM%03d:\n", $market->{term});

      if ($DEBUG) {
	map {$_->[1]->preterm($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->preterm($m2);
	  $market->change_after_tactics($tid, $m2);
	} @{$market->{tactics}};
      }
      if ($DEBUG) {
	map {$_->[1]->preproduce($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->preproduce($m2);
	  $market->change_after_tactics($tid, $m2);
	} @{$market->{tactics}};
      }
      $market->annual_produce();
      if ($DEBUG) {
	map {$_->[1]->predemand_essential($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->predemand_essential($m2);
	  $market->change_after_tactics($tid, $m2);
	} @{$market->{tactics}};
      }
      $market->annual_demand_essential();
      if ($DEBUG) {
	map {$_->[1]->preconsume_essential($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->preconsume_essential($m2);
	  $market->change_after_tactics($tid, $m2,
					discountable_essential => 1
				       );
	} @{$market->{tactics}};
      }
      $market->annual_consume_essential();
      if ($DEBUG) {
	map {$_->[1]->predemand_luxury($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->predemand_luxury($m2);
	  $market->change_after_tactics($tid, $m2);
	} @{$market->{tactics}};
      }
      $market->annual_demand_luxury();
      if ($DEBUG) {
	map {$_->[1]->preconsume_luxury($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->preconsume_luxury($m2);
	  $market->change_after_tactics($tid, $m2,
					discountable_luxury => 1
				       );
	} @{$market->{tactics}};
      }
      $market->annual_consume_luxury();
      if ($DEBUG) {
	map {$_->[1]->preattach($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->preattach($m2);
	  $market->change_after_tactics($tid, $m2);
	} @{$market->{tactics}};
      }
      $market->annual_attach();
      if ($DEBUG) {
	map {$_->[1]->precleanup($market)} @{$market->{tactics}};
      } else {
	map {
	  my $m2 = $market->clone();
	  my ($tid, $tactics) = @$_;
	  $tactics->precleanup($m2);
	  $market->change_after_tactics($tid, $m2);
	} @{$market->{tactics}};
      }
      $market->annual_cleanup();
    }

    $market->{term} = $while;
    $market->{term_rest} = 0;
    if ($DEBUG) {
      map {$_->[1]->postloop($market)} @{$market->{tactics}};
    } else {
      map {
	my $m2 = $market->clone();
	my ($tid, $tactics) = @$_;
	$tactics->postloop($m2);
	$market->change_after_tactics($tid, $m2);
      } @{$market->{tactics}};
    }

    return;
  }
}


# Math functions
{
  package Math::Aux;

  sub sgn {
    my ($x) = @_;
    return ($x < 0)? -1 : 1;
  }

## redifinition of POSIX math functions.
#   sub pow {
#     my ($x, $y) = @_;
#     return $x ** $y;
# #    return 0 if $x == 0;
# #    return exp($y * log($x));
#   }

#   sub floor {return int($_[0])}

#   sub ceil {
#     my ($x) = @_;
#     return int($x) + !!($x - int($x));
#   }

  sub normal_rand {
    my ($mean, $variance) = @_;
    $mean = 0.0 if ! defined $mean;
    $variance = 1.0 if ! defined $variance;

    return $mean + ($variance * sqrt(- 2 * log(1.0 - rand(1)))
		    * cos(pi * rand(1)));
  }

  sub powered_normal_rand {
    my ($mean, $variance, $pow) = @_;
    $mean = 0.0 if ! defined $mean;
    $variance = 1.0 if ! defined $variance;

    my $r = normal_rand(0, $variance);

    return $mean if $r == 0.0;
#    return sgn($r) * pow(abs($r), $pow) + $mean;
    return pow(abs($r), $pow) + $mean;
  }

  sub variance_for_mean_of_pnr {
    my ($mean, $pow) = @_;

    return pow($mean * sqrt(pi) / gamma(($pow + 1) / 2), 1/$pow)/sqrt(2);
  }

  my $_PSPNR_POW;
  my $_PSPNR_A;
  my $_PSPNR_B;
  my $_PSPNR_L;
  my $_PSPNR_R;

  sub pseudo_powered_normal_rand {
    my ($mean, $pow) = @_;
    $mean = 1.0 if ! defined $mean;

    if (! defined $_PSPNR_POW || $_PSPNR_POW != $pow) {
      $_PSPNR_POW = $pow;
      $_PSPNR_A = exp(-2) * (pow(2, $pow + 1) / (1 - pow(2, $pow)))
	* (- 1 + 1.0 / gamma(1 + $pow));
      $_PSPNR_B = exp(-1) - 0.5 * $_PSPNR_A * exp(1);
      $_PSPNR_L = 0.5 * $_PSPNR_B * $_PSPNR_B / $_PSPNR_A;
      $_PSPNR_R = 0.5 * $_PSPNR_A * pow(exp(1) + ($_PSPNR_B / $_PSPNR_A), 2);
    }
    my ($u, $y);

    while (! defined $y || $y == 0.00000000) {
      $u = $_PSPNR_L + rand(1) * ($_PSPNR_R - $_PSPNR_L);
      $y = - sqrt(2 * $u / $_PSPNR_A) - $_PSPNR_B / $_PSPNR_A;
    }

    return $mean * pow(1 - log($y), $pow);
  }

  ## approximation of gamma & erf;
  my $_ERF_A;
  sub _approx_erf1 {
    my ($x) = @_;

    $_ERF_A = - 8 * (pi - 3) / 3 * pi / (pi - 4) if ! defined $_ERF_A;
    return sgn($x) * sqrt(1 - exp(- $x * $x * (4 / pi + $_ERF_A * $x * $x) 
				  / (1 + $_ERF_A * $x * $x)));
  }

  sub _approx_normal_distr_c {
    my ($x) = @_;
    return sqrt(1 - exp(- 2 * $x * $x / pi));
  }

  sub _approx_normal_distr {
    my ($x) = @_;
    if ($x >= 0) {
      return 0.5 + 0.5 * _approx_normal_distr_c($x);
    } else {
      return 0.5 - 0.5 * _approx_normal_distr_c($x);
    }
  }

  sub _approx_normal_distr_inv {
    my ($x) = @_;
    my $y = 2 * $x - 1;
    my $sgn = ($x > 0.5)? - 1 : 1;
    return $sgn * sqrt(- pi / 2 * log((1 - $y * $y)));
  }

  sub _approx_erf2 {
    my ($x) = @_;
    return 2 * _approx_normal_distr($x * sqrt(2)) - 1;
  }

  my $_ERF;
  sub erf {
    if (! defined $_ERF) {
      eval {require Math::GSL::SF;};
      $_ERF = \&Math::GSL::SF::gsl_sf_erf if ! $@;
      if (! defined $_ERF) {
	eval {require Math::Libm;};
	$_ERF = \&Math::Libm::erf if ! $@;
      }
      if (! defined $_ERF) {
	eval {require Math::SpecFun::Erf;};
	$_ERF = \&Math::SpecFun::Erf::erf if ! $@;
      }
      if (! defined $_ERF) {
	$_ERF = \&_approx_erf2;
      }
    }
    return &{$_ERF}(@_);
  }

  sub _approx_gamma {
    my ($x) = @_;
    my $r = 1;

    sub _minigamma {
      my ($x) = @_;
      ## Euler-Mascheroni constant
      my $G = 0.57721566490153286060651209008240243104215933593992;

      return (1/$x) - $G + (1/6) * (3 * $G * $G + pi * pi /2) * $x;
    }

    sub _fact {
      my ($x) = @_;
      my $r = 1;
      if ($x < 0) {
	while ($x < 0) {
	  $r *= 1 / $x;
	  $x += 1;
	}
      } else {
	while ($x - 1 > 0) {
	  $r *= ($x - 1);
	  $x -= 1;
	}
      }
      return ($x, $r);
    }

    ($x, $r) = _fact($x);

    return $r if $x == 1.0;
    return $r * "Inf" if $x == 0.0;
    return $r * sqrt(pi) if $x == 0.5;

    if ($x < 0.5 && $x > 0) {
      return $r * &_minigamma($x);
    } else {
      my $c = &_minigamma(1 - $x);
      return $r * (pi / sin(pi * $x)) / $c;
    }
  }

  my $_GAMMA;
  sub gamma {
    my ($x) = @_;

    if (! defined $_GAMMA) {
      eval {require Math::GSL::SF;};
      $_GAMMA = \&Math::GSL::SF::gsl_sf_gamma if ! $@;
      if (! defined $_GAMMA) {
	eval {require Math::SpecFun::GAMMA;};
	$_GAMMA = \&Math::SpecFun::Gamma::gamma if ! $@;
      }
      if (! defined $_GAMMA) {
	$_GAMMA = \&_approx_gamma;
      }
    }

    return &{$_GAMMA}($x);
  }

  sub _ngammainc_half {
    my ($s, $x) = @_;

    if ($s == 0.5) {
      return sqrt(pi) * (1 - erf(sqrt($x)));
    } else {
      croak "No support for $s != 0.5.\n";
    }
  }

  ## Non-Regularized Incomplete Gamma Function.
  my $_NGAMMAINC;
  sub ngammainc {
    my ($s, $x) = @_;
    if (! defined $_NGAMMAINC) {
      eval {require Math::GSL::SF;};
      $_NGAMMAINC = \&Math::GSL::SF::gsl_sf_gamma_inc if ! $@;
      if (! defined $_NGAMMAINC) {
	eval {require Math::SpecFun::GAMMA;};
	$_NGAMMAINC = sub {
	  my ($s, $x) = @_;
	  return gamma($s) * Math::SpecFun::gammaincc($x, $s);
	};
      }
      if (! defined $_NGAMMAINC) {
	$_NGAMMAINC = \&_ngammainc_half;
      }
    }
    return &{$_NGAMMAINC}($s, $x);
  }
}


# MAIN

MAIN:
{
  srand($SRAND) if defined $SRAND;

  my $market = Main::_Market->new();

  for (my $i = 0; $i < $PLAYERS; $i++) {
    my $p = Main::_Player->new();
    $market->{player}->{$p->{id}} = $p;
    $p->{liability} = $INITIAL_LIABILITY;
    $p->{asset} = $INITIAL_ASSET;
  }

  for (my $i = 0; $i < $LUXURIES; $i++) {
    my $c = Main::_LuxuryCommodity->new();
    $c->{difficulty} = 1 - (1.0 / $PLAYERS);
    $c->{simplicity} = 0.7;
    $market->{luxury}->{$c->{id}} = $c;
  }

  for (my $i = 0; $i < $ESSENTIALS; $i++) {
    my $c = Main::_EssentialCommodity->new();
    $c->{need} = $PLAYERS * 0.7;
    $market->{essential}->{$c->{id}} = $c;
    $c->random_attach(ceil($PLAYERS * 0.8), $market->{player});
  }

  foreach my $tspec (@TACTICS) {
    my ($class, @options) = @$tspec;
    my $tid = sprintf("T%03d", 1 + $Main::_Tactics::num_of_tactics);
    my $tactics = $class->new(@options);

    push(@{$market->{tactics}}, [$tid, $tactics]);
  }

  $market->main_loop($TERM);

  exit(0);
}

=pod

=encoding utf8

=head1	NAME

simple_market_0.pl - an simple economy simulation for external-reaction.

=head1	SYNOPSIS

perl B<simple_market_0.pl> [--term TERM] [--tactics-dumper]

=head1	OPTIONS

=over 8

=item B<--srand> F<NUM>

specify the rand seed.

=item B<--term> F<TERM>

specify the loop count.

=item B<--player> F<NUM>

specify the number of players.

=item B<--essential> F<NUM>

specify the number of essential commodities.

=item B<--luxury> F<NUM>

specify the number of luxury commodities.

=item B<--initial-asset> F<FLOAT>

specify the amount of initial asset of the players.

=item B<--initial-liability> F<FLOAT>

specify the amount of initial liability of the players.

=item B<--ppnr>

use pseudo_powered_normal_rand to produce essential commodities.  But
I, the author, should decide which to use. *B<OBSOLETE>*

=item B<--tactics-dumper>

print the dump of $market at the end of each term.

=item B<--tactics-desire-greed-convert>

use tactics to convet rights of discount if possible.

=item B<--help>

show help message about options.

=back

=head1	DESCRIPTION

A simple simulation of economy for modeling external reaction by
internal reaction.

嗜好品需要モデル。luxury

ある market に
buyer と seller が情報を出す。
seller の価格を超える desire を持つ buyer のうち、
もっとも高い desire を持つものが、
二番目に高い desire 価格または seller の価格で購入を行う。


必需品需要モデル。essential

greed のもっとも低い seller から順に買い入れられて必要量消費される。
その平均額がその品の seller から均等にわけ与えられる。
その総額が大きいと、desire の総量が大きくなる。
  (すなわち、所得ルール。消費量一定でも消費総額が多いと気が大きくなって、
   desire も大きくなる。)

一般需要ルール。

その期の収穫が少なく liability が大きいと greed が大きくなる。
asset が大きいと desire の振輻が大きくなる。


嗜好品供給モデル。luxury

各 player に毎期一定の確率でできる。その価格は asset が大きいほど、大きくなる。


必需品供給モデル。

総産出量がある程度決まっており、毎期、必要量以上はある。
総 liability が大きいほど、産出量が大きくなる。
ランダムに持分権と権利期間が割り振られ、権利期間が終るまで持続する。
持分権にランダム変動をかけた割合で分割して生産したことになる。


収支による変化のルール。

嗜好品購入 --> 資産と負債が同時に増える。
必需品購入 --> 負債が増える。
嗜好品売却 --> 負債が減る。または(現金)資産が増える。
必需品売却 --> 負債が減る。または(現金)資産が増える。

なお、「(現金)資産」とは単にマイナスの負債のことである。
つまり、このシミュレーション世界では、現金とは「マイナスの負債」
すなわち、負債の相手方が誰かいるということの標章に過ぎない。


資産減耗。

減耗はなくてもよい。
減耗する場合は、定率にするか、ランダムにばらすか試してみるべきだろう。
なお、資産価格の上昇は、asset に関して
disire や供給価格が変化することに捉えられているとする。


オークションモデルへの応用。

本来このモデルを作りはじめた目的が、ペニーオークションのモデル作成だった。

オークションの簡単なモデルを作るにも、外部経済への影響というのを考えざ
るを得ない。それをとりあえずでいいので考慮できる単純化された経済の外作
用モデルが欲しかった。


戦略の違いの反映。

例えば desire に有効時間を設定したいといったとき、
すでに有効時間という考え方のある production に結びつける戦略と定義する
…といった考え方ができないか？


集団の埋め込み。

複数の player を統合して、一人の player と見なす「埋め込み」は可能か？
どういう条件が必要か？


property。

property は、戦略のパラメータの格納などに使う。

--> property はやめて $market->{tactics_data} を使うようにした。


View。

Player(に埋め込まれた集団の中)の view に関して統計をとる。
(この統計が simulation そのものの実験者への view とも言える。)


戦術。

戦略に view を反映させる方法、すなわちこのゲームのコマは二種しかない。
一つは、必需品を「廃棄」すること、もう一つは嗜好品の価格を下げること。
量をいじれるのは commodity だけで、
asset や liability, production もいじることはできない。

必需品の「廃棄」により、必要需要量を下回ってもよい。
これは単に、ある者の必需品売却に対する受益権の放棄を意味するに近い。

嗜好品を自分の desire よりも安い価格とすることで、
それを自分の asset にもできる。
ただし、自分の desire がどれぐらいかが明らかであるとは限らない。
(その期、買い手のいない嗜好品は廃棄される。)

他者の価格推移がわかったり desire がわかったりすると考えるかどうか、
それによりどのように「廃棄」を決定するかが戦術となる。
desire や greed のやりとりもできないが、
「自分」が知っているそのデータを根拠に
「廃棄」や「ディスカウント」する「契約」は結べる。


二つの基本戦術。

「廃棄」や「価格低下」のような「マイナス」の操作だけでは、
経済的に意味がないと思うかもしれないが、そうではない。

まず、廃棄されるはずだった嗜好品の価格を自分の desire にまで下落させる
ことができれば、相手には収入が入るし、自分の asset は大きくなる。だか
ら、価格下落の権利は未実現資産として view されることになる。

また、greed の低い者の必需品を廃棄させることができれば、必需品の価格は
たいてい上がることになり、それは liability の減少、ひいては、greed の
減少につながり、「好順還」をもたらすかもしれない。特に、誰でもいいから
廃棄させられるといったものは「現金」として view できるようになるかもし
れない。

ただし、asset が多きいことも liability が大きくなることも「良いことと
する」かどうかは決まっていない。とりあえずは、総 liability が小さい中
で、嗜好品の一期の売買総額が大きくなることを目標にしようかと思う。

最初の view モデルは上の二つを直接つなげるように考えよう。売れ残った嗜
好品を 10% 下げれば買える者がいるとき、その者は任意の期の必需品の 10% 
を廃棄する権利を渡すことで、ディスカウントが実現するとする。そして、そ
の者の必需品が売れる状態ならば、必ず 10% 廃棄の権利が使われるとする。
そして、view として、必需品廃棄の権利がどれぐらい大きくなるかも調べよ
う。その内作用により、どう外作用的経済の統計が変わるか、その内作用的 
view を最大にする外作用とはどのようなものか考えたい。

このような戦術の導入により、asset があり liability がなくても貧乏と 
view されるものが出てくるだろう。それによって、外作用モデルの結果も変
わる。しかし、それでも外作用のルールが変更されるわけではない。

何かを最大にする外作用を考えたとしても、その実現は、それを可能とする別
の内作用を見つけることが理想である。ただし、その内作用どうしの相互作用
が意図しない結果を生むことも十分考えられる。


解析。

場面の設定と、場面の変分により本質的パラメータを特定する。


=head1	AUTHOR

JRF E<lt>http://jrf.cocolog-nifty.com/society/E<gt>

=head1	LICENSE

Public Domain as a sort of mathematical formulae.

=cut
