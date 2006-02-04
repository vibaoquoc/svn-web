# -*- Mode: cperl-mode; cperl-indent-level: 4 -*-

package Devel::Memory;

use strict;
use warnings;

use BSD::Resource;

BEGIN {
    $^P |= 0x01;
}

package DB;

no strict 'refs';

our $in_br;
our @r;

sub DB { }			# Must exist, doesn't need to do anything

sub sub {
    if($DB::sub =~ /BSD::Resource/ || $in_br) {
	local $in_br;
	$in_br++;
	&{$DB::sub};
    } else {
	@r = BSD::Resource::getrusage();

	print STDERR "Entering $DB::sub, maxrss = $r[2]\n" if $DB::sub =~ /SVN::Web/;

	&{$DB::sub};
    }
}

return 1;
