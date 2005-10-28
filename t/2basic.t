#!/usr/bin/perl -w
use strict;
use SVN::Web;
use Test::More;
use File::Path;
use File::Spec;

my $repospath;
BEGIN {
plan skip_all => "Test::WWW::Mechanize not installed"
    unless eval { require Test::WWW::Mechanize; 1 };

plan skip_all => "can't find svnadmin"
    unless `svnadmin --version` =~ /version/;

plan 'no_plan';
$repospath = File::Spec->rel2abs("t/repos");

rmtree ([$repospath]) if -d $repospath;
$ENV{SVNFSTYPE} ||= (($SVN::Core::VERSION =~ /^1\.0/) ? 'bdb' : 'fsfs');

`svnadmin create --fs-type=$ENV{SVNFSTYPE} $repospath`;
`svnadmin load $repospath < t/test_repo.dump`;

}

my $url = 'http://localhost/svnweb';
use SVN::Web::Test ('http://localhost', '/svnweb',
		    repos => $repospath);
my $mech = SVN::Web::Test->new;
$mech->get ('http://localhost/svnweb/repos/browse/A');

$mech->content_is ('internal server error', 'no trailing slash for dir');

$mech->get ('http://localhost/svnweb/repos/browse/');
$mech->title_is ('SVN::Web');

$mech->get ('http://localhost/svnweb/repos/revision/?rev=2');
$mech->title_is ('SVN::Web');

$mech->get ('http://localhost/svnweb/');
$mech->title_is ('SVN::Web');

my %seen;

check_links (0);


sub check_links {
    my $indent = shift;
    is ($mech->status, 200);
    $mech->content_unlike (qr'operation failed');
    my @links = $mech->links;
    diag ((' ' x $indent)."==> ".$mech->uri.": ".(scalar @links)."\n")
	if $ENV{TEST_VERBOSE};
    for my $i (0..$#links) {
	my $link_url = $links[$i]->url_abs;
	next if $seen{$link_url};
	++$seen{$link_url};
	next if $link_url =~ m/diff/;
	$mech->follow_link ( n => $i+1 );
	check_links ($indent+1);
	$mech->back;
    }
    --$indent;
}


#warn join("\n", map {$_->url_abs} @links);

#$mech->link_status_is( [ grep {$_->url_abs =~ m|^$url.+| } @links ], 200);
