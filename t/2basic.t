#!/usr/bin/perl -w
use strict;
use SVN::Web;
use Test::More qw(no_plan);
use File::Path;
use File::Spec;

my $repospath;
BEGIN {
plan skip_all => "Test::WWW::Mechanize not installed"
    unless eval { require Test::WWW::Mechanize; 1 };

plan skip_all => "can't find svnadmin"
    unless `svnadmin --version` =~ /version/;

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

$mech->get ('http://localhost/svnweb/repos/browse/');
$mech->title_is ('browse: /repos/ (via SVN::Web)', "'browse' has correct title");

$mech->get ('http://localhost/svnweb/repos/revision/?rev=2');
$mech->title_is ('revision: /repos/ (Rev: 2, via SVN::Web)', "'revision' has correct title");

$mech->get ('http://localhost/svnweb/');
$mech->title_is ('Repository List (via SVN::Web)', "'list' has correct title");

my %seen;

check_links();


sub check_links {
    diag "---";
    is ($mech->status, 200, 'Fetched: ' . $mech->uri());
    $mech->content_unlike (qr'operation failed', '   and content was correct');
    my @links = $mech->links;
    diag 'Found ' . (scalar @links) . ' links' if $ENV{TEST_VERBOSE};
    for my $i (0..$#links) {
        my $link_url = $links[$i]->url_abs;
        diag "Link $i/$#links: $link_url" if $ENV{TEST_VERBOSE};
        next if $seen{$link_url};
        ++$seen{$link_url};
        next if $link_url =~ m/diff/;
        next if $link_url !~ /localhost/;
        diag "Following $link_url" if $ENV{TEST_VERBOSE};
        $mech->follow_link ( n => $i+1 );
        check_links();
        diag "--- Back";
        $mech->back;
    }
}


#warn join("\n", map {$_->url_abs} @links);

#$mech->link_status_is( [ grep {$_->url_abs =~ m|^$url.+| } @links ], 200);
