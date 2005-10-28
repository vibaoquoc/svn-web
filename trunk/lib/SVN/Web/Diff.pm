# -*- Mode: cperl; cperl-indent-level: 4 -*-

package SVN::Web::Diff;
use strict;
use Text::Diff;
use SVN::Core;
use SVN::Repos;
use SVN::Fs;

eval 'use SVN::DiffEditor 0.09; require IO::String; 1' and my $has_svk = 1;

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    %$self = @_;

    return $self;
}

sub run {
    my $self = shift;
    my $pool = SVN::Pool->new_default_sub;
    my $fs = $self->{repos}->fs;
    my $rev1 = $self->{cgi}->param('rev1');
    my $rev2 = $self->{cgi}->param('rev2');
    my $mime = $self->{cgi}->param('mime') || 'text/html';

    my $root1 = $fs->revision_root ($rev1);
    my $root2 = $fs->revision_root ($rev2);
    my $kind = $root1->check_path ($self->{path});

    die "path does not exist" if $kind == $SVN::Node::none;

    my $output;

    if ($kind == $SVN::Node::dir) {
	die "directory diff requires svk"
	    unless $has_svk;

	my $path = $self->{path};
	$path =~ s|/$||;

	my $editor = SVN::DiffEditor->new
	    ( cb_basecontent => sub { my ($rpath) = @_;
				      my $base = $root1->file_contents ("$path/$rpath");
				      return $base;
				  },
	      cb_baseprop => sub { my ($rpath, $pname) = @_;
				   return $root1->node_prop ("$path/$rpath", $pname);
			       },
	      llabel => "revision $rev1",
	      rlabel => "revision $rev2",
	      lpath  => $path,
	      rpath  => $path,
	      fh     => IO::String->new(\$output)
	    );

	SVN::Repos::dir_delta ($root1, $path, '',
			       $root2, $path,
			       $editor, undef,
			       1, 1, 0, 1);
    }
    else {
        my $style;
	$mime eq 'text/html' and $style = 'Text::Diff::HTML';
	$mime eq 'text/plain' and $style = 'Unified';

	$output = Text::Diff::diff
	    ($root1->file_contents ($self->{path}),
	     $root2->file_contents ($self->{path}),
	     { STYLE => $style });
    }

    if($mime eq 'text/html') {
	$output =~ s/^  //mg;
	$output =~ s/<span class="ctx">  /<span class="ctx">/mg;
	$output =~ s/<ins>\+ /<ins>/mg;
	$output =~ s/<del>- /<del>/mg;
	$output =~ s/^- //mg;
	$output =~ s/^\+ //mg;

	return { template => 'diff',
		 data => { body => $output }};
    } else {
	return { mimetype => $mime,
		 body => $output };
    }
}

1;