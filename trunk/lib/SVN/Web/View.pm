package SVN::Web::View;
use strict;
use SVN::Core;
use SVN::Repos;
use SVN::Fs;

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    %$self = @_;

    return $self;
}

sub _log {
    my ($self, $paths, $rev, $author, $date, $msg, $pool) = @_;
#    my ($self, $rev, $root, $paths, $props) = @_;
    return unless $rev > 0;
#    my ($author, $date, $message) = @{$props}{qw/svn:author svn:date svn:log/};

    my $data = { rev => $rev, author => $author,
		 date => $date, msg => $msg };
    $data->{paths} = { map { $_ => { action => $paths->{$_}->action(),
				     copyfrom => $paths->{$_}->copyfrom_path(),
				     copyfromrev => $paths->{$_}->copyfrom_rev(),
				     }} keys %$paths};

    return $data;
}

sub run {
    my $self = shift;
    my $pool = SVN::Pool->new_default_sub;
    my $fs = $self->{repos}->fs;
    my $rev = $self->{cgi}->param('rev') || $fs->youngest_rev;
    my $root = $fs->revision_root($rev);

    # Start at $rev, and look backwards for the first interesting
    # revision number for this file.
    my $hist = $root->node_history($self->{path});
    $hist = $hist->prev(0);
    $rev = ($hist->location())[1];

    # Get the log for this revision of the file
    $self->{repos}->get_logs([$self->{path}], $rev - 1, $rev, 1, 0,
                             sub { $self->{REV} = $self->_log(@_)});

    # Get the text for this revision of the file
    $root = $fs->revision_root($rev);
    my $file = $root->file_contents($self->{path});
    local $/;
    return {template => 'view',
	    data => { rev => $rev,
		      mimetype => $root->node_prop($self->{path},
						   'svn:mime-type') || 'text/plain'),
		      file => <$file>,
		      %{$self->{REV}},
		    }};
}

1;
