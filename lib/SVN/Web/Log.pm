package SVN::Web::Log;
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

    push @{$self->{REVS}}, $data;
}

# XXX: stolen from svk::util
sub traverse_history {
    my %args = @_;

    my $old_pool = SVN::Pool->new;
    my $new_pool = SVN::Pool->new;
    my $spool = SVN::Pool->new_default;

    my $hist = $args{root}->node_history ($args{path}, $old_pool);
    my $rv;

    while ($hist = $hist->prev(($args{cross} || 0), $new_pool)) {
        $rv = $args{callback}->($hist->location ($new_pool));
        last if !$rv;
        $old_pool->clear;
	$spool->clear;
        ($old_pool, $new_pool) = ($new_pool, $old_pool);
    }

    return $rv;
}

sub run {
    my $self = shift;
    my $pool = SVN::Pool->new_default_sub;
    my $fs = $self->{repos}->fs;
    my $limit = $self->{cgi}->param('limit') || 20;
    my $rev   = $self->{cgi}->param('rev') || $fs->youngest_rev();
    my $root = $fs->revision_root ($rev);
    my $endrev = 0;
    if ($limit) {
	my $left = $limit;
	traverse_history (root => $root, path => $self->{path}, cross => 0,
			  callback => sub { $endrev = $_[1]; return --$left });
    }
#    SVK::Command::Log::do_log (repos => $self->{repos}, limit => $limit,
#			       path => $self->{path},
#			       fromrev => $fs->youngest_rev, torev => -1,
#			       cb_log => sub {$self->_log(@_)});

    $self->{repos}->get_logs ([$self->{path}], $rev, $endrev, 1, 0,
                             sub { $self->_log(@_)});
    return {template => 'log',
	    data => { isdir => ($root->is_dir($self->{path})),
		      revs => $self->{REVS},
		      limit => $limit,
		      branchpoints => $self->{branch}->branchpoints ($self->{path}),
		    }};
}

1;
