package SVN::Web::Browse;
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
    return unless $rev > 0;

    return ($author, $msg);
}

sub run {
    my $self = shift;
    my $fs = $self->{repos}->fs;
    my $rev = $self->{cgi}->param('rev') || $fs->youngest_rev;
    if ($self->{path} !~ m|/$|) {
        print $self->{cgi}->redirect(-uri => $self->{cgi}->self_url() . '/');
    }
    my $path = $self->{path};
    $path =~ s|/$|| unless $path eq '/';
    my $root = $fs->revision_root ($rev);
    my $kind = $root->check_path ($path);
    die "path does not exist" if $kind == $SVN::Node::none;

    die "not a directory in browse" unless $kind == $SVN::Node::dir;

    my $entries = [ map {{ name => $_->name,
			   kind => $_->kind,
			   isdir => ($_->kind == $SVN::Node::dir),
		       }} values %{$root->dir_entries ($self->{path})}];


    my $spool = SVN::Pool->new_default;
    for (@$entries) {
	my $path = "$self->{path}$_->{name}";
	$_->{rev} = ($fs->revision_root ($rev)->node_history
		     ($path)->prev(0)->location)[1];
	$_->{size} = $_->{isdir} ? '' :
	    $root->file_length ($path);
	$_->{type} = $root->node_prop ($self->{path}.$_->{name},
				       'svn:mime-type') unless $_->{isdir};
	$_->{type} =~ s|/\w+|| if $_->{type};

	# Get the log for this revision of the file
	#
	# At least some of this shouldn't be necessary, as the 'last-author'
	# and 'log' properties should be accessible.  But I can't get
	# the code to work, hence this workaround.
	$self->{repos}->get_logs([$path], $_->{rev}, $_->{rev}, 0, 1,
				 sub { ($_->{author}, $_->{msg}) = $self->_log(@_)});

	$spool->clear;
    }

    # TODO: custom sorting
    @$entries = sort {($b->{isdir} <=> $a->{isdir}) || ($a->{name} cmp $b->{name})} @$entries;

    return { template => 'browse',
	     data => { entries => $entries, revision => $rev,
		       branchto => $self->{branch}->branchto ($self->{path}, $rev),
		       branchfrom => $self->{branch}->branchfrom ($self->{path}, $rev),
		     }};
}

1;
