# -*- Mode: cperl; cperl-indent-level: 4 -*-
package SVN::Web::Browse;
use strict;
use SVN::Core;
use SVN::Repos;
use SVN::Fs;
use SVN::Web::X;

=head1 NAME

SVN::Web::Browse - SVN::Web action to browse a Subversion repository

=head1 SYNOPSIS

In F<config.yaml>

  actions:
    ...
    browse:
      class: SVN::Web::Browse
    ...

=head1 DESCRIPTION

Returns a file/directory listing for the given repository path.

=head1 OPTIONS

=over 4

=item rev

The repository revision to show.  Defaults to the repository's youngest
revision.

=back

=head1 TEMPLATE VARIABLES

=over 4

=item entries

A list of hash refs, one for each file and directory entry in the browsed
path.  The list is ordered with directories first, then files, sorted
alphabetically.

Each hash ref has the following keys.

=over 8

=item path

The entry's full path.

=item rev

The entry's most recent interesting revision.

=item size

The entry's size, in bytes.  The empty string C<''> for directories.

=item type

The entry's C<svn:mime-type> property.  Not set for directories.

=item author

The userid that committed the most recent interesting revision for this
entry.

=item date

The date of the entry's most recent interesting revision.

=item msg

The log message for the entry's most recent interesting revision.

=back

=item rev

The repository revision that is being browsed.  Will be the same as the
C<rev> parameter given to the action, unless that parameter was not set,
in which case it will be the repository's youngest revision.

=item youngest_rev

The repository's youngest revision.

=back

=head1 EXCEPTIONS

=over 4

=item (path %1 does not exist in revision %2)

The given path is not present in the repository at the given revision.

=back

=cut

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    %$self = @_;

    return $self;
}

sub run {
    my $self = shift;
    my $rev = $self->{cgi}->param('rev') || $self->{repos}->get_latest_revnum();

    if ($self->{path} !~ m|/$|) {
        print $self->{cgi}->redirect(-uri => $self->{cgi}->self_url() . '/');
	return;
    }
    my $path = $self->{path};
    $path =~ s|/$|| unless $path eq '/';
    my $kind = $self->{repos}->check_path($path, $rev);

    if($kind == $SVN::Node::none) {
      SVN::Web::X->throw(error => '(path %1 does not exist in revision %2)',
			 vars => [$path, $rev]);
    }

    die "not a directory in browse" unless $kind == $SVN::Node::dir;

    my @results = $self->{repos}->get_dir($path, $rev);
    my %dirents = %{$results[0]};
    my %props   = %{$results[2]};

    my $entries = [];
    foreach my $ent (keys %dirents) {
	my %h = ();
	$h{name} = $ent;

	$h{kind}   = $dirents{$ent}->kind();
	$h{isdir}  = $dirents{$ent}->kind() == $SVN::Node::dir;
	$h{rev}    = $dirents{$ent}->created_rev();
	$h{size}   = $dirents{$ent}->size();
	$h{props}  = $dirents{$ent}->has_props();
	$h{author} = $dirents{$ent}->last_author();
	$h{date_modified} = $self->{repos}->rev_prop($h{rev}, 'svn:date');
	$h{msg} = $self->{repos}->rev_prop($h{rev}, 'svn:log');

	push @$entries, \%h;
    }

#    my $spool = SVN::Pool->new_default;
#    for (@$entries) {
#	my $path = "$self->{path}$_->{name}";
#	$_->{type} = $root->node_prop ($self->{path}.$_->{name},
#				       'svn:mime-type') unless $_->{isdir};
#	$_->{type} =~ s|/\w+|| if $_->{type};
#	$spool->clear;
#    }

    # TODO: custom sorting
    @$entries = sort {($b->{isdir} <=> $a->{isdir}) || ($a->{name} cmp $b->{name})} @$entries;

    my @props = ();
    foreach my $prop_name (qw(svn:externals)) {
	if(exists $props{$prop_name}) {
	    push @props, { name => $prop_name,
			   value => $props{$prop_name} };
	}
    }

    return { template => 'browse',
	     data => { entries => $entries,
		       rev => $rev,
		       youngest_rev => $self->{repos}->get_latest_revnum(),
		       props => \@props,
		     }};
}

1;
