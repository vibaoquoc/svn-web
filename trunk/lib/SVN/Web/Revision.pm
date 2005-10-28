package SVN::Web::Revision;
use strict;
use Text::Diff;
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
    $pool->default;
    my $data = {rev => $rev, author => $author,
		date => $date, msg => $msg};
    $data->{paths} = {map { $_ => {action => $paths->{$_}->action,
				   copyfrom => $paths->{$_}->copyfrom_path,
				   copyfromrev => $paths->{$_}->copyfrom_rev,
				  }} keys %$paths};
    my $root = $self->{repos}->fs->revision_root ($rev);
    my $oldroot = $self->{repos}->fs->revision_root ($rev-1);
    for (keys %{$data->{paths}}) {
	$data->{paths}{$_}{isdir} = 1
	    if $data->{paths}{$_}{action} eq 'D' ? $oldroot->is_dir ($_) : $root->is_dir ($_);
    }
    return $data;
}

sub run {
    my $self = shift;
    my $pool = SVN::Pool->new_default_sub;
    my $rev = $self->{cgi}->param('rev') || die 'no revision';

    $self->{repos}->get_logs (['/'], $rev, $rev, 1, 0,
			      sub { $self->{REV} = $self->_log(@_)});

    # Generate the diffs for each file
    foreach my $path (keys %{$self->{REV}->{paths}}) {
      if($self->{REV}->{paths}{$path}{action} eq 'M') {
	my $root1 = $self->{repos}->fs()->revision_root($rev);
	my $root2 = $self->{repos}->fs()->revision_root($rev - 1);

	my $kind;
	$kind = $root1->check_path($path);
	next if $kind == $SVN::Node::none;
	$kind = $root2->check_path($path);
	next if $kind == $SVN::Node::none;

	$self->{REV}->{paths}{$path}{diff} = Text::Diff::diff
	  ($root1->file_contents($path),
	   $root2->file_contents($path),
	   { STYLE => 'Text::Diff::HTML' });

	$self->{REV}->{paths}{$path}{diff} =~ s/^  //mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/<span class="ctx">  /<span class="ctx">/mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/<ins>\+ /<ins>/mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/<del>- /<del>/mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/^- //mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/^\+ //mg;
      }
    }

    return {template => 'revision',
	    data => { rev => $rev, %{$self->{REV}}}};
}

1;

