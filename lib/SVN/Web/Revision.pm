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

    my $fs = $self->{repos}->fs();

    # Generate the diffs for each file
    foreach my $path (keys %{$self->{REV}->{paths}}) {
      next if $self->{REV}->{paths}{$path}{isdir};

      if($self->{REV}->{paths}{$path}{action} eq 'M') {
	my $root1 = $fs->revision_root($rev);
	my $root2 = $fs->revision_root($rev - 1);

	my $kind;
	$kind = $root1->check_path($path);
	next if $kind == $SVN::Node::none;
	$kind = $root2->check_path($path);
	next if $kind == $SVN::Node::none;

	$self->{REV}->{paths}{$path}{diff} = Text::Diff::diff
	  ($root2->file_contents($path),
	   $root1->file_contents($path),
	   { STYLE => 'Text::Diff::HTML' });

	next;
      }

      # If the file was added it may have been copied from another file.
      # Find out if it was, and if it was, do a diff between the two files.
      # If there were any changes then show them
      if(($self->{REV}->{paths}{$path}{action} eq 'A') and
	 defined $self->{REV}->{paths}{$path}{copyfrom}) {
	my $src = $self->{REV}->{paths}{$path}{copyfrom};

	my $root1 = $fs->revision_root($rev);
	my $root2 = $fs->revision_root($self->{REV}->{paths}{$path}{copyfromrev});

	# If the files have differing MD5s then do a diff
	if($root1->file_md5_checksum($path) ne $root2->file_md5_checksum($src)) {
	  $self->{REV}->{paths}{$path}{diff} = Text::Diff::diff
	    ($root2->file_contents($src),
	     $root1->file_contents($path),
	     { STYLE => 'Text::Diff::HTML' });
	}

	next;
      }
    } continue {
      if(defined $self->{REV}->{paths}{$path}{diff}) {
	$self->{REV}->{paths}{$path}{diff} =~ s/^  //mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/<span class="ctx">  /<span class="ctx">/mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/<ins>\+ /<ins>/mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/<del>- /<del>/mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/^- //mg;
	$self->{REV}->{paths}{$path}{diff} =~ s/^\+ //mg;
      }
    }

    return {template => 'revision',
	    data => { rev => $rev,
		      youngest_rev => $fs->youngest_rev(),
		      %{$self->{REV}}}};
}

1;

