package SVN::Web::List;
use strict;
use File::Basename ();

sub new {
    my $class = shift;
    my $self = bless {}, $class;
    %$self = @_;

    return $self;
}

sub run {
    my $self = shift;

    my @repos = SVN::Web::repos_list();

    # If there's only one repo listed then jump straight to it
    if(@repos == 1) {
        my $url = $self->{cgi}->self_url();
	$url =~ s{/$}{};
	$url .= "/$repos[0]";
        print $self->{cgi}->redirect(-uri => $url);
	return;
    }

    return { template => 'list',
	     data => {
		      action => 'list',
		      nonav => 1,
                      repos => \@repos,
                      reposcount => scalar @repos}};
}

1;
