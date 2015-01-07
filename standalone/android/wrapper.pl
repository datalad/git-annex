#!/usr/bin/perl
my $prog=q{PROG}; # replaced
my @opts=qw{OPTS}; # replaced

if (grep { $_ eq "-r" || $_ eq "--relocatable" } @ARGV) {
	print "running $prog withthout extra opts, as relocatable build detected\n";
	exec($prog,@ARGV) || die "failed to run $prog";
}
else {
	print "running $prog with extra opts @opts\n";
	exec($prog,@opts,@ARGV) || die "failed to run $prog";
}
