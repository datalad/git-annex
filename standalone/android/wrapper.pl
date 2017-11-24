#!/usr/bin/perl
my $prog=q{PROG}; # replaced
my @opts=qw{OPTS}; # replaced

if (grep { $_ eq "-r" || $_ eq "--relocatable" } @ARGV) {
	exec($prog,@ARGV) || die "failed to run $prog";
}
else {
	exec($prog,@opts,@ARGV) || die "failed to run $prog";
}
