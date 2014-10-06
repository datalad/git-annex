#! /usr/bin/perl -w

use Getopt::Long;
use Pod::Usage;

my $help = 0;
my $usage = 0;
my $dryrun = 0;
my $verbose = 0;
my $path = '';
my $annex = '';
my $home = $ENV{'HOME'};

sub main() {
    checkargs();
    if (!$path) {
        $path = $home . '/.xbmc/userdata/Database';
    }
    print("# checking XBMC directory '$path'\n") if ($verbose);
    $dbpath = finddb($path);
    if (!$dbpath) {
        pod2usage("$0: can't find a XBMC database in '$path'.");
    }
    print("# using database '$dbpath'\n") if ($verbose);
    checkdb();
}

# list videos database, find the latest one
# modified version of
# http://stackoverflow.com/questions/4651092/getting-the-list-of-files-sorted-by-modification-date-in-perl
sub finddb($) {
    my $path = shift(@_);
    opendir my($dirh), $path or die "can't opendir $path: $!";
    my @flist = sort {  -M $a <=> -M $b } # Sort by modification time
        map  { "$path/$_" } # We need full paths for sorting
        grep { /^MyVideos.*\.db$/ }
        readdir $dirh;
    closedir $dirh;
    if ($#flist > 0) {
        return $flist[0];
    }
    else {
        return 0;
    }
}

sub checkargs() {
    pod2usage(1) if $help;
    pod2usage(-exitval => 0, -verbose => 2) if $usage;

    GetOptions('h|?' => \$help,
               'help|usage' => \$usage,
               # we want to operate on relative links, so set this to
               # the common annex to the git annex repo
               'annex=s' => \$annex,
               'path=s' => \$path,
               'home=s' => \$home,
               'dryrun|n' => \$dryrun,
               'verbose|v' => \$verbose,
        )
        or die("Error parsing commandline\n");
}

sub checkdb() {
    my @lines = `echo 'SELECT playCount, path.strPath, files.strFileName FROM movie JOIN files ON files.idFile=movie.idFile JOIN path ON path.idPath=files.idPath;' | sqlite3 $dbpath`;
    print "# finding files...\n" if $verbose;
    for (@lines) {
        my ($count, $dir, $file) = split /\|/;
        chomp $file;
        # empty or non-numeric count is zero
        if ($count !~ /[0-9]/) {
            $count = 0;
        }
        print "# $dir/$file\n" if $verbose;
        if ($file =~ s#stack://##) {
            for (split /,/, $file) {
                s/$annex//;
                s/^ //;
                s/ $//;
                my @cmd = (qw(git annex metadata --set), "playCount=$count", $_);
                if ($dryrun) {
                    print join(' ', @cmd) . "\n";
                }
                else {
                    system(@cmd);
                }
            }
        }
        else {
            $dir =~ s/$annex//;
            my @cmd = (qw(git annex metadata --set), "playCount=$count", "$dir$file");
            if ($dryrun) {
                print join(' ', @cmd) . "\n";
            }
            else {
                system(@cmd);
            }
        }
    }
}

main();

__END__
=encoding utf8

=head1 NAME

git-annex-xbmc-playcount - register XBMC playcounts as git-annex metadata

=head1 SYNOPSIS

git-annex-xbmc-playcount [--path .xbmc/userdata/Database]

 Options:
  -h         short usage
  --help     complete help
  --dryrun, -n do nothing and show the commands that would be ran
  --annex    path to the git-annex repo
  --home     the home directory where the .xbmc directory is located
  --path     the location of the Database directory of XBMC, overrides --home
  --verbose  show interaction details with the database

=head1 DESCRIPTION

This program will look into the XBMC database for the "playcount"
field to register that number as metadata in the git-annex repository.

=head1 OPTIONS

=over 8

=item B<--dryrun>

Do nothing but show all the steps that would be ran. The output can be
piped through a POSIX shell after inspection. B<-n> is an alias of
this command. Example:

    git-annex-xbmc-playcount -n | tee runme
    # inspect the output
    sh < runme

=item B<--annex>

This option allows the user to specify the root of the git-annex
repository, which is then stripped off the paths found in the XBMC
database.

=item B<--home>

Home of the user running XBMC. If not specified, defaults to the $HOME
environment variables. The script will look into
B<$home/.xbmc/userdata/Database> for a file matching
B<^MyVideos.*\.db$> and will fail if none is found.

=item B<--path>

Manually specify the path to B<.xbmc/userdata/Database>. This
overrides B<--home>.

Note that this doesn't point directly to the datbase itself, because
there are usually many database files and we want to automatically
find the latest. This may be a stupid limitation.

=item B<--verbose>

Show more information about path discovery. Doesn't obstruct
B<--dryrun> output because lines are prefixed with C<#>.

=back

=head1 EXAMPLES

You have a git annex in B</srv/video> and XBMC is ran as the
B<video> user and you want to be cautious:

    $ ./git-annex-xbmc-playcount.pl --home /home/video/ -n --annex /srv/video/ | tee set-metadata
    git annex metadata --set playCount=0 films/Animal.Farm.1954.DVDRip.DivX-MDX.avi

This looks about right, set the metadata:

    $ git annex metadata --set playCount=0 films/Animal.Farm.1954.DVDRip.DivX-MDX.avi
    metadata films/Animal.Farm.1954.DVDRip.DivX-MDX.avi
      lastchanged=2014-10-04@22-17-42
      playCount=0
      playCount-lastchanged=2014-10-04@22-17-42
    ok
    (Recording state in git...)

=head1 ENVIRONMENT

B<$HOME> is looked into to find the B<.xbmc> home directory if none of
B<--home> or B<--path> is specified.

=head1 FILES

=over 8

=item B<$HOME/.xbmc/userdata/Database/MyVideos.*\.db>

This is where we assume the SQLite database of videos XBMC uses is
stored.

=back

=head1 BUGS

If there are pipes (C<|>) in filenames, the script may fail to find
the files properly. We would need to rewrite the database code to use
B<DBD::SQLite>(3pm) instead of a pipe to B<sqlite3>(1).

=head1 LIMITATIONS

It took longer writing this help than writing the stupid script.

The script will not tag files not yet detected by XBMC.

The script is not incremental, so it will repeatedly add the same
counts to files it has already found.

=head1 SEE ALSO

B<git-annex>(1), B<xbmc>(1)

=head1 AUTHOR

Written by Antoine Beaupré <anarcat@debian.org>
