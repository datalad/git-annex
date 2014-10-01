#! /usr/bin/perl -w

# we want to operate on relative links, so set this to the common prefix
# to the git annex repo
my $prefix="/home/media/video/";
# this is the directory for the XBMC database
my $path = '/home/video/.xbmc/userdata/Database/';

# no user-serviceable parts below

# list videos database, find the latest one
# modified version of
# http://stackoverflow.com/questions/4651092/getting-the-list-of-files-sorted-by-modification-date-in-perl
opendir my($dirh), $path or die "can't opendir $path: $!";
my @flist = sort {  -M $a <=> -M $b } # Sort by modification time
            map  { "$path/$_" } # We need full paths for sorting
            grep { /^MyVideos.*\.db$/ }
            readdir $dirh;
closedir $dirh;

my $dbpath=$flist[0];

my @lines = `echo 'SELECT playCount, path.strPath, files.strFileName FROM movie JOIN files ON files.idFile=movie.idFile JOIN path ON path.idPath=files.idPath;' | sqlite3 $dbpath`;
for (@lines) {
    my ($count, $dir, $file) = split /\|/;
    chomp $file;
    # empty or non-numeric count is zero
    if ($count !~ /[0-9]/) {
        $count = 0;
    }
    if ($file =~ s#stack://##) {
        for (split /,/, $file) {
            s/$prefix//;
            s/^ //;
            s/ $//;
            my @cmd = (qw(git annex metadata --set), "playCount=$count", $_);
            system(@cmd);
        }
    }
    else {
        $dir =~ s/$prefix//;
        my @cmd = (qw(git annex metadata --set), "playCount=$count", "$dir$file");
        system(@cmd);
    }
}
