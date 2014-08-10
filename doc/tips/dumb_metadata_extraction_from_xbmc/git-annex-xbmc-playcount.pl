#! /usr/bin/perl -w

my $dbpath="/home/video/.xbmc/userdata/Database/MyVideos75.db";
my $prefix="/home/media/video/";

my @lines = `echo 'SELECT playCount, path.strPath, files.strFileName FROM movie JOIN files ON files.idFile=movie.idFile JOIN path ON path.idPath=files.idPath;' | sqlite3 $dbpath`;
for (@lines) {
    my ($count, $dir, $file) = split /\|/;
    chomp $file;
    # empty or non-numeric count is zero
    if ($count !~ /[0-9]/) {
        $count = 0;
    }
    $dir =~ s/$prefix//;
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
        my @cmd = (qw(git annex metadata --set), "playCount=$count", "$dir$file");
        system(@cmd);
    }
}
