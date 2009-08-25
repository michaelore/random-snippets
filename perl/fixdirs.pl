#!/usr/bin/perl
#Eliminates \'s in filenames
my @files = `ls`;
foreach $file (@files) {
	@paths = split(/\\/, $file);
	$newfile = join("/", @paths);
	pop(@paths);
	$newpath = join("/", @paths);
	chomp($file);
        $file =~ s/\\/\\\\/g;
	`mkdir $newpath` if $newpath and not (-e $newpath);
	`mv $file $newfile`;
 }
