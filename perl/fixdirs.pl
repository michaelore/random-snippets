#!/usr/bin/perl
#Transforms \'s in pathnames into /'s, creating subdirectories
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
