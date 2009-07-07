#!/usr/bin/perl
#Zip archives made in Windows don't unzip correctly in Unix;
# the subdirecteries have \'s instead of /'s.
#This script fixes the subdirecteries when run inside the
# folder where the archived was unzipped to.
my @files = `ls`;
foreach $file (@files) {
	@paths = split(/\\/, $file);
	$newfile = join("/", @paths);
	pop(@paths);
	$newpath = join("/", @paths);
	$file =~ s/\\/\\\\/g;
	chomp($file);
	print "mv $file $newfile";
	`mkdir $newpath` if $newpath and (not (-e $newpath));
	`mv $file $newfile` if not ($newfile == $file);
 }
