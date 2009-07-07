#!/usr/bin/perl
#Example: ./concordance.pl someword somefile.txt

$string = shift(@ARGV);
@lines = <>;
chomp(@lines);
$text = "@lines";
@pieces = split(/\b$string\b/i, $text);
foreach $n (0..(@pieces-2)) {
    print substr(substr($pieces[$n - 1], -30) . $string . $pieces[$n], -30);
    print $string;
    print substr($pieces[$n+1] . $string . substr($pieces[$n+2], 0, 30), 0, 30), "\n";
}
