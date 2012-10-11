#!/usr/bin/perl
# Simple script that feeds lines from stdin into OS X's speech synth

while (<>) {
  system("say", "$_");
}
