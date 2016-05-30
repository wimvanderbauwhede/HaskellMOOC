#!/usr/bin/perl
use v5.16;
use warnings;
use strict;

if (!@ARGV) {die "please provude the Latex source file name.\n"; }
# ./convert_to_md.pl FPH-lec_w1-2.tex
my $tex_fn = $ARGV[0];
my $md_fn= $tex_fn;
$md_fn=~s/\.tex/\.md/;
system("pandoc --latexmathml -f latex -t markdown -o tmp.md $tex_fn");

my $fin_src='';
open my $TMP, '<', 'tmp.md';
while (my $line=<$TMP>) {
    $line=~/\#+\s+Topics/ && next;
    $line=~/\[fragile/ && next;
    $line=~/\[image/ && next;
    # a bit weak but we're not aiming for perfection
    $line=~s/\$/\$\$/g;
      $fin_src.= $line;
}
close $TMP;



open my $FIN, '>', $md_fn;
print $FIN $fin_src;
close $FIN;
