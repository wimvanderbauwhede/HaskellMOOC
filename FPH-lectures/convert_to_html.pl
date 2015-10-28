#!/usr/bin/perl
use v5.16;
use warnings;
use strict;

if (!@ARGV) {die "please provude the Latex source file name.\n"; }

my $tex_fn = $ARGV[0];
my $html_fn= $tex_fn;
$html_fn=~s/\.tex/\.html/;
system("pandoc --latexmathml -f latex -t html -o tmp.html $tex_fn");

my $fin_src='';
open my $TMP, '<', 'tmp.html';
while (my $line=<$TMP>) {
    $line=~/<h3 id=.topics.>Topics<.h3>/ && next;
    $line=~/\[fragile/ && next;
    $line=~/\<img/ && next;
    #$line=~s/\$/\$\$/g;

    $line=~s/\<span\ class=.LaTeX.\>\$/\\(/g;
    $line=~s/\$\<\/span\>/\\)/g;
    $line=~s/code\>\-/code>--/;
#    $line=~s/.backslash\s+/$bs/g;

#    $line=~s/.rightarrow/$rightarow_ent/g;
#    $line=~s/\\,/ /g;
#    $line=~s/\$.tt\{//;
#    $line=~s/\}\$//;
#    if ($line=~/\+  \+/) {say $line;

#    $line=~s/\s*\+  \+\s*/++/;
#}
#    $\tt{f \, =\, \backslash x \rightarrow x+1}$
    $fin_src.= $line;
}
close $TMP;



open my $SKEL, '<', 'FPH-lec_skeleton.html';
open my $FIN, '>', $html_fn;
while (my $line=<$SKEL>) {
    $line=~/__BODY__/ && do {
    print $FIN $fin_src;
    next;
    };
    print $FIN $line;
}
close $SKEL;
close $FIN;
