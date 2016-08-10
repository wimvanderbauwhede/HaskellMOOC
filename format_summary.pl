#!/usr/bin/env perl
use warnings;
use strict;



my $bullet='& & \LARGE{\color{navy}{\mathsf{\bullet \;\hbox{ ';
my $cont='& & \LARGE{\color{navy}{\mathsf{\; \; \hbox{ ';
my $lev2='& & \Large{\color{navy}{\mathsf{\; \; \hbox{ ';
my $end='}}}} \\\\'."\n";    

my $begineq = '
$$
\begin{eqnarray}
';

my $endeq='
\end{eqnarray}
$$
';

my $in_summary=0;
while(my $line=<>) {
    chomp $line;
    while ($line=~/\_([a-z:]+)_/) {

    if ($line=~/\_([a-z:]+)_/) {
my $word=$1;
my $word_it = '} \mathit{'.$word.'} \hbox{';
$line=~s/_${word}_/$word_it/;
}
}

    if ($line=~/^Summary/) {
        $in_summary=1;
        print "\n";
        print $begineq;
        next;
    } elsif ($line=~/^\#/ and $in_summary) {
        $in_summary=0;
        print $endeq;
        print "\n";
#        next;
    } elsif ($line=~/^\*/ ) {
        $line=~s/^\*\s+//;
        $line= $bullet.$line.$end;        
    } elsif( $line=~/^\-/) {
        $line=~s/^\-\s+//;
        $line= $lev2.$line.$end;
    } elsif ($line=~/^\ \ /) {
        $line= $cont.$line.$end;
    } else {
        print "\n";
#    next;
    }
    print $line;
}
#print $endeq;



