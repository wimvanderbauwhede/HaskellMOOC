#!/usr/bin/perl
 for my $n ( 1 .. 6) {
for my $ab (1,2) {
system("pdflatex FPH-lec_w${n}-$ab.tex");
system("pdflatex FPH-lec_w${n}-$ab.tex");
system('rm  *.aux *.out *.log *.snm *.vrb *.nav *.toc');
}}
