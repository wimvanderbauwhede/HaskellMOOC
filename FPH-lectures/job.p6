use v6;
#use IO::Glob; # does not build!
use Inline::Perl5;
use File::Glob:from<Perl5> ':glob';

my @fns = glob('FP4*tex');
for @fns -> $fn {
#    say $fn;
    my $nfn = $fn;
    $nfn~~s/FP4\-2014\-15/FPH/;
    $nfn~~s/w(\d)(\d)/w$1{$2+1}/;
    $nfn~~s/\d(a)/-1/;
    $nfn~~s/\d(b)/-2/;
    say "mv $fn $nfn";
}
