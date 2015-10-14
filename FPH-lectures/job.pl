use v5.16;

my @fns = glob('FP4*tex');
for my $fn (@fns) {
#    say $fn;
    my $nfn = $fn;
    $nfn=~s/FP4\-2014\-15/FPH/;
    if ($nfn=~/w(\d)(\d)([ab])/) {
        next if $1 eq '1';
        #say $1,$2,$3;
        my $w = $2;
        my $d="$1$2$3";
        my $n = $3 eq 'a' ? 1  : 2;
        $nfn=~s/$d/$w-$n/;
        }
#    $nfn=~s/\d(a)/-1/;
#    $nfn=~s/\d(b)/-2/;
    say "mv $fn $nfn";
    system( "mv $fn $nfn" );
}
