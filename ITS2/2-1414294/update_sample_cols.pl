#!/usr/bin/perl
use strict;
use warnings;

my $mappingfile = shift || 'sample2peayID.tsv';
my $otu = shift || 'otu.amptk.otu_table.taxonomy.txt';
open(my $fh => $mappingfile) || die $!;
my $header = <$fh>;
my %lookup;
while(<$fh>) {
    chomp;
    my ($sampid,$ourId) = split(/\t/,$_);
    $lookup{$sampid} = $ourId;
}

open($fh => $otu) || die $!;
my $otu_header = <$fh>;
chomp($otu_header);
my ($sample,@samples) = split(/\t/,$otu_header);
my $nohit = pop @samples;
my $taxonomy = pop @samples;

my @new_header = ($sample);
for my $s ( @samples ) {
    my ($sid) = $s;
    $sid =~ s/SAMPLE_//;
		 
    if ( exists $lookup{$sid} ) {
	push @new_header, $lookup{$sid};
    } else {
	warn("cannot find $sid in lookup table ($mappingfile)\n");
	push @new_header,$sid;
    }
}
print join("\t", @new_header,$taxonomy,$nohit),"\n";
while(<$fh>) {
    # no need to fix any of the rows
    print;
}

