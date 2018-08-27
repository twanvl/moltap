#!/usr/bin/perl

# ------------------------------------------------------------
#
# This script converts text files to html pages with nice markup
#
# ------------------------------------------------------------

use strict;
use warnings "all";
use FileHandle;

require "format.pl";
require "format_tex.pl";

# ------------------------------------------------------------
# Config

our $indir  = 'doc-src';
our $outdir = 'dist/website';
our $tempdir = 'dist/build/website';

our $full_build = (join '',@ARGV) =~ /--full/;
print "Performing full rebuild of documentation\n" if $full_build;

# base url
#our $baseurl = "";

# variables
use vars qw! $title $category $body $filename !;

# ------------------------------------------------------------
# Templating

my $fh = new FileHandle;
my $template = join '', <$fh> if $fh->open("< $indir/template.html");

sub interpolate {
	my $template = shift;
	$template =~ s/(\$\w+(?:::)?\w*)/"defined $1 ? $1 : ''"/gee;
	$template =~ s/IF\|(.*?)=(.*?)\|(.*?)\|(.*?)\|/($1 eq $2) ? $3 : $4/ge;
	$template =~ s/href="$filename.html"/$& class="current"/g;
	return $template;
};  

sub convert {
	my $abs_filename = shift;
	$filename = $abs_filename; $filename =~ s@.*/@@;
	# Open & parse file
	open IN,"< $abs_filename.txt";
	my @file = <IN>;
	close IN;
	@file = ('MISSING','') if @file==0;
	$title    = shift(@file);
	$category = shift(@file);
	$body = join('',@file);
	$title    =~ s/\s$//sg; #trim
	$category =~ s/\s$//sg; #trim
	$body = format::my_format($body);
	return interpolate($template);
}

sub convert_tex {
	my $abs_filename = shift;
	# Open & parse file
	open IN,"< $abs_filename.txt";
	my @file = <IN>;
	close IN;
	@file = ('MISSING','') if @file==0;
	$title    = shift(@file);
	$category = shift(@file);
	$body = join('',@file);
	$title    =~ s/\s$//sg; #trim
	$category =~ s/\s$//sg; #trim
	$body = format_tex::format($body);
}

# ------------------------------------------------------------
# For all files

use File::Find;
use File::Copy;
find({wanted => \&doit, no_chdir => 1}, $indir);

sub doit {
	my $file = $File::Find::name;
	return if ($file =~ /\.svn/);
	$file =~ s@$indir/@@;
	
	if ($file =~ /\.css|\.js|\.png|\.gif/) {
		copy("$indir/$file", "$outdir/$file");
		print "  $file\n";
		return;
	} if (!($file =~ /\.txt$/)) {
		return;
	}
	
	# convert text file
	$file  =~ s@\.txt@@;
	
	if (-e "$outdir/$file.html" and -M "$outdir/$file.html" < -M "$indir/$file.txt" and not $full_build) {
		# already exists, skip
		print "  $file    \t[skip]\n";
	} else {
		print "  $file\n";
		my $result = convert("$indir/$file");
		open OUT,"> $outdir/$file.html";
		print OUT $result;
		close OUT;
		# Latex output
		$result = convert_tex("$indir/$file");
		open OUT,"> $tempdir/page-$file.tex";
		print OUT $result;
		close OUT;
	}
}

# version file
print "  version.js\n";
use Time::localtime;
open OUT,"> $outdir/version.js";
printf OUT "version=\"%04d-%02d-%02d\";", localtime->year()+1900, localtime->mon()+1, localtime->mday();
close OUT;

__END__
