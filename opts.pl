#!/usr/bin/perl

use warnings;
use strict;
use IPC::Open2;

my %graph = ();

# incdirs, ldopts are collected as hash keys to avoid duplicates
# and because their order does not matter — unlike the order of the libraries
my %incdirs = ();
my %ldopts = ();
my @libfiles;
my $ghcver = `ghc --numeric-version`;
chomp $ghcver;

sub add_to_graph($);
sub add_to_graph($) {
  my ($pkgname) = @_;

  return if exists $graph{$pkgname};

  open my $ghcpkg, "-|", "ghc-pkg", "field", "--simple-output",
    $pkgname, "depends,hs-libraries,extra-libraries,ld-options,library-dirs,dynamic-library-dirs,include-dirs";

  my %pkg;
  $pkg{'depends'} = [split ' ', <$ghcpkg>];
  my @hslibs = map {
      /^C/
        ? s/^C//r
        : ((($_ eq "HSrts") ? "HSrts_thr" : $_) . "-ghc$ghcver")
    } split ' ', <$ghcpkg>;
  my @extralibs = split ' ', <$ghcpkg>;
  my @libs = (@hslibs, @extralibs);
  $pkg{'libs'} = \@libs;

  $ldopts{$_}  = 1 for (map { s/^"(.*)"/$1/r; } split(' ', <$ghcpkg>));

  my @libdirs_static = split ' ', <$ghcpkg>;
  my @libdirs_dynamic = (split ' ', <$ghcpkg>);
  # the rts package info does not define dynamic-library-dirs
  unless (@libdirs_dynamic) { @libdirs_dynamic = @libdirs_static };
  for my $lib (@hslibs) {
    my $found = 0;
    my $libfile = "lib$lib.so";
    for my $libdir (@libdirs_dynamic) {
      my $path = "$libdir/$libfile";
      if (-e $path) {
        push @libfiles, $path;
        $found = 1;
        last;
      }
    }
    if (!$found) {
      local $,=", ";
      die "Library not found: $libfile\nDirectories searched: @libdirs_dynamic\n";
    }
  }
  $incdirs{$_} = 1 for (split ' ', <$ghcpkg>);
  close $ghcpkg;
  $graph{$pkgname} = \%pkg;

  add_to_graph($_) for @{$pkg{'depends'}};
}

my $root_pkg = $ARGV[0];
die "USAGE: $0 PKGNAME\n" unless $root_pkg;

add_to_graph $root_pkg;

local $\ = "\n";

system("mkdir", "-p", "lib");
for my $lib (@libfiles) {
  system("ln", "-sf", $lib, "lib/");
}

open INCOPTS, ">", "incopts.txt";
print INCOPTS "-I$_" for keys(%incdirs);
close INCOPTS;

my ($tsort_in, $tsort_out);
open2($tsort_out, $tsort_in, "tsort");
for my $pkgname (keys(%graph)) {
  for (@{$graph{$pkgname}->{'depends'}}) {
    printf $tsort_in "%s %s\n", $pkgname, $_;
  }
}
close $tsort_in;

open LDOPTS, ">", "ldopts.txt";

print LDOPTS for keys(%ldopts);

while (my $pkgname = <$tsort_out>) {
  chomp $pkgname;
  my %pkg = %{$graph{$pkgname}};
  print LDOPTS "-l$_" for @{$pkg{'libs'}};
}

close LDOPTS;
