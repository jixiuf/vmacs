use warnings;
use strict;
sub go_package_name {
    my ($path) = @_;
    unless (open(IN, $path)) {
        warn "open $: $.";
        return ''
    }
    my $package;
    while (<IN>) {
        if (/^package (\S+)/) {
            $package = $1;
            last
        }
    }
    close(IN);
    return $package;
}
sub has_prefix {
    my ($str, $prefix) = @_;
    return (substr($str, 0, length($prefix)) eq $prefix);
}
sub visit_dir {
    my ($dir, $packages, $visited) = @_;
    my ($dev, $ino) = stat($dir);
    my ($dev_ino) = "$dev/$ino";
    if (defined $visited->{$dev_ino}) {
        return
    }
    $visited->{$dev_ino} = 1;
    unless (opendir(DIR, $dir)) {
        warn "opendir $dir: $.\n";
        return
    }
    my @filenames = readdir(DIR);
    closedir(DIR);
    my $package = '';
    foreach my $filename (@filenames) {
        if (has_prefix($filename, ".") or has_prefix($filename, "bazel-")) {
            next;
        }
        my $path = "$dir/$filename";
        if (-d $path) {
            visit_dir($path, $packages, $visited);
            next
        }
        if ($path =~ /\.go$/ and $package eq '') {
            my $p = go_package_name($path);
            if ($p !~ /test$/ and $p ne '') {
                $package = $p;
            }
        }
    }
    if ($package ne '' and $package ne 'main' and $package ne 'p') {
        $packages->{$dir} = $package;
    }
}
for my $root (@ARGV) {
    my ($packages, $visited) = ({}, {});
    visit_dir("$root/src", $packages, $visited);
    while (my($dir, $name) = each %$packages) {
        my $path = $dir;
        if ($dir =~ m!/vendor/(.*)!) {
            $path = $1;
        } else {
            for my $root (@ARGV) {
                my $root_prefix = "$root/src/";
                if (has_prefix($dir, $root_prefix)) {
                    $path = substr($dir, length($root_prefix));
                    last
                }
            }
        }
        print "(go-imports-define-package \"$name\" \"$path\")\n"
    }
}
