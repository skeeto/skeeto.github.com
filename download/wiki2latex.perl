#!/usr/bin/perl -w
# Copyright (c) 2007 Christopher Wellons <mosquitopsu@gmail.com>
# 
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
# 
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.

# Either provide the wiki markup as standard input or as an argument
# on the command line. The latex version is sent to standard output
#

my $print_outer = 1;  # Print header and footer material
my $listmode = 0;     # Are we in a list?

$title = "My Title";
$author = "My Name";
$date = "22 September 2007";

# Header
if ($print_outer){
    print "\\documentclass{article}\n\n";
    print "\\begin{document}\n\n";
    
    print "\\title{$title}\n";
    print "\\author{$author}\n";
    print "\\date{$date}\n";
    
    print "\\begin{titlepage}\n";
    print "\\maketitle\n";
    print "\\end{titlepage}\n\n";

    print "\\tableofcontents\n";
    print "\\pagebreak\n\n";
}

# Line by line conversion
while (<>) {
    # Sections
    s/======([^=]+)======/\\subparagraph{$1}/g;
    s/=====([^=]+)=====/\\paragraph{$1}/g;
    s/====([^=]+)====/\\subsubsection{$1}/g;
    s/===([^=]+)===/\\subsection{$1}/g;
    s/==([^=]+)==/\\section{$1}/g;
    
    # Special characters
    s/([&#_])/\\$1/g;
    
    # Lists
    if (m/^\* / && !$listmode) {
	print "\\begin{itemize}\n";
	$listmode = 1;
    }
    if (!m/^\* / && $listmode) {
	print "\\end{itemize}\n";
	$listmode = 0;
    }
    s/^\* /\\item{} /;
    
    print;
}

# End listmode
if ($listmode) {
    print "\\end{itemize}\n";
    $listmode = 0;
}

# Footer
if ($print_outer){
    print "\n\\end{document}\n";
}
