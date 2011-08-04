#!/usr/bin/perl
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see
# <http://www.gnu.org/licenses/>.

use warnings;
use strict;
use File::stat;
use CGI qw(:standard);

# Feeds we can handle
my %feeds = (
    'explosm' => [
        'http://explosm.com/comics/', '/db/files/Comics/',
        'Cyanide and Happiness',      2 * 60 * 60
    ],
    'darklegacy' => [
        'http://darklegacycomics.com/', '\d+\.jpg',
        'Dark Legacy Comics',           3 * 60 * 60
    ],
    'whiteninja' => [
        'http://whiteninjacomics.com/comics.shtml', '/images/comics/[^t][^-]',
        'white ninja comics',                       2 * 60 * 60
    ],
    'list' => ['dummy']
);

my $query = new CGI;
my $feed  = $query->param('q');
$feed =~ s/[^\w\/]//g;

list() if $feed eq 'list';
error404() if !$feeds{$feed};

# Generate feed if needed
system(
    "/usr/bin/perl",  "grab.pl", $feeds{$feed}[0],
    $feeds{$feed}[1], "$feed",   $feeds{$feed}[2]
  )
  if !-e "$feed.xml"
      or time - stat("$feed.xml")->mtime > $feeds{$feed}[3];

# Serve feed
print header( -type => 'text/xml' );
open my ($feedfile), "$feed.xml";
print while <$feedfile>;

sub list {
    print header();
    print <<EOF;
<html>
<head>
<title>RSS Feed List</title>
</head>
<body>
<h1>RSS Feeds</h1>
<ul>
EOF
    for my $feed ( keys %feeds ) {
        print "<li><a href=\"$feed\">$feeds{$feed}[2]</li>\n"
          if $feed ne 'list';
    }
    print <<EOF;
</ul>
</body>
</html>
EOF
    exit(0);
}

sub error404 {
    print header( -status => '404 Not Found' );
    print <<EOF;
<html>
<body>
<h1>404 Not Found</h1>
<p>
The feed "$feed" does not exist.
</p>
</body>
</html>
EOF
    exit(0);
}
