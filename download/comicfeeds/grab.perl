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

use WWW::Mechanize;
use XML::RSS;
use strict;
use warnings;

die("Usage: grab.pl url pattern feedname feedtitle") if ( scalar @ARGV != 4 );

my $mech = WWW::Mechanize->new();

my $url       = $ARGV[0];
my $match     = $ARGV[1];
my $feedname  = $ARGV[2];
my $feedtitle = $ARGV[3];

$mech->get($url) or die("poop");

my $img = $mech->find_image( url_regex => qr/$match/i );

my $item    = $img->url_abs();
my $img_tag = '<img src="' . $item . '">';

# Do we need to write a new feed?
my $urlcache = "$feedname.url";
if ( -e $urlcache ) {
    open my ($cache), '<', $urlcache;
    my $oldurl = <$cache>;
    chomp($oldurl);
    exit(0) if $item eq $oldurl;
}

(
    my $Second,
    my $Minute,
    my $Hour,
    my $Day,
    my $Month,
    my $Year,
    my $WeekDay,
    my $DayOfYear,
    my $IsDST
) = localtime(time);
$Year += 1900;

my @days = ( "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" );
my @mon = (
    "Jan", "Feb", "Mar", "Apr", "May", "Jun",
    "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
);

$WeekDay = $days[$WeekDay];
$Month   = $mon[$Month];

my $date = "$WeekDay, $Day $Month $Year, "
  . sprintf( '%d:%02d:%02d PST', $Hour, $Minute, $Second );

my $rss = new XML::RSS( version => '2.0' );
$rss->channel(
    title       => $feedtitle,
    link        => $url,
    language    => 'en',
    description => 'Comic Feed',
    rating => '(PICS-1.1 "http://www.classify.org/safesurf/" 1 r (SS~~000 1))',
    copyright     => 'Copyright $url',
    pubDate       => $date,
    lastBuildDate => $date,
);

$rss->add_item(
    title       => "Cartoon for " . $date,
    permaLink   => $item,
    enclosure   => { url => $item, type => "application/x-data" },
    description => $img_tag
);

$rss->save("$feedname.xml");

# Write out the current URL
open my ($cache), '>', $urlcache;
print $cache $item;
