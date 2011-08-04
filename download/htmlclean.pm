package htmlclean;

# Copyright 2009, Christopher Wellons
#
# This library is free software; you can redistribute it and/or
# modify it under the same terms as Perl itself.

use warnings;
use strict;
use HTML::Parser;
use CGI qw(escapeHTML);

BEGIN {
    use Exporter ();
    our ( $VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS );

    $VERSION     = 1.00;
    @ISA         = qw(Exporter);
    @EXPORT      = qw(&clean_html);
    %EXPORT_TAGS = ();
    @EXPORT_OK   = qw();
}
our @EXPORT_OK;

# Settings
my @allowed_tags = qw(a b i code pre br);
my %allowed_tags = map { $_ => 1 } @allowed_tags;
my @open_tags    = ('p');

# Create a parser
my $p = HTML::Parser->new(
    start_h => [ \&tag_h,  "tagname,attr" ],
    end_h   => [ \&end_h,  "tagname" ],
    text_h  => [ \&text_h, "dtext" ],
);
my $clean;

# The actual html stripper
sub clean_html {
    $clean = "<p>\n";
    $p->parse(shift);
    $p->eof;
    $clean .= "</$_>" for (@open_tags);    # Close up the tags
    return $clean;
}

# Tag handler
sub tag_h {
    my $tag = shift;
    return if !$allowed_tags{$tag};
    my %attr   = %{ shift() };
    my $attr   = "";
    my $tagend = "";

    $attr{'/'}++ if $tag eq 'br';          # br tag always closes itself

    unshift @open_tags, $tag;              # make tag open

    # This tag has no end tag
    if ( $attr{'/'} ) {
        $tagend = " /";
        shift @open_tags;
    }

    # a tag is special
    if ( $tag eq 'a' ) {
        my $href = $attr{'href'};
        $attr = " href=\"$href\" rel=\"nofollow\"";
    }

    $clean .= "<" . $tag . $attr . $tagend . ">";
}

# End tag handler
sub end_h {
    my $tag = shift;
    return if !$allowed_tags{$tag};
    if ( $open_tags[0] eq $tag ) {

        # Only close the last open tag
        shift @open_tags;
        $clean .= "</" . $tag . ">";
    }
}

# Text handler
sub text_h {
    my $text = escapeHTML shift;
    $text =~ s/\r//g;
    $text =~ s/\n\n/\n<\/p><p>\n/g;
    $text = linkify($text) if $open_tags[0] ne 'a';
    $clean .= $text;
}

sub linkify {
    my $text = shift;
    $text =~ s!(http://\S+)!<a href="$1" rel="nofollow">$1</a>!g;
    return $text;
}

1;
