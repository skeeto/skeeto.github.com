/* Container folding
 *
 * Folds up containers classed as foldable containing one h2 and
 * ul. On my blog, these are the asides containing lists of
 * links. Without JavaScript nothing ever gets hidden, so this will
 * degrade gracefully.
 */

$(document).ready(function() {
    var up = '&#x25B2;', down = '&#x25BC;';
    $('.foldable ul').slideToggle();
    $('.foldable h2').append($('<a>' + down + '</a>').attr({
        href: '#',
        'class': 'fold-button folded'
    }));
    $('.fold-button').bind('click', function () {
        var $this = $(this);
        $this.parent().closest('.foldable').find('ul').slideToggle();
        if ($this.hasClass('folded')) {
            $this.html(up);
            $this.removeClass('folded');
            $this.addClass('unfolded');
        } else {
            $this.html(down);
            $this.removeClass('unfolded');
            $this.addClass('folded');
        }
        return false;
    });
});
