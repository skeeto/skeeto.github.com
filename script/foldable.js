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
        'class': 'fold-button folded',
        'href': '#'
    }));
    $('.fold-button').bind('click', function () {
        var $this = $(this);
        $this.parent().closest('.foldable').find('ul').slideToggle();
        if ($this.hasClass('folded')) {
            $this.html(up).removeClass('folded').addClass('unfolded');
        } else {
            $this.html(down).removeClass('unfolded').addClass('folded');
        }
        return false;
    });
});
