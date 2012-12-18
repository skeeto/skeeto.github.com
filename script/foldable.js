/* Container folding
 *
 * Folds up containers classed as foldable containing one h2 and
 * ul. On my blog, these are the asides containing lists of
 * links. Without JavaScript nothing ever gets hidden, so this will
 * degrade gracefully.
 */

$(document).ready(function() {
    var up = '&#x25B2;', down = '&#x25BC;';
    $('.foldable ul').toggle();
    $('.foldable h2').append($('<a>' + down + '</a>').attr({
        'class': 'fold-button',
        'href': '#'
    }));
    $('.fold-button').bind('click', function () {
        var ul = $(this).parent().closest('.foldable').find('ul');
        this.innerHTML = ul.css('display') === 'none' ? up : down;
        ul.slideToggle();
        return false;
    });
});
