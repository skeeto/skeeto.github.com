/* Post frequency histogram plotter. */
$(document).ready(function() {
    /* Gather post dates. */
    var MONTH = 60 * 60 * 24 * 30 * 1000;
    var dates = $.map($('.archive time'), function(e) {
        var midnight = new Date(e.getAttribute('datetime')).valueOf();
        return 12 * 60 * 60 * 1000 + midnight;
    }).reverse();
    var histo = [];

    /* Compute histogram of posting frequency per month. */
    for (var p = dates[0] + MONTH; p <= dates.slice(-1)[0]; p += MONTH) {
        histo.push(dates.filter(function(date) {
            return date >= p - MONTH && date < p;
        }).length);
    }
    var max = Math.max.apply(null, histo);

    /* Setup the canvas. */
    var width = 600, height = 60, pad = 10;
    var div = $('<div/>')
            .css('width', width + 'px')
            .css('margin-left', 'auto')
            .css('margin-right', 'auto');
    var $canvas = $('<canvas/>')
            .attr({width: width, height: height});
    div.append($canvas);
    $('article h2').first().after(div);
    var ctx = $canvas.get(0).getContext('2d');

    /* Plotting functions. */
    function h(i) {
        return (height - pad * 2) * (max - histo[i]) / max + pad;
    }
    function w(i) {
        return (width  - pad * 2) * i / histo.length + pad;
    }

    /* Draw month markers. */
    ctx.strokeStyle = '#CCC';
    ctx.lineWidth = 1;
    for (i = 1; i < histo.length - 1; i++) {
        var scale = Math.abs(i / histo.length - 0.5) * Math.PI / 2;
        var v = Math.sin(Math.pow(scale, 2)) * height;
        ctx.beginPath();
        ctx.moveTo(w(i), 0 + v);
        ctx.lineTo(w(i), height - v);
        ctx.stroke();
    }

    /* Draw the trendline. */
    ctx.strokeStyle = 'black';
    ctx.lineWidth = 2;
    ctx.beginPath();
    ctx.moveTo(w(0), h(0));
    for (var i = 1; i < histo.length; i++) {
        ctx.lineTo(w(i), h(i));
    }
    ctx.stroke();

    /* Final red dot (sparkline-ish). */
    ctx.fillStyle = 'red';
    ctx.beginPath();
    ctx.arc(w(histo.length - 1), h(histo.length - 1), 3, 0, 2 * Math.PI);
    ctx.fill();
});
