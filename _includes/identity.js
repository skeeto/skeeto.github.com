window.addEventListener('load', function() {
    var addr = 'wellons\u0040nullprogram\u002ecom';
    var email = document.getElementById('email');
    email.href = 'mailto:' + addr;
    email.textContent = addr;

    var articles = document.getElementsByTagName('article');
    for (var i = 0; i < articles.length; i++) {
        var links = articles[i].querySelectorAll('p > a');
        var refs = articles[i].getElementsByClassName('references')[0];
        for (var j = 0; j < links.length; j++) {
            var ref = document.createElement('li');
            ref.textContent = links[j].href;
            refs.appendChild(ref);
            var cite = document.createElement('sup');
            cite.classList.add('print-only');
            cite.classList.add('cite');
            cite.textContent = j + 1;
            links[j].parentNode.insertBefore(cite, links[j].nextSibling);
        }
    }
});
