window.addEventListener('load', function() {
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
