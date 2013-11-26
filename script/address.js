/* Fills in various address information on the page.
 */

$(document).ready(function() {
    var email = 'wellons\u0040nullprogram.com';
    $('#email').attr('href', 'mailto:' + email).text(email);
});
