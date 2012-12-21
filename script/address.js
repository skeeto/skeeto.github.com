/* Fills in various address information on the page.
 */

$(document).ready(function() {
    var email = 'mosquitopsu\u0040gmail.com';
    $('#email').attr('href', 'mailto:' + email).text(email);

    var bitcoin = '17YcJGMHjeTqEE9k1XKnC2PENBAtuoz8GQ';
    $('#bitcoin-qrcode').qrcode({height: 125, width: 125, text: bitcoin});
    $('#bitcoin-address').text(bitcoin);
});
