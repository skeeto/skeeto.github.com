/* Disables all themes. */
function disableAllThemes() {
    $(".theme[rel='stylesheet']").each(function() {
        $(this).attr("disabled", true);
    });
}

/* Set the theme by index number. */
function setTheme(index) {
    disableAllThemes();
    $(".theme[rel='stylesheet']").eq(index).attr("disabled", false);
}

$("document").ready(function() {
    setTheme(0); // Enable the default theme.
});
