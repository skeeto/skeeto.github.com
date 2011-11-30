/* Disables all themes. */
function disableAllThemes() {
    $(".theme[rel='stylesheet']").each(function() {
        $(this).attr("disabled", true);
    });
    $(".theme-button").each(function() {
        $(this).removeClass("selected");
    });
}

/* Set the theme by index number. */
function setTheme(index) {
    disableAllThemes();
    $(".theme[rel='stylesheet']").eq(index * 2).attr("disabled", false);
    $(".theme[rel='stylesheet']").eq(index * 2 + 1).attr("disabled", false);
    $(".theme-button").eq(index).addClass("selected");
    $.cookies.set("theme", index);
}

$("document").ready(function() {
    var save = $.cookies.get("theme");
    if (save) {
        setTheme(save);
    } else {
        setTheme(0); // Enable the default theme.
    }
});
