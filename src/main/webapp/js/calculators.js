/**
 * Set up handlers
 */
$(document).ready(function() {
    // Show the calculator summary
    $(".showDoc").click(function(){
        $(this).closest(".calculator").find(".doc").toggle();
    });

    // Show/hide more calculator documentation
    $(".showMoreDoc").click(function(){
        $(this).closest(".calculator").find(".description").toggle();
        var s = $(this).closest(".doc").find(".showMoreDoc").text();
        if ( s.substring(0,4).toLowerCase() == "more" ) {
            $(this).closest(".doc").find(".showMoreDoc").text("less...");
        } else {
            $(this).closest(".doc").find(".showMoreDoc").text("more...");
        }
    });

    // Show/hide the calculator body
    $(".titleBadge").click(function(){
        $(this).closest(".calculator").find(".body").toggle();
    });

    // Show/hide the calculator body
    $(".titleText").click(function(){
        $(this).closest(".calculator").find(".body").toggle();
    });
});

