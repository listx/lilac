$(function() {
    $("h2,h3,h4,h5,h6").each(function() {
        var $this = $(this);
        var href = $this.attr("id");
        $this.wrapInner(
            $("<a/>").attr("href", '#' + href)
                     .addClass("section-heading"));
    });
});
