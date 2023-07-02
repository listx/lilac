$(function() {
    $("h2,h3,h4,h5,h6").each(function() {
        var $this = $(this);
        var href = $this.attr("id");
        $this.wrapInner(
            $("<a/>").attr("href", "#" + href)
                     .addClass("section-heading"));
    });
});
function scrollIntoViewIfNeeded(target) {
    if (target.getBoundingClientRect().bottom > window.innerHeight) {
        target.scrollIntoView({ behavior: "smooth",
                                block: "end",
                                inline: "nearest" });
    }

    if (target.getBoundingClientRect().top < 0) {
        target.scrollIntoView({ behavior: "smooth",
                                block: "start",
                                inline: "nearest" });
    }
}

function deactivate_other_toc_items(hash) {
    $("#text-table-of-contents a").each((index, elt) => {
        if (elt.hash !== hash) {
            $(elt).removeClass("active");
        }
    })
}

function get_toc_item(hash) {
    return $(`#text-table-of-contents a[href='${hash}']`)[0];
}

$(document).ready(() => {
    $("#text-table-of-contents a").click((elt) => {
        var tocItem = get_toc_item(elt.target.hash);
        $(tocItem).addClass("active");
        deactivate_other_toc_items(elt.target.hash);
    });

    $("*[id^='outline-container-h-']").each((index, elt) => {
        var hash = elt.getAttribute("id")
        hash = hash.replace("outline-container-", "#")
        var tocItem = get_toc_item(hash);
        elt.addEventListener("mouseover", () => {
            $(tocItem).addClass("active");
            deactivate_other_toc_items(hash);
        });
        elt.addEventListener("mouseover", (e) => {
            // If we don't call stopPropagation(), we end up scrolling *all*
            // elements in the stack, which means we will try to scroll e.g.,
            // Section 5 and Section 5.1.2.3 (when we only want the latter).
            e.stopPropagation();
            // Unfortunately, scrollIntoViewIfNeeded is not supported on
            // Firefox. Otherwise we could do
            //
            //    elems[0].scrollIntoViewIfNeeded({ block: "center" });
            //
            // instead. So here we call the custom function that does what we
            // want.  See https://stackoverflow.com/a/37829643/437583.
            scrollIntoViewIfNeeded(tocItem);
        });
    });
});
function deactivate_other_non_toc_items(hash) {
    $(".outline-2 *").each((index, elt) => {
        if (`#${elt.id}` !== hash) {
            $(elt).removeClass("active");
        }
    })
}

$(document).ready(() => {
    $("a").click((elt) => {
        var destination;
        if (elt.target.nodeName === "CODE") {
            destination = elt.target.parentElement.hash;
        } else {
            destination = elt.target.hash;
        }
        $(destination).addClass("active");
        deactivate_other_non_toc_items(destination);
        elt.preventDefault();
        scrollIntoViewIfNeeded($(destination)[0]);
    });
});
