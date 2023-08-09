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
    if ((target.getBoundingClientRect().bottom > window.innerHeight)
        || (target.getBoundingClientRect().top < 0)) {
        target.scrollIntoView({ behavior: "smooth",
                                block: "center",
                                inline: "center" });
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
    $("#text-table-of-contents a").click((e) => {
        var tocItem = get_toc_item(e.target.hash);
        $(tocItem).addClass("active");
        deactivate_other_toc_items(e.target.hash);
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

function is_external_link(destination) {
    return (destination[0] !== "#");
}

$(document).ready(() => {
    $("a").click((e) => {
        var destination = null;
        if (e.target.attributes.length > 0) {
            destination = e.target.attributes.href.nodeValue;
        } else {
            destination = e.target.parentElement.hash;
        }

        // Only disable the browser's "jump to the link immediately" behavior if
        // we are dealing with an intra-document link. For links to other pages,
        // we want the default behavior. The destination is empty if the link
        // goes to another page.
        if (is_external_link(destination)) {
            return;
        } else {
            e.preventDefault();
        }
        $(destination).addClass("active");
        deactivate_other_non_toc_items(destination);
        scrollIntoViewIfNeeded($(destination)[0]);
        // Save intra-document link into history, but only if it's not a repeat
        // of one already there.
        var hash = destination;
        if (history.state === null || history.state.hash != hash) {
            history.pushState( //ref:HISTORY_PUSHSTATE
                {hash: destination},
                "", destination);
        }
    });
});
$(document).ready(() => {
    history.scrollRestoration = "manual";
    window.addEventListener("popstate", function (e) {
        if (e.state === null) {
           return;
        }
        var hash = e.state.hash;
        e.preventDefault();
        scrollIntoViewIfNeeded($(hash)[0]);
        $(hash).addClass("active");
        deactivate_other_non_toc_items(hash);
    });
});
