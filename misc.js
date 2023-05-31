$(function() {
    $("h2,h3,h4,h5,h6").each(function() {
        var $this = $(this);
        var href = $this.attr("id");
        $this.wrapInner(
            $("<a/>").attr("href", '#' + href)
                     .addClass("section-heading"));
    });
});

function scrollIntoViewIfNeeded(target) {
    if (target.getBoundingClientRect().bottom > window.innerHeight) {
        target.scrollIntoView(false);
    }

    if (target.getBoundingClientRect().top < 0) {
        target.scrollIntoView();
    }
}

$(document).ready(() => {
  $('#text-table-of-contents ul').first().addClass('nav');
  $('#text-table-of-contents ul li').addClass('nav-item');
  $('#text-table-of-contents ul li a').addClass('nav-link');

  $("*[id^='outline-container-h-']").each((index, elt) => {
    var s = elt.getAttribute('id')
    s = s.replace('outline-container-', '')
    var tocItem = $(`#text-table-of-contents a[href='#${s}']`)[0];
    elt.addEventListener('mouseover', () => {
      $(tocItem).addClass("active");
    });
    elt.addEventListener('mouseover', (e) => {
      // If we don't call stopPropagation(), we end up scrolling *all*
      // elements in the stack, which means we will try to scroll e.g.,
      // Section 5 and Section 5.1.2.3 (when we only want the latter).
      e.stopPropagation();
      // Unfortunately, scrollIntoViewIfNeeded is not supported on Firefox.
      // Otherwise we could do
      //
      //    elems[0].scrollIntoViewIfNeeded({ block: "center" });
      //
      // instead. So here we call the custom function that does what we want.
      // See https://stackoverflow.com/a/37829643/437583.
      scrollIntoViewIfNeeded(tocItem);
    });
    elt.addEventListener('mouseout', () => {
      $(tocItem).removeClass("active");
    });
  });
});
