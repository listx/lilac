$(function() {
    $("h2,h3,h4,h5,h6").each(function() {
        var $this = $(this);
        var href = $this.attr("id");
        $this.wrapInner(
            $("<a/>").attr("href", '#' + href)
                     .addClass("section-heading"));
    });
});

function activate_toc_item_on_click (item){
    $(item).toggleClass("active");
}

function scrollIntoViewIfNeeded(target) {
    if (target.getBoundingClientRect().bottom > window.innerHeight) {
        target.scrollIntoView(false);
    }

    if (target.getBoundingClientRect().top < 0) {
        target.scrollIntoView();
    }
}

$(document).ready(function() {
  $('#text-table-of-contents ul').first().addClass('nav');
  $('#text-table-of-contents ul li').addClass('nav-item');
  $('#text-table-of-contents ul li a').addClass('nav-link');

  const scrollSpy = new bootstrap.ScrollSpy(document.body, {
    smoothScroll: true,
    target: '#text-table-of-contents'
  })

  $("#text-table-of-contents a").click(function() {
    activate_toc_item_on_click(this);
  });

  const firstScrollSpyEl = document.querySelector('[data-bs-spy="scroll"]')
  firstScrollSpyEl.addEventListener('activate.bs.scrollspy', () => {
    let elems = document.getElementsByClassName('active');
    if (elems.length > 0) {
      // Unfortunately, scrollIntoViewIfNeeded is not supported on Firefox.
      // Otherwise we could do
      //
      //    elems[0].scrollIntoViewIfNeeded({ block: "center" });
      //
      // instead. So here we call the custom function that does what we want.
      // See https://stackoverflow.com/a/37829643/437583.
      scrollIntoViewIfNeeded(elems[0]);
    }
  })
});
