// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.lazy_scroll');
goog.require('cljs.core');
goog.require('cljs.core.constants');
goog.require('lilac.focus');
lilac.lazy_scroll.href__GT_id = (function lilac$lazy_scroll$href__GT_id(href){
return href.substring((1));
});
lilac.lazy_scroll.get_breadcrumbs_height = (function lilac$lazy_scroll$get_breadcrumbs_height(){
var or__4126__auto__ = (function (){var G__14319 = document;
var G__14319__$1 = (((G__14319 == null))?null:G__14319.querySelector(".breadcrumbs"));
var G__14319__$2 = (((G__14319__$1 == null))?null:G__14319__$1.parentElement);
if((G__14319__$2 == null)){
return null;
} else {
return G__14319__$2.offsetHeight;
}
})();
if(cljs.core.truth_(or__4126__auto__)){
return or__4126__auto__;
} else {
return (0);
}
});
lilac.lazy_scroll.visible_QMARK_ = (function lilac$lazy_scroll$visible_QMARK_(elt){
var rect = elt.getBoundingClientRect();
var bch = lilac.lazy_scroll.get_breadcrumbs_height();
var rect_top = (rect.top - bch);
var rect_bottom = (rect.bottom - bch);
var window_height = window.innerHeight;
var is_visible = (((rect_top < window_height)) && ((rect_bottom > (0))));
return is_visible;
});
lilac.lazy_scroll.maybe_scroll = (function lilac$lazy_scroll$maybe_scroll(elt){
if(lilac.lazy_scroll.visible_QMARK_(elt)){
return null;
} else {
return elt.scrollIntoView();
}
});
lilac.lazy_scroll.maybe_scroll_by_hash = (function lilac$lazy_scroll$maybe_scroll_by_hash(_popstate_event){
var href = window.location.hash;
var id = lilac.lazy_scroll.href__GT_id(href);
var temp__5825__auto__ = document.getElementById(id);
if(cljs.core.truth_(temp__5825__auto__)){
var target_element = temp__5825__auto__;
lilac.focus.activate(target_element);

return lilac.lazy_scroll.maybe_scroll(target_element);
} else {
return null;
}
});
lilac.lazy_scroll.weaken_anchor_links = (function lilac$lazy_scroll$weaken_anchor_links(){
var links = document.querySelectorAll("a[href^=\"#\"]");
var seq__14320 = cljs.core.seq(links);
var chunk__14321 = null;
var count__14322 = (0);
var i__14323 = (0);
while(true){
if((i__14323 < count__14322)){
var link = chunk__14321.cljs$core$IIndexed$_nth$arity$2(null,i__14323);
link.addEventListener("click",((function (seq__14320,chunk__14321,count__14322,i__14323,link,links){
return (function (event){
var href = link.getAttribute("href");
var id = lilac.lazy_scroll.href__GT_id(href);
var old_href = location.hash;
var temp__5825__auto__ = document.getElementById(id);
if(cljs.core.truth_(temp__5825__auto__)){
var target_element = temp__5825__auto__;
event.preventDefault();

if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(href,old_href)){
if(cljs.core.truth_(target_element.classList.contains("active"))){
return lilac.focus.deactivate_element(target_element);
} else {
return lilac.focus.activate_element(target_element);
}
} else {
window.history.pushState(null,null,href);

return lilac.lazy_scroll.maybe_scroll_by_hash(null);
}
} else {
return null;
}
});})(seq__14320,chunk__14321,count__14322,i__14323,link,links))
);


var G__14324 = seq__14320;
var G__14325 = chunk__14321;
var G__14326 = count__14322;
var G__14327 = (i__14323 + (1));
seq__14320 = G__14324;
chunk__14321 = G__14325;
count__14322 = G__14326;
i__14323 = G__14327;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14320);
if(temp__5825__auto__){
var seq__14320__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14320__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14320__$1);
var G__14328 = cljs.core.chunk_rest(seq__14320__$1);
var G__14329 = c__4556__auto__;
var G__14330 = cljs.core.count(c__4556__auto__);
var G__14331 = (0);
seq__14320 = G__14328;
chunk__14321 = G__14329;
count__14322 = G__14330;
i__14323 = G__14331;
continue;
} else {
var link = cljs.core.first(seq__14320__$1);
link.addEventListener("click",((function (seq__14320,chunk__14321,count__14322,i__14323,link,seq__14320__$1,temp__5825__auto__,links){
return (function (event){
var href = link.getAttribute("href");
var id = lilac.lazy_scroll.href__GT_id(href);
var old_href = location.hash;
var temp__5825__auto____$1 = document.getElementById(id);
if(cljs.core.truth_(temp__5825__auto____$1)){
var target_element = temp__5825__auto____$1;
event.preventDefault();

if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(href,old_href)){
if(cljs.core.truth_(target_element.classList.contains("active"))){
return lilac.focus.deactivate_element(target_element);
} else {
return lilac.focus.activate_element(target_element);
}
} else {
window.history.pushState(null,null,href);

return lilac.lazy_scroll.maybe_scroll_by_hash(null);
}
} else {
return null;
}
});})(seq__14320,chunk__14321,count__14322,i__14323,link,seq__14320__$1,temp__5825__auto__,links))
);


var G__14332 = cljs.core.next(seq__14320__$1);
var G__14333 = null;
var G__14334 = (0);
var G__14335 = (0);
seq__14320 = G__14332;
chunk__14321 = G__14333;
count__14322 = G__14334;
i__14323 = G__14335;
continue;
}
} else {
return null;
}
}
break;
}
});
lilac.lazy_scroll.init = (function lilac$lazy_scroll$init(){
lilac.lazy_scroll.weaken_anchor_links();

window.addEventListener("popstate",lilac.lazy_scroll.maybe_scroll_by_hash);

return lilac.lazy_scroll.maybe_scroll_by_hash(null);
});
