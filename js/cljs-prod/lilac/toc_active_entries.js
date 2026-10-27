// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.toc_active_entries');
goog.require('cljs.core');
goog.require('cljs.core.constants');
goog.require('clojure.walk');
goog.require('clojure.set');
if((typeof lilac !== 'undefined') && (typeof lilac.toc_active_entries !== 'undefined') && (typeof lilac.toc_active_entries.visible_headings !== 'undefined')){
} else {
lilac.toc_active_entries.visible_headings = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(cljs.core.PersistentHashSet.EMPTY);
}
lilac.toc_active_entries.get_heading_ancestry = (function lilac$toc_active_entries$get_heading_ancestry(){
var temp__5823__auto__ = document.querySelector("#heading-ancestry");
if(cljs.core.truth_(temp__5823__auto__)){
var script_tag = temp__5823__auto__;
var json_string = script_tag.textContent;
var js_object = JSON.parse(json_string);
return clojure.walk.keywordize_keys(cljs.core.js__GT_clj.cljs$core$IFn$_invoke$arity$1(js_object));
} else {
console.error("Warning: Heading ancestry data not found; using empty map.");

return cljs.core.PersistentArrayMap.EMPTY;
}
});
lilac.toc_active_entries.update_visible_headings = (function lilac$toc_active_entries$update_visible_headings(entries){
var seq__14480 = cljs.core.seq(entries);
var chunk__14481 = null;
var count__14482 = (0);
var i__14483 = (0);
while(true){
if((i__14483 < count__14482)){
var entry = chunk__14481.cljs$core$IIndexed$_nth$arity$2(null,i__14483);
var heading_container_id_14484 = entry.target.id;
var heading_id_14485 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(heading_container_id_14484,(18));
var visible_QMARK__14486 = entry.isIntersecting;
var modifier_14487 = (cljs.core.truth_(visible_QMARK__14486)?cljs.core.conj:cljs.core.disj);
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(lilac.toc_active_entries.visible_headings,modifier_14487,heading_id_14485);


var G__14488 = seq__14480;
var G__14489 = chunk__14481;
var G__14490 = count__14482;
var G__14491 = (i__14483 + (1));
seq__14480 = G__14488;
chunk__14481 = G__14489;
count__14482 = G__14490;
i__14483 = G__14491;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14480);
if(temp__5825__auto__){
var seq__14480__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14480__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14480__$1);
var G__14492 = cljs.core.chunk_rest(seq__14480__$1);
var G__14493 = c__4556__auto__;
var G__14494 = cljs.core.count(c__4556__auto__);
var G__14495 = (0);
seq__14480 = G__14492;
chunk__14481 = G__14493;
count__14482 = G__14494;
i__14483 = G__14495;
continue;
} else {
var entry = cljs.core.first(seq__14480__$1);
var heading_container_id_14496 = entry.target.id;
var heading_id_14497 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(heading_container_id_14496,(18));
var visible_QMARK__14498 = entry.isIntersecting;
var modifier_14499 = (cljs.core.truth_(visible_QMARK__14498)?cljs.core.conj:cljs.core.disj);
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(lilac.toc_active_entries.visible_headings,modifier_14499,heading_id_14497);


var G__14500 = cljs.core.next(seq__14480__$1);
var G__14501 = null;
var G__14502 = (0);
var G__14503 = (0);
seq__14480 = G__14500;
chunk__14481 = G__14501;
count__14482 = G__14502;
i__14483 = G__14503;
continue;
}
} else {
return null;
}
}
break;
}
});
lilac.toc_active_entries.update_toc_highlights = (function lilac$toc_active_entries$update_toc_highlights(ancestry,visible_headings){
var toc_entries = document.querySelectorAll(".toc-entry");
var toc_container = document.querySelector(".toc");
var visible_headings_with_ancestors = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3((function (acc,id){
var ancestors = cljs.core.set(cljs.core.get.cljs$core$IFn$_invoke$arity$3(ancestry,cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(id),cljs.core.PersistentVector.EMPTY));
return clojure.set.union.cljs$core$IFn$_invoke$arity$2(acc,ancestors);
}),visible_headings,visible_headings);
var seq__14504 = cljs.core.seq(toc_entries);
var chunk__14505 = null;
var count__14506 = (0);
var i__14507 = (0);
while(true){
if((i__14507 < count__14506)){
var toc_entry = chunk__14505.cljs$core$IIndexed$_nth$arity$2(null,i__14507);
var temp__5825__auto___14508 = toc_entry.querySelector("a");
if(cljs.core.truth_(temp__5825__auto___14508)){
var toc_entry_link_14509 = temp__5825__auto___14508;
var heading_14510 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(toc_entry_link_14509.hash,(1));
var class_list_14511 = toc_entry.classList;
if(cljs.core.contains_QMARK_(visible_headings,heading_14510)){
class_list_14511.add("active");

class_list_14511.remove("active-ancestor");
} else {
if(cljs.core.contains_QMARK_(visible_headings_with_ancestors,heading_14510)){
class_list_14511.add("active-ancestor");

class_list_14511.remove("active");
} else {
class_list_14511.remove("active");

class_list_14511.remove("active-ancestor");

}
}
} else {
}


var G__14512 = seq__14504;
var G__14513 = chunk__14505;
var G__14514 = count__14506;
var G__14515 = (i__14507 + (1));
seq__14504 = G__14512;
chunk__14505 = G__14513;
count__14506 = G__14514;
i__14507 = G__14515;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14504);
if(temp__5825__auto__){
var seq__14504__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14504__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14504__$1);
var G__14516 = cljs.core.chunk_rest(seq__14504__$1);
var G__14517 = c__4556__auto__;
var G__14518 = cljs.core.count(c__4556__auto__);
var G__14519 = (0);
seq__14504 = G__14516;
chunk__14505 = G__14517;
count__14506 = G__14518;
i__14507 = G__14519;
continue;
} else {
var toc_entry = cljs.core.first(seq__14504__$1);
var temp__5825__auto___14520__$1 = toc_entry.querySelector("a");
if(cljs.core.truth_(temp__5825__auto___14520__$1)){
var toc_entry_link_14521 = temp__5825__auto___14520__$1;
var heading_14522 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(toc_entry_link_14521.hash,(1));
var class_list_14523 = toc_entry.classList;
if(cljs.core.contains_QMARK_(visible_headings,heading_14522)){
class_list_14523.add("active");

class_list_14523.remove("active-ancestor");
} else {
if(cljs.core.contains_QMARK_(visible_headings_with_ancestors,heading_14522)){
class_list_14523.add("active-ancestor");

class_list_14523.remove("active");
} else {
class_list_14523.remove("active");

class_list_14523.remove("active-ancestor");

}
}
} else {
}


var G__14524 = cljs.core.next(seq__14504__$1);
var G__14525 = null;
var G__14526 = (0);
var G__14527 = (0);
seq__14504 = G__14524;
chunk__14505 = G__14525;
count__14506 = G__14526;
i__14507 = G__14527;
continue;
}
} else {
return null;
}
}
break;
}
});
lilac.toc_active_entries.maybe_scroll_toc = (function lilac$toc_active_entries$maybe_scroll_toc(visible_headings){
var toc_entries = document.querySelectorAll(".toc-entry");
var toc_container = document.querySelector(".toc");
var all_active_entries = cljs.core.filter.cljs$core$IFn$_invoke$arity$2((function (toc_entry){
var temp__5825__auto__ = toc_entry.querySelector("a");
if(cljs.core.truth_(temp__5825__auto__)){
var toc_entry_link = temp__5825__auto__;
return cljs.core.contains_QMARK_(visible_headings,cljs.core.subs.cljs$core$IFn$_invoke$arity$2(toc_entry_link.hash,(1)));
} else {
return null;
}
}),toc_entries);
var first_active = cljs.core.first(all_active_entries);
var last_active = cljs.core.last(all_active_entries);
if(cljs.core.seq(all_active_entries)){
var toc_rect = toc_container.getBoundingClientRect();
var first_active_rect = first_active.getBoundingClientRect();
var last_active_rect = last_active.getBoundingClientRect();
if((first_active_rect.top < toc_rect.top)){
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(first_active,cljs.core.first(toc_entries))){
return toc_container.scrollTo(({"top": (0), "behavior": "instant"}));
} else {
return first_active.scrollIntoView(({"behavior": "instant", "block": "start"}));
}
} else {
if((last_active_rect.bottom > toc_rect.bottom)){
if(cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(last_active,cljs.core.last(toc_entries))){
return toc_container.scrollTo(({"top": toc_container.scrollHeight, "behavior": "instant"}));
} else {
return last_active.scrollIntoView(({"behavior": "instant", "block": "end"}));
}
} else {
return null;

}
}
} else {
return null;
}
});
lilac.toc_active_entries.init = (function lilac$toc_active_entries$init(){
var heading_ancestry = lilac.toc_active_entries.get_heading_ancestry();
cljs.core.add_watch(lilac.toc_active_entries.visible_headings,cljs.core.cst$kw$lilac_DASH_toc_DASH_updater,(function (_key,atom,_old_state,_new_state){
lilac.toc_active_entries.update_toc_highlights(heading_ancestry,cljs.core.deref(atom));

return lilac.toc_active_entries.maybe_scroll_toc(cljs.core.deref(atom));
}));

var options_14536 = ({"rootMargin": "0px 0px 0px 0px", "threshold": (0)});
var heading_containers_14537 = document.querySelectorAll(".heading-container");
var observer_14538 = (new IntersectionObserver(lilac.toc_active_entries.update_visible_headings,options_14536));
var seq__14528_14539 = cljs.core.seq(heading_containers_14537);
var chunk__14529_14540 = null;
var count__14530_14541 = (0);
var i__14531_14542 = (0);
while(true){
if((i__14531_14542 < count__14530_14541)){
var heading_container_14543 = chunk__14529_14540.cljs$core$IIndexed$_nth$arity$2(null,i__14531_14542);
observer_14538.observe(heading_container_14543);


var G__14544 = seq__14528_14539;
var G__14545 = chunk__14529_14540;
var G__14546 = count__14530_14541;
var G__14547 = (i__14531_14542 + (1));
seq__14528_14539 = G__14544;
chunk__14529_14540 = G__14545;
count__14530_14541 = G__14546;
i__14531_14542 = G__14547;
continue;
} else {
var temp__5825__auto___14548 = cljs.core.seq(seq__14528_14539);
if(temp__5825__auto___14548){
var seq__14528_14549__$1 = temp__5825__auto___14548;
if(cljs.core.chunked_seq_QMARK_(seq__14528_14549__$1)){
var c__4556__auto___14550 = cljs.core.chunk_first(seq__14528_14549__$1);
var G__14551 = cljs.core.chunk_rest(seq__14528_14549__$1);
var G__14552 = c__4556__auto___14550;
var G__14553 = cljs.core.count(c__4556__auto___14550);
var G__14554 = (0);
seq__14528_14539 = G__14551;
chunk__14529_14540 = G__14552;
count__14530_14541 = G__14553;
i__14531_14542 = G__14554;
continue;
} else {
var heading_container_14555 = cljs.core.first(seq__14528_14549__$1);
observer_14538.observe(heading_container_14555);


var G__14556 = cljs.core.next(seq__14528_14549__$1);
var G__14557 = null;
var G__14558 = (0);
var G__14559 = (0);
seq__14528_14539 = G__14556;
chunk__14529_14540 = G__14557;
count__14530_14541 = G__14558;
i__14531_14542 = G__14559;
continue;
}
} else {
}
}
break;
}

var toc_entry_links = document.querySelectorAll(".toc-entry a");
var seq__14532 = cljs.core.seq(toc_entry_links);
var chunk__14533 = null;
var count__14534 = (0);
var i__14535 = (0);
while(true){
if((i__14535 < count__14534)){
var link = chunk__14533.cljs$core$IIndexed$_nth$arity$2(null,i__14535);
link.addEventListener("click",((function (seq__14532,chunk__14533,count__14534,i__14535,link,toc_entry_links,heading_ancestry){
return (function (event){
var clicked_link = event.currentTarget;
if(cljs.core.truth_(clicked_link.classList.contains("active"))){
return null;
} else {
return ((function (seq__14532,chunk__14533,count__14534,i__14535,clicked_link,link,toc_entry_links,heading_ancestry){
return (function (){
return cljs.core.reset_BANG_(lilac.toc_active_entries.visible_headings,cljs.core.PersistentHashSet.EMPTY);
});
;})(seq__14532,chunk__14533,count__14534,i__14535,clicked_link,link,toc_entry_links,heading_ancestry))
}
});})(seq__14532,chunk__14533,count__14534,i__14535,link,toc_entry_links,heading_ancestry))
);


var G__14560 = seq__14532;
var G__14561 = chunk__14533;
var G__14562 = count__14534;
var G__14563 = (i__14535 + (1));
seq__14532 = G__14560;
chunk__14533 = G__14561;
count__14534 = G__14562;
i__14535 = G__14563;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14532);
if(temp__5825__auto__){
var seq__14532__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14532__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14532__$1);
var G__14564 = cljs.core.chunk_rest(seq__14532__$1);
var G__14565 = c__4556__auto__;
var G__14566 = cljs.core.count(c__4556__auto__);
var G__14567 = (0);
seq__14532 = G__14564;
chunk__14533 = G__14565;
count__14534 = G__14566;
i__14535 = G__14567;
continue;
} else {
var link = cljs.core.first(seq__14532__$1);
link.addEventListener("click",((function (seq__14532,chunk__14533,count__14534,i__14535,link,seq__14532__$1,temp__5825__auto__,toc_entry_links,heading_ancestry){
return (function (event){
var clicked_link = event.currentTarget;
if(cljs.core.truth_(clicked_link.classList.contains("active"))){
return null;
} else {
return ((function (seq__14532,chunk__14533,count__14534,i__14535,clicked_link,link,seq__14532__$1,temp__5825__auto__,toc_entry_links,heading_ancestry){
return (function (){
return cljs.core.reset_BANG_(lilac.toc_active_entries.visible_headings,cljs.core.PersistentHashSet.EMPTY);
});
;})(seq__14532,chunk__14533,count__14534,i__14535,clicked_link,link,seq__14532__$1,temp__5825__auto__,toc_entry_links,heading_ancestry))
}
});})(seq__14532,chunk__14533,count__14534,i__14535,link,seq__14532__$1,temp__5825__auto__,toc_entry_links,heading_ancestry))
);


var G__14568 = cljs.core.next(seq__14532__$1);
var G__14569 = null;
var G__14570 = (0);
var G__14571 = (0);
seq__14532 = G__14568;
chunk__14533 = G__14569;
count__14534 = G__14570;
i__14535 = G__14571;
continue;
}
} else {
return null;
}
}
break;
}
});
