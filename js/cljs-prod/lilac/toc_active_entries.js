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
var seq__14447 = cljs.core.seq(entries);
var chunk__14448 = null;
var count__14449 = (0);
var i__14450 = (0);
while(true){
if((i__14450 < count__14449)){
var entry = chunk__14448.cljs$core$IIndexed$_nth$arity$2(null,i__14450);
var heading_container_id_14451 = entry.target.id;
var heading_id_14452 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(heading_container_id_14451,(18));
var visible_QMARK__14453 = entry.isIntersecting;
var modifier_14454 = (cljs.core.truth_(visible_QMARK__14453)?cljs.core.conj:cljs.core.disj);
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(lilac.toc_active_entries.visible_headings,modifier_14454,heading_id_14452);


var G__14455 = seq__14447;
var G__14456 = chunk__14448;
var G__14457 = count__14449;
var G__14458 = (i__14450 + (1));
seq__14447 = G__14455;
chunk__14448 = G__14456;
count__14449 = G__14457;
i__14450 = G__14458;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14447);
if(temp__5825__auto__){
var seq__14447__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14447__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14447__$1);
var G__14459 = cljs.core.chunk_rest(seq__14447__$1);
var G__14460 = c__4556__auto__;
var G__14461 = cljs.core.count(c__4556__auto__);
var G__14462 = (0);
seq__14447 = G__14459;
chunk__14448 = G__14460;
count__14449 = G__14461;
i__14450 = G__14462;
continue;
} else {
var entry = cljs.core.first(seq__14447__$1);
var heading_container_id_14463 = entry.target.id;
var heading_id_14464 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(heading_container_id_14463,(18));
var visible_QMARK__14465 = entry.isIntersecting;
var modifier_14466 = (cljs.core.truth_(visible_QMARK__14465)?cljs.core.conj:cljs.core.disj);
cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$3(lilac.toc_active_entries.visible_headings,modifier_14466,heading_id_14464);


var G__14467 = cljs.core.next(seq__14447__$1);
var G__14468 = null;
var G__14469 = (0);
var G__14470 = (0);
seq__14447 = G__14467;
chunk__14448 = G__14468;
count__14449 = G__14469;
i__14450 = G__14470;
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
var visible_headings_with_ancestors = cljs.core.reduce.cljs$core$IFn$_invoke$arity$3((function (acc,id){
var ancestors = cljs.core.set(cljs.core.get.cljs$core$IFn$_invoke$arity$3(ancestry,cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(id),cljs.core.PersistentVector.EMPTY));
return clojure.set.union.cljs$core$IFn$_invoke$arity$2(acc,ancestors);
}),visible_headings,visible_headings);
var seq__14471 = cljs.core.seq(toc_entries);
var chunk__14472 = null;
var count__14473 = (0);
var i__14474 = (0);
while(true){
if((i__14474 < count__14473)){
var toc_entry = chunk__14472.cljs$core$IIndexed$_nth$arity$2(null,i__14474);
var temp__5825__auto___14475 = toc_entry.querySelector("a");
if(cljs.core.truth_(temp__5825__auto___14475)){
var toc_entry_link_14476 = temp__5825__auto___14475;
var heading_14477 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(toc_entry_link_14476.hash,(1));
var class_list_14478 = toc_entry.classList;
if(cljs.core.contains_QMARK_(visible_headings,heading_14477)){
class_list_14478.add("active");

class_list_14478.remove("active-ancestor");
} else {
if(cljs.core.contains_QMARK_(visible_headings_with_ancestors,heading_14477)){
class_list_14478.add("active-ancestor");

class_list_14478.remove("active");
} else {
class_list_14478.remove("active");

class_list_14478.remove("active-ancestor");

}
}
} else {
}


var G__14479 = seq__14471;
var G__14480 = chunk__14472;
var G__14481 = count__14473;
var G__14482 = (i__14474 + (1));
seq__14471 = G__14479;
chunk__14472 = G__14480;
count__14473 = G__14481;
i__14474 = G__14482;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14471);
if(temp__5825__auto__){
var seq__14471__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14471__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14471__$1);
var G__14483 = cljs.core.chunk_rest(seq__14471__$1);
var G__14484 = c__4556__auto__;
var G__14485 = cljs.core.count(c__4556__auto__);
var G__14486 = (0);
seq__14471 = G__14483;
chunk__14472 = G__14484;
count__14473 = G__14485;
i__14474 = G__14486;
continue;
} else {
var toc_entry = cljs.core.first(seq__14471__$1);
var temp__5825__auto___14487__$1 = toc_entry.querySelector("a");
if(cljs.core.truth_(temp__5825__auto___14487__$1)){
var toc_entry_link_14488 = temp__5825__auto___14487__$1;
var heading_14489 = cljs.core.subs.cljs$core$IFn$_invoke$arity$2(toc_entry_link_14488.hash,(1));
var class_list_14490 = toc_entry.classList;
if(cljs.core.contains_QMARK_(visible_headings,heading_14489)){
class_list_14490.add("active");

class_list_14490.remove("active-ancestor");
} else {
if(cljs.core.contains_QMARK_(visible_headings_with_ancestors,heading_14489)){
class_list_14490.add("active-ancestor");

class_list_14490.remove("active");
} else {
class_list_14490.remove("active");

class_list_14490.remove("active-ancestor");

}
}
} else {
}


var G__14491 = cljs.core.next(seq__14471__$1);
var G__14492 = null;
var G__14493 = (0);
var G__14494 = (0);
seq__14471 = G__14491;
chunk__14472 = G__14492;
count__14473 = G__14493;
i__14474 = G__14494;
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

var options = ({"rootMargin": "0px 0px 0px 0px", "threshold": (0)});
var heading_containers = document.querySelectorAll(".heading-container");
var observer = (new IntersectionObserver(lilac.toc_active_entries.update_visible_headings,options));
var seq__14495 = cljs.core.seq(heading_containers);
var chunk__14496 = null;
var count__14497 = (0);
var i__14498 = (0);
while(true){
if((i__14498 < count__14497)){
var heading_container = chunk__14496.cljs$core$IIndexed$_nth$arity$2(null,i__14498);
observer.observe(heading_container);


var G__14499 = seq__14495;
var G__14500 = chunk__14496;
var G__14501 = count__14497;
var G__14502 = (i__14498 + (1));
seq__14495 = G__14499;
chunk__14496 = G__14500;
count__14497 = G__14501;
i__14498 = G__14502;
continue;
} else {
var temp__5825__auto__ = cljs.core.seq(seq__14495);
if(temp__5825__auto__){
var seq__14495__$1 = temp__5825__auto__;
if(cljs.core.chunked_seq_QMARK_(seq__14495__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14495__$1);
var G__14503 = cljs.core.chunk_rest(seq__14495__$1);
var G__14504 = c__4556__auto__;
var G__14505 = cljs.core.count(c__4556__auto__);
var G__14506 = (0);
seq__14495 = G__14503;
chunk__14496 = G__14504;
count__14497 = G__14505;
i__14498 = G__14506;
continue;
} else {
var heading_container = cljs.core.first(seq__14495__$1);
observer.observe(heading_container);


var G__14507 = cljs.core.next(seq__14495__$1);
var G__14508 = null;
var G__14509 = (0);
var G__14510 = (0);
seq__14495 = G__14507;
chunk__14496 = G__14508;
count__14497 = G__14509;
i__14498 = G__14510;
continue;
}
} else {
return null;
}
}
break;
}
});
