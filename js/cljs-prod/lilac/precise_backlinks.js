// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.precise_backlinks');
goog.require('cljs.core');
goog.require('cljs.core.constants');
lilac.precise_backlinks.ensure_popup_BANG_ = (function lilac$precise_backlinks$ensure_popup_BANG_(){
var or__4126__auto__ = document.getElementById("popup-backlinks-active");
if(cljs.core.truth_(or__4126__auto__)){
return or__4126__auto__;
} else {
var popup = document.createElement("div");
(popup.id = "popup-backlinks-active");

popup.classList.add("popup-backlinks");

return popup;
}
});
lilac.precise_backlinks.extract_trigger = (function lilac$precise_backlinks$extract_trigger(target,selector){
if(cljs.core.truth_(target)){
var element = ((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(target.nodeType,Node.TEXT_NODE))?target.parentElement:target);
if(cljs.core.truth_(element)){
return element.closest(selector);
} else {
return null;
}
} else {
return null;
}
});
lilac.precise_backlinks.parse_backlinks = (function lilac$precise_backlinks$parse_backlinks(trigger){
var raw_data = trigger.getAttribute("data-backlinks");
return JSON.parse((function (){var or__4126__auto__ = raw_data;
if(cljs.core.truth_(or__4126__auto__)){
return or__4126__auto__;
} else {
return "[]";
}
})());
});
lilac.precise_backlinks.build_backlinks_list = (function lilac$precise_backlinks$build_backlinks_list(tuples){
var ul = document.createElement("ul");
if((tuples.length > (0))){
var seq__14576_14580 = cljs.core.seq(cljs.core.array_seq.cljs$core$IFn$_invoke$arity$1(tuples));
var chunk__14577_14581 = null;
var count__14578_14582 = (0);
var i__14579_14583 = (0);
while(true){
if((i__14579_14583 < count__14578_14582)){
var item_14584 = chunk__14577_14581.cljs$core$IIndexed$_nth$arity$2(null,i__14579_14583);
var href_14585 = (item_14584[(0)]);
var html_label_14586 = (item_14584[(1)]);
var li_14587 = document.createElement("li");
var p_14588 = document.createElement("p");
var a_14589 = document.createElement("a");
(a_14589.href = href_14585);

(a_14589.innerHTML = html_label_14586);

p_14588.appendChild(a_14589);

li_14587.appendChild(p_14588);

ul.appendChild(li_14587);


var G__14590 = seq__14576_14580;
var G__14591 = chunk__14577_14581;
var G__14592 = count__14578_14582;
var G__14593 = (i__14579_14583 + (1));
seq__14576_14580 = G__14590;
chunk__14577_14581 = G__14591;
count__14578_14582 = G__14592;
i__14579_14583 = G__14593;
continue;
} else {
var temp__5825__auto___14594 = cljs.core.seq(seq__14576_14580);
if(temp__5825__auto___14594){
var seq__14576_14595__$1 = temp__5825__auto___14594;
if(cljs.core.chunked_seq_QMARK_(seq__14576_14595__$1)){
var c__4556__auto___14596 = cljs.core.chunk_first(seq__14576_14595__$1);
var G__14597 = cljs.core.chunk_rest(seq__14576_14595__$1);
var G__14598 = c__4556__auto___14596;
var G__14599 = cljs.core.count(c__4556__auto___14596);
var G__14600 = (0);
seq__14576_14580 = G__14597;
chunk__14577_14581 = G__14598;
count__14578_14582 = G__14599;
i__14579_14583 = G__14600;
continue;
} else {
var item_14601 = cljs.core.first(seq__14576_14595__$1);
var href_14602 = (item_14601[(0)]);
var html_label_14603 = (item_14601[(1)]);
var li_14604 = document.createElement("li");
var p_14605 = document.createElement("p");
var a_14606 = document.createElement("a");
(a_14606.href = href_14602);

(a_14606.innerHTML = html_label_14603);

p_14605.appendChild(a_14606);

li_14604.appendChild(p_14605);

ul.appendChild(li_14604);


var G__14607 = cljs.core.next(seq__14576_14595__$1);
var G__14608 = null;
var G__14609 = (0);
var G__14610 = (0);
seq__14576_14580 = G__14607;
chunk__14577_14581 = G__14608;
count__14578_14582 = G__14609;
i__14579_14583 = G__14610;
continue;
}
} else {
}
}
break;
}
} else {
(ul.textContent = "No backlinks found.");
}

return ul;
});
lilac.precise_backlinks.position_popup_BANG_ = (function lilac$precise_backlinks$position_popup_BANG_(popup,x,y){
var padding = (10);
var vw = window.innerWidth;
var vh = window.innerHeight;
var pw = popup.offsetWidth;
var ph = popup.offsetHeight;
var target_x = (x + padding);
var target_y = (y + padding);
var final_x = ((((target_x + pw) > vw))?(function (){var x__4214__auto__ = (0);
var y__4215__auto__ = ((vw - pw) - padding);
return ((x__4214__auto__ > y__4215__auto__) ? x__4214__auto__ : y__4215__auto__);
})():target_x);
var final_y = ((((target_y + ph) > vh))?(function (){var x__4214__auto__ = (0);
var y__4215__auto__ = ((vh - ph) - padding);
return ((x__4214__auto__ > y__4215__auto__) ? x__4214__auto__ : y__4215__auto__);
})():target_y);
(popup.style.left = [cljs.core.str.cljs$core$IFn$_invoke$arity$1(final_x),"px"].join(''));

return (popup.style.top = [cljs.core.str.cljs$core$IFn$_invoke$arity$1(final_y),"px"].join(''));
});
lilac.precise_backlinks.close_popup_BANG_ = (function lilac$precise_backlinks$close_popup_BANG_(popup,active_trigger_atom){
popup.classList.remove("active");

return cljs.core.reset_BANG_(active_trigger_atom,null);
});
lilac.precise_backlinks.extract_id = (function lilac$precise_backlinks$extract_id(href){
if(cljs.core.truth_(href)){
var idx = href.lastIndexOf("#");
if((idx >= (0))){
return href.substring((idx + (1)));
} else {
return null;
}
} else {
return null;
}
});
lilac.precise_backlinks.activate_target_element_BANG_ = (function lilac$precise_backlinks$activate_target_element_BANG_(href){
var temp__5825__auto__ = lilac.precise_backlinks.extract_id(href);
if(cljs.core.truth_(temp__5825__auto__)){
var id_str = temp__5825__auto__;
var temp__5825__auto____$1 = document.getElementById(id_str);
if(cljs.core.truth_(temp__5825__auto____$1)){
var target_el = temp__5825__auto____$1;
return target_el.classList.toggle("active");
} else {
return null;
}
} else {
return null;
}
});
lilac.precise_backlinks.open_popup_BANG_ = (function lilac$precise_backlinks$open_popup_BANG_(popup,trigger,x,y,active_trigger_atom){
if(cljs.core.truth_(popup.parentNode)){
} else {
document.body.appendChild(popup);
}

(popup.innerHTML = "");

var tuples = lilac.precise_backlinks.parse_backlinks(trigger);
var list_el = lilac.precise_backlinks.build_backlinks_list(tuples);
popup.appendChild(list_el);

popup.classList.add("active");

lilac.precise_backlinks.position_popup_BANG_(popup,x,y);

return cljs.core.reset_BANG_(active_trigger_atom,trigger);
});
lilac.precise_backlinks.init = (function lilac$precise_backlinks$init(){
var popup = lilac.precise_backlinks.ensure_popup_BANG_();
var active_trigger = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(null);
var close_BANG_ = (function (){
return lilac.precise_backlinks.close_popup_BANG_(popup,active_trigger);
});
window.addEventListener("scroll",close_BANG_,({"passive": true}));

return document.addEventListener("click",(function (e){
var target = e.target;
var trigger = lilac.precise_backlinks.extract_trigger(target,".button-popup-backlinks");
var popup_link = target.closest("a");
var is_inside_popup_QMARK_ = (function (){var and__4115__auto__ = popup.parentNode;
if(cljs.core.truth_(and__4115__auto__)){
return popup.contains(target);
} else {
return and__4115__auto__;
}
})();
var is_open_QMARK_ = popup.classList.contains("active");
var is_same_trigger_QMARK_ = (function (){var and__4115__auto__ = trigger;
if(cljs.core.truth_(and__4115__auto__)){
var and__4115__auto____$1 = is_open_QMARK_;
if(cljs.core.truth_(and__4115__auto____$1)){
return cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(cljs.core.deref(active_trigger),trigger);
} else {
return and__4115__auto____$1;
}
} else {
return and__4115__auto__;
}
})();
if(cljs.core.truth_((function (){var and__4115__auto__ = is_inside_popup_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
return popup_link;
} else {
return and__4115__auto__;
}
})())){
var href = popup_link.getAttribute("href");
lilac.precise_backlinks.activate_target_element_BANG_(href);

return close_BANG_();
} else {
if(cljs.core.truth_(is_same_trigger_QMARK_)){
e.preventDefault();

e.stopPropagation();

return close_BANG_();
} else {
if(cljs.core.truth_(trigger)){
e.preventDefault();

e.stopPropagation();

return lilac.precise_backlinks.open_popup_BANG_(popup,trigger,e.clientX,e.clientY,active_trigger);
} else {
if(cljs.core.truth_((function (){var and__4115__auto__ = popup.parentNode;
if(cljs.core.truth_(and__4115__auto__)){
return cljs.core.not(is_inside_popup_QMARK_);
} else {
return and__4115__auto__;
}
})())){
return close_BANG_();
} else {
return null;
}
}
}
}
}));
});
