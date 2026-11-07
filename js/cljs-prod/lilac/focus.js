// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.focus');
goog.require('cljs.core');
goog.require('cljs.core.constants');
lilac.focus.deactivate_element = (function lilac$focus$deactivate_element(elt){
var class_list = elt.classList;
return class_list.remove("active");
});
lilac.focus.deactivate_other_elements = (function lilac$focus$deactivate_other_elements(active_id){
var temp__5825__auto__ = document.querySelector("main");
if(cljs.core.truth_(temp__5825__auto__)){
var main_elt = temp__5825__auto__;
var child_anchors = main_elt.querySelectorAll("[id]");
var seq__14300 = cljs.core.seq(child_anchors);
var chunk__14301 = null;
var count__14302 = (0);
var i__14303 = (0);
while(true){
if((i__14303 < count__14302)){
var elt = chunk__14301.cljs$core$IIndexed$_nth$arity$2(null,i__14303);
if(cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(elt.id,active_id)){
lilac.focus.deactivate_element(elt);
} else {
}


var G__14304 = seq__14300;
var G__14305 = chunk__14301;
var G__14306 = count__14302;
var G__14307 = (i__14303 + (1));
seq__14300 = G__14304;
chunk__14301 = G__14305;
count__14302 = G__14306;
i__14303 = G__14307;
continue;
} else {
var temp__5825__auto____$1 = cljs.core.seq(seq__14300);
if(temp__5825__auto____$1){
var seq__14300__$1 = temp__5825__auto____$1;
if(cljs.core.chunked_seq_QMARK_(seq__14300__$1)){
var c__4556__auto__ = cljs.core.chunk_first(seq__14300__$1);
var G__14308 = cljs.core.chunk_rest(seq__14300__$1);
var G__14309 = c__4556__auto__;
var G__14310 = cljs.core.count(c__4556__auto__);
var G__14311 = (0);
seq__14300 = G__14308;
chunk__14301 = G__14309;
count__14302 = G__14310;
i__14303 = G__14311;
continue;
} else {
var elt = cljs.core.first(seq__14300__$1);
if(cljs.core.not_EQ_.cljs$core$IFn$_invoke$arity$2(elt.id,active_id)){
lilac.focus.deactivate_element(elt);
} else {
}


var G__14312 = cljs.core.next(seq__14300__$1);
var G__14313 = null;
var G__14314 = (0);
var G__14315 = (0);
seq__14300 = G__14312;
chunk__14301 = G__14313;
count__14302 = G__14314;
i__14303 = G__14315;
continue;
}
} else {
return null;
}
}
break;
}
} else {
return null;
}
});
lilac.focus.activate_element = (function lilac$focus$activate_element(elt){
var class_list = elt.classList;
return class_list.add("active");
});
lilac.focus.activate = (function lilac$focus$activate(elt){
lilac.focus.deactivate_other_elements(elt.id);

return lilac.focus.activate_element(elt);
});
