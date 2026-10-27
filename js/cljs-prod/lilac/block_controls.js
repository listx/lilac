// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.block_controls');
goog.require('cljs.core');
goog.require('cljs.core.constants');
if((typeof lilac !== 'undefined') && (typeof lilac.block_controls !== 'undefined') && (typeof lilac.block_controls.state !== 'undefined')){
} else {
lilac.block_controls.state = cljs.core.atom.cljs$core$IFn$_invoke$arity$1(new cljs.core.PersistentArrayMap(null, 2, [cljs.core.cst$kw$block_DASH_ids,cljs.core.PersistentArrayMap.EMPTY,cljs.core.cst$kw$updated_DASH_block_DASH_id,null], null));
}
lilac.block_controls.calculate_initial_state = (function lilac$block_controls$calculate_initial_state(container){
var body = container.querySelector(".block-body");
var is_collapsed_QMARK_ = body.classList.contains("collapsed");
var has_collapse_button_QMARK_ = body.classList.contains("collapsible");
var is_anonymous_QMARK_ = body.classList.contains("anonymous");
return new cljs.core.PersistentArrayMap(null, 4, [cljs.core.cst$kw$meta_DASH_visible_QMARK_,false,cljs.core.cst$kw$body_DASH_visible_QMARK_,cljs.core.not(is_collapsed_QMARK_),cljs.core.cst$kw$has_DASH_collapse_DASH_button_QMARK_,has_collapse_button_QMARK_,cljs.core.cst$kw$anonymous_QMARK_,is_anonymous_QMARK_], null);
});
lilac.block_controls.ensure_block_state = (function lilac$block_controls$ensure_block_state(st,block_id,block_el){
if(cljs.core.contains_QMARK_(cljs.core.cst$kw$block_DASH_ids.cljs$core$IFn$_invoke$arity$1(st),block_id)){
return st;
} else {
return cljs.core.assoc_in(st,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.cst$kw$block_DASH_ids,block_id], null),lilac.block_controls.calculate_initial_state(block_el));
}
});
lilac.block_controls.handle_click_BANG_ = (function lilac$block_controls$handle_click_BANG_(e){
var temp__5825__auto__ = e.target.closest("[data-action]");
if(cljs.core.truth_(temp__5825__auto__)){
var btn = temp__5825__auto__;
var temp__5825__auto____$1 = btn.closest(".block-container");
if(cljs.core.truth_(temp__5825__auto____$1)){
var block = temp__5825__auto____$1;
var block_id = cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(block.id);
var action = btn.getAttribute("data-action");
var G__14372 = action;
switch (G__14372) {
case "toggle-block-meta-other":
case "toggle-block-body":
var state_key = ((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(action,"toggle-block-meta-other"))?cljs.core.cst$kw$meta_DASH_visible_QMARK_:cljs.core.cst$kw$body_DASH_visible_QMARK_);
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(lilac.block_controls.state,(function (p1__14371_SHARP_){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(lilac.block_controls.ensure_block_state(p1__14371_SHARP_,block_id,block),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.cst$kw$block_DASH_ids,block_id,state_key], null),cljs.core.not),cljs.core.cst$kw$updated_DASH_block_DASH_id,block_id);
}));

break;
default:
return console.warn("[WARN] Unrecognized action:",action);

}
} else {
return null;
}
} else {
return null;
}
});
lilac.block_controls.toggle_class_BANG_ = (function lilac$block_controls$toggle_class_BANG_(el,class_name,enable_QMARK_){
if(cljs.core.truth_(el)){
return el.classList.toggle(class_name,enable_QMARK_);
} else {
return null;
}
});
lilac.block_controls.rounding_styles = new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 4, ["top-rounded",null,"fully-rounded",null,"bottom-rounded",null,"not-rounded",null], null), null);
lilac.block_controls.set_enum_class_BANG_ = (function lilac$block_controls$set_enum_class_BANG_(el,all_variants,target_variant){
if(cljs.core.truth_(el)){
var class_list = el.classList;
var seq__14374_14378 = cljs.core.seq(all_variants);
var chunk__14375_14379 = null;
var count__14376_14380 = (0);
var i__14377_14381 = (0);
while(true){
if((i__14377_14381 < count__14376_14380)){
var variant_14382 = chunk__14375_14379.cljs$core$IIndexed$_nth$arity$2(null,i__14377_14381);
class_list.remove(variant_14382);


var G__14383 = seq__14374_14378;
var G__14384 = chunk__14375_14379;
var G__14385 = count__14376_14380;
var G__14386 = (i__14377_14381 + (1));
seq__14374_14378 = G__14383;
chunk__14375_14379 = G__14384;
count__14376_14380 = G__14385;
i__14377_14381 = G__14386;
continue;
} else {
var temp__5825__auto___14387 = cljs.core.seq(seq__14374_14378);
if(temp__5825__auto___14387){
var seq__14374_14388__$1 = temp__5825__auto___14387;
if(cljs.core.chunked_seq_QMARK_(seq__14374_14388__$1)){
var c__4556__auto___14389 = cljs.core.chunk_first(seq__14374_14388__$1);
var G__14390 = cljs.core.chunk_rest(seq__14374_14388__$1);
var G__14391 = c__4556__auto___14389;
var G__14392 = cljs.core.count(c__4556__auto___14389);
var G__14393 = (0);
seq__14374_14378 = G__14390;
chunk__14375_14379 = G__14391;
count__14376_14380 = G__14392;
i__14377_14381 = G__14393;
continue;
} else {
var variant_14394 = cljs.core.first(seq__14374_14388__$1);
class_list.remove(variant_14394);


var G__14395 = cljs.core.next(seq__14374_14388__$1);
var G__14396 = null;
var G__14397 = (0);
var G__14398 = (0);
seq__14374_14378 = G__14395;
chunk__14375_14379 = G__14396;
count__14376_14380 = G__14397;
i__14377_14381 = G__14398;
continue;
}
} else {
}
}
break;
}

if(cljs.core.truth_(target_variant)){
return class_list.add(target_variant);
} else {
return null;
}
} else {
return null;
}
});
lilac.block_controls.render_block_BANG_ = (function lilac$block_controls$render_block_BANG_(block_id){
var temp__5823__auto__ = document.getElementById(cljs.core.name(block_id));
if(cljs.core.truth_(temp__5823__auto__)){
var container = temp__5823__auto__;
var block_state = cljs.core.get_in.cljs$core$IFn$_invoke$arity$2(cljs.core.deref(lilac.block_controls.state),new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.cst$kw$block_DASH_ids,block_id], null));
var anon_QMARK_ = cljs.core.cst$kw$anonymous_QMARK_.cljs$core$IFn$_invoke$arity$1(block_state);
var body_vis_QMARK_ = cljs.core.cst$kw$body_DASH_visible_QMARK_.cljs$core$IFn$_invoke$arity$1(block_state);
var collapse_button_QMARK_ = cljs.core.cst$kw$has_DASH_collapse_DASH_button_QMARK_.cljs$core$IFn$_invoke$arity$1(block_state);
var meta_vis_QMARK_ = cljs.core.cst$kw$meta_DASH_visible_QMARK_.cljs$core$IFn$_invoke$arity$1(block_state);
var other_meta = container.querySelector(".block-meta-other");
var body = container.querySelector(".block-body");
var controls = container.querySelector(".block-controls-inline");
var body_needs_full_rounding_QMARK_ = (function (){var and__4115__auto__ = anon_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
if(cljs.core.not(meta_vis_QMARK_)){
if(cljs.core.not(collapse_button_QMARK_)){
return body_vis_QMARK_;
} else {
return false;
}
} else {
return false;
}
} else {
return and__4115__auto__;
}
})();
var rounding_style = (cljs.core.truth_((function (){var and__4115__auto__ = anon_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
var and__4115__auto____$1 = meta_vis_QMARK_;
if(cljs.core.truth_(and__4115__auto____$1)){
return body_vis_QMARK_;
} else {
return and__4115__auto____$1;
}
} else {
return and__4115__auto__;
}
})())?"not-rounded":(cljs.core.truth_((function (){var and__4115__auto__ = anon_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
return ((cljs.core.not(meta_vis_QMARK_)) && (cljs.core.not(body_vis_QMARK_)));
} else {
return and__4115__auto__;
}
})())?"fully-rounded":(cljs.core.truth_((function (){var and__4115__auto__ = anon_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
var and__4115__auto____$1 = meta_vis_QMARK_;
if(cljs.core.truth_(and__4115__auto____$1)){
return cljs.core.not(body_vis_QMARK_);
} else {
return and__4115__auto____$1;
}
} else {
return and__4115__auto__;
}
})())?"bottom-rounded":(cljs.core.truth_((function (){var and__4115__auto__ = anon_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
if(cljs.core.not(meta_vis_QMARK_)){
return body_vis_QMARK_;
} else {
return false;
}
} else {
return and__4115__auto__;
}
})())?"top-rounded":(cljs.core.truth_(((cljs.core.not(anon_QMARK_))?(function (){var and__4115__auto__ = meta_vis_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
return body_vis_QMARK_;
} else {
return and__4115__auto__;
}
})():false))?"not-rounded":((((cljs.core.not(anon_QMARK_)) && (cljs.core.not(meta_vis_QMARK_)) && (cljs.core.not(body_vis_QMARK_))))?"bottom-rounded":(cljs.core.truth_(((cljs.core.not(anon_QMARK_))?(function (){var and__4115__auto__ = meta_vis_QMARK_;
if(cljs.core.truth_(and__4115__auto__)){
return cljs.core.not(body_vis_QMARK_);
} else {
return and__4115__auto__;
}
})():false))?"bottom-rounded":(cljs.core.truth_(((cljs.core.not(anon_QMARK_))?((cljs.core.not(meta_vis_QMARK_))?body_vis_QMARK_:false):false))?"not-rounded":null
))))))));
lilac.block_controls.toggle_class_BANG_(other_meta,"hidden",cljs.core.not(meta_vis_QMARK_));

lilac.block_controls.toggle_class_BANG_(body,"collapsed",cljs.core.not(body_vis_QMARK_));

lilac.block_controls.toggle_class_BANG_(body,"fully-rounded",body_needs_full_rounding_QMARK_);

lilac.block_controls.toggle_class_BANG_(controls,"hide-border-bottom",body_vis_QMARK_);

return lilac.block_controls.set_enum_class_BANG_(controls,lilac.block_controls.rounding_styles,rounding_style);
} else {
return console.warn("[WARN] Render target not found for ID:",cljs.core.name(block_id));
}
});
lilac.block_controls.init = (function lilac$block_controls$init(){
cljs.core.add_watch(lilac.block_controls.state,cljs.core.cst$kw$render,(function (_,___$1,___$2,new_state){
var temp__5825__auto__ = cljs.core.cst$kw$updated_DASH_block_DASH_id.cljs$core$IFn$_invoke$arity$1(new_state);
if(cljs.core.truth_(temp__5825__auto__)){
var block_id = temp__5825__auto__;
return lilac.block_controls.render_block_BANG_(block_id);
} else {
return null;
}
}));

return document.addEventListener("click",lilac.block_controls.handle_click_BANG_);
});
