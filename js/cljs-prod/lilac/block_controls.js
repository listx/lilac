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
var G__14339 = action;
switch (G__14339) {
case "toggle-block-meta-other":
case "toggle-block-body":
var state_key = ((cljs.core._EQ_.cljs$core$IFn$_invoke$arity$2(action,"toggle-block-meta-other"))?cljs.core.cst$kw$meta_DASH_visible_QMARK_:cljs.core.cst$kw$body_DASH_visible_QMARK_);
return cljs.core.swap_BANG_.cljs$core$IFn$_invoke$arity$2(lilac.block_controls.state,(function (p1__14338_SHARP_){
return cljs.core.assoc.cljs$core$IFn$_invoke$arity$3(cljs.core.update_in.cljs$core$IFn$_invoke$arity$3(lilac.block_controls.ensure_block_state(p1__14338_SHARP_,block_id,block),new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [cljs.core.cst$kw$block_DASH_ids,block_id,state_key], null),cljs.core.not),cljs.core.cst$kw$updated_DASH_block_DASH_id,block_id);
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
var seq__14341_14345 = cljs.core.seq(all_variants);
var chunk__14342_14346 = null;
var count__14343_14347 = (0);
var i__14344_14348 = (0);
while(true){
if((i__14344_14348 < count__14343_14347)){
var variant_14349 = chunk__14342_14346.cljs$core$IIndexed$_nth$arity$2(null,i__14344_14348);
class_list.remove(variant_14349);


var G__14350 = seq__14341_14345;
var G__14351 = chunk__14342_14346;
var G__14352 = count__14343_14347;
var G__14353 = (i__14344_14348 + (1));
seq__14341_14345 = G__14350;
chunk__14342_14346 = G__14351;
count__14343_14347 = G__14352;
i__14344_14348 = G__14353;
continue;
} else {
var temp__5825__auto___14354 = cljs.core.seq(seq__14341_14345);
if(temp__5825__auto___14354){
var seq__14341_14355__$1 = temp__5825__auto___14354;
if(cljs.core.chunked_seq_QMARK_(seq__14341_14355__$1)){
var c__4556__auto___14356 = cljs.core.chunk_first(seq__14341_14355__$1);
var G__14357 = cljs.core.chunk_rest(seq__14341_14355__$1);
var G__14358 = c__4556__auto___14356;
var G__14359 = cljs.core.count(c__4556__auto___14356);
var G__14360 = (0);
seq__14341_14345 = G__14357;
chunk__14342_14346 = G__14358;
count__14343_14347 = G__14359;
i__14344_14348 = G__14360;
continue;
} else {
var variant_14361 = cljs.core.first(seq__14341_14355__$1);
class_list.remove(variant_14361);


var G__14362 = cljs.core.next(seq__14341_14355__$1);
var G__14363 = null;
var G__14364 = (0);
var G__14365 = (0);
seq__14341_14345 = G__14362;
chunk__14342_14346 = G__14363;
count__14343_14347 = G__14364;
i__14344_14348 = G__14365;
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
var body_toggle = container.querySelector(".toggle-block-body");
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

lilac.block_controls.toggle_class_BANG_(body_toggle,"action-collapse",body_vis_QMARK_);

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
