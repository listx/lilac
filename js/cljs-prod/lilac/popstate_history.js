// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.popstate_history');
goog.require('cljs.core');
goog.require('cljs.core.constants');
goog.require('clojure.string');
goog.require('lilac.focus');
lilac.popstate_history.handle_popstate = (function lilac$popstate_history$handle_popstate(){
var current_hash = ((clojure.string.blank_QMARK_(window.location.hash))?"#top":window.location.hash);
var id = current_hash.substring((1));
var temp__5825__auto__ = document.getElementById(id);
if(cljs.core.truth_(temp__5825__auto__)){
var target_el = temp__5825__auto__;
return lilac.focus.activate(target_el);
} else {
return null;
}
});
lilac.popstate_history.init = (function lilac$popstate_history$init(){
return window.addEventListener("popstate",lilac.popstate_history.handle_popstate);
});
