// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.core');
goog.require('cljs.core');
goog.require('cljs.core.constants');
goog.require('lilac.popstate_history');
goog.require('lilac.toc_active_entries');
goog.require('lilac.hamburger');
goog.require('lilac.lazy_scroll');
goog.require('lilac.block_controls');
goog.require('lilac.precise_backlinks');
lilac.popstate_history.init();
lilac.toc_active_entries.init();
lilac.lazy_scroll.init();
lilac.block_controls.init();
lilac.hamburger.init();
lilac.precise_backlinks.init();
console.log("Lilac loaded successfully.");
