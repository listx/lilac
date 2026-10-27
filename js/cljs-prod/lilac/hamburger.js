// Compiled by ClojureScript 1.10.773 {:static-fns true, :optimize-constants true}
goog.provide('lilac.hamburger');
goog.require('cljs.core');
goog.require('cljs.core.constants');
lilac.hamburger.init_sidebar_for_mobile = (function lilac$hamburger$init_sidebar_for_mobile(){
var sidebar = document.getElementById("tocs");
var hamburger = document.getElementById("hamburger");
hamburger.addEventListener("click",(function (e){
e.stopPropagation();

return sidebar.classList.toggle("active");
}));

return document.addEventListener("click",(function (e){
if(cljs.core.truth_((function (){var and__4115__auto__ = sidebar.classList.contains("active");
if(cljs.core.truth_(and__4115__auto__)){
return cljs.core.not(sidebar.contains(e.target));
} else {
return and__4115__auto__;
}
})())){
return sidebar.classList.remove("active");
} else {
return null;
}
}));
});
lilac.hamburger.init = (function lilac$hamburger$init(){
return document.addEventListener("DOMContentLoaded",(function (_){
return lilac.hamburger.init_sidebar_for_mobile();
}));
});
