goog.provide('sablono.core');
var module$node_modules$react$index=shadow.js.require("module$node_modules$react$index", {});
/**
 * The React.js create element function.
 */
sablono.core.create_element = module$node_modules$react$index.createElement;
/**
 * The React.js Fragment.
 */
sablono.core.fragment = module$node_modules$react$index.Fragment;
/**
 * Add an optional attribute argument to a function that returns a element vector.
 */
sablono.core.wrap_attrs = (function sablono$core$wrap_attrs(func){
return (function() { 
var G__16269__delegate = function (args){
if(cljs.core.map_QMARK_(cljs.core.first(args))){
var vec__15965 = cljs.core.apply.cljs$core$IFn$_invoke$arity$2(func,cljs.core.rest(args));
var seq__15966 = cljs.core.seq(vec__15965);
var first__15967 = cljs.core.first(seq__15966);
var seq__15966__$1 = cljs.core.next(seq__15966);
var tag = first__15967;
var body = seq__15966__$1;
if(cljs.core.map_QMARK_(cljs.core.first(body))){
return cljs.core.into.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [tag,cljs.core.merge.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([cljs.core.first(body),cljs.core.first(args)], 0))], null),cljs.core.rest(body));
} else {
return cljs.core.into.cljs$core$IFn$_invoke$arity$2(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [tag,cljs.core.first(args)], null),body);
}
} else {
return cljs.core.apply.cljs$core$IFn$_invoke$arity$2(func,args);
}
};
var G__16269 = function (var_args){
var args = null;
if (arguments.length > 0) {
var G__16273__i = 0, G__16273__a = new Array(arguments.length -  0);
while (G__16273__i < G__16273__a.length) {G__16273__a[G__16273__i] = arguments[G__16273__i + 0]; ++G__16273__i;}
  args = new cljs.core.IndexedSeq(G__16273__a,0,null);
} 
return G__16269__delegate.call(this,args);};
G__16269.cljs$lang$maxFixedArity = 0;
G__16269.cljs$lang$applyTo = (function (arglist__16274){
var args = cljs.core.seq(arglist__16274);
return G__16269__delegate(args);
});
G__16269.cljs$core$IFn$_invoke$arity$variadic = G__16269__delegate;
return G__16269;
})()
;
});
sablono.core.update_arglists = (function sablono$core$update_arglists(arglists){
var iter__4529__auto__ = (function sablono$core$update_arglists_$_iter__15968(s__15969){
return (new cljs.core.LazySeq(null,(function (){
var s__15969__$1 = s__15969;
while(true){
var temp__5735__auto__ = cljs.core.seq(s__15969__$1);
if(temp__5735__auto__){
var s__15969__$2 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_(s__15969__$2)){
var c__4527__auto__ = cljs.core.chunk_first(s__15969__$2);
var size__4528__auto__ = cljs.core.count(c__4527__auto__);
var b__15971 = cljs.core.chunk_buffer(size__4528__auto__);
if((function (){var i__15970 = (0);
while(true){
if((i__15970 < size__4528__auto__)){
var args = cljs.core._nth(c__4527__auto__,i__15970);
cljs.core.chunk_append(b__15971,cljs.core.vec(cljs.core.cons(new cljs.core.Symbol(null,"attr-map?","attr-map?",116307443,null),args)));

var G__16275 = (i__15970 + (1));
i__15970 = G__16275;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__15971),sablono$core$update_arglists_$_iter__15968(cljs.core.chunk_rest(s__15969__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__15971),null);
}
} else {
var args = cljs.core.first(s__15969__$2);
return cljs.core.cons(cljs.core.vec(cljs.core.cons(new cljs.core.Symbol(null,"attr-map?","attr-map?",116307443,null),args)),sablono$core$update_arglists_$_iter__15968(cljs.core.rest(s__15969__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__(arglists);
});
/**
 * Include a list of external stylesheet files.
 */
sablono.core.include_css = (function sablono$core$include_css(var_args){
var args__4742__auto__ = [];
var len__4736__auto___16276 = arguments.length;
var i__4737__auto___16277 = (0);
while(true){
if((i__4737__auto___16277 < len__4736__auto___16276)){
args__4742__auto__.push((arguments[i__4737__auto___16277]));

var G__16278 = (i__4737__auto___16277 + (1));
i__4737__auto___16277 = G__16278;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((0) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((0)),(0),null)):null);
return sablono.core.include_css.cljs$core$IFn$_invoke$arity$variadic(argseq__4743__auto__);
});

(sablono.core.include_css.cljs$core$IFn$_invoke$arity$variadic = (function (styles){
var iter__4529__auto__ = (function sablono$core$iter__15973(s__15974){
return (new cljs.core.LazySeq(null,(function (){
var s__15974__$1 = s__15974;
while(true){
var temp__5735__auto__ = cljs.core.seq(s__15974__$1);
if(temp__5735__auto__){
var s__15974__$2 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_(s__15974__$2)){
var c__4527__auto__ = cljs.core.chunk_first(s__15974__$2);
var size__4528__auto__ = cljs.core.count(c__4527__auto__);
var b__15976 = cljs.core.chunk_buffer(size__4528__auto__);
if((function (){var i__15975 = (0);
while(true){
if((i__15975 < size__4528__auto__)){
var style = cljs.core._nth(c__4527__auto__,i__15975);
cljs.core.chunk_append(b__15976,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"link","link",-1769163468),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),"text/css",new cljs.core.Keyword(null,"href","href",-793805698),sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([style], 0)),new cljs.core.Keyword(null,"rel","rel",1378823488),"stylesheet"], null)], null));

var G__16279 = (i__15975 + (1));
i__15975 = G__16279;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__15976),sablono$core$iter__15973(cljs.core.chunk_rest(s__15974__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__15976),null);
}
} else {
var style = cljs.core.first(s__15974__$2);
return cljs.core.cons(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"link","link",-1769163468),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),"text/css",new cljs.core.Keyword(null,"href","href",-793805698),sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([style], 0)),new cljs.core.Keyword(null,"rel","rel",1378823488),"stylesheet"], null)], null),sablono$core$iter__15973(cljs.core.rest(s__15974__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__(styles);
}));

(sablono.core.include_css.cljs$lang$maxFixedArity = (0));

/** @this {Function} */
(sablono.core.include_css.cljs$lang$applyTo = (function (seq15972){
var self__4724__auto__ = this;
return self__4724__auto__.cljs$core$IFn$_invoke$arity$variadic(cljs.core.seq(seq15972));
}));

/**
 * Include the JavaScript library at `src`.
 */
sablono.core.include_js = (function sablono$core$include_js(src){
return goog.dom.appendChild(goog.dom.getDocument().body,goog.dom.createDom("script",({"src": src})));
});
/**
 * Include Facebook's React JavaScript library.
 */
sablono.core.include_react = (function sablono$core$include_react(){
return sablono.core.include_js("http://fb.me/react-0.12.2.js");
});
/**
 * Wraps some content in a HTML hyperlink with the supplied URL.
 */
sablono.core.link_to15995 = (function sablono$core$link_to15995(var_args){
var args__4742__auto__ = [];
var len__4736__auto___16280 = arguments.length;
var i__4737__auto___16281 = (0);
while(true){
if((i__4737__auto___16281 < len__4736__auto___16280)){
args__4742__auto__.push((arguments[i__4737__auto___16281]));

var G__16282 = (i__4737__auto___16281 + (1));
i__4737__auto___16281 = G__16282;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return sablono.core.link_to15995.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(sablono.core.link_to15995.cljs$core$IFn$_invoke$arity$variadic = (function (url,content){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"a","a",-2123407586),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"href","href",-793805698),sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([url], 0))], null),content], null);
}));

(sablono.core.link_to15995.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(sablono.core.link_to15995.cljs$lang$applyTo = (function (seq15996){
var G__15997 = cljs.core.first(seq15996);
var seq15996__$1 = cljs.core.next(seq15996);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__15997,seq15996__$1);
}));


sablono.core.link_to = sablono.core.wrap_attrs(sablono.core.link_to15995);
/**
 * Wraps some content in a HTML hyperlink with the supplied e-mail
 *   address. If no content provided use the e-mail address as content.
 */
sablono.core.mail_to16000 = (function sablono$core$mail_to16000(var_args){
var args__4742__auto__ = [];
var len__4736__auto___16283 = arguments.length;
var i__4737__auto___16284 = (0);
while(true){
if((i__4737__auto___16284 < len__4736__auto___16283)){
args__4742__auto__.push((arguments[i__4737__auto___16284]));

var G__16285 = (i__4737__auto___16284 + (1));
i__4737__auto___16284 = G__16285;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return sablono.core.mail_to16000.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(sablono.core.mail_to16000.cljs$core$IFn$_invoke$arity$variadic = (function (e_mail,p__16020){
var vec__16021 = p__16020;
var content = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16021,(0),null);
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"a","a",-2123407586),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"href","href",-793805698),["mailto:",cljs.core.str.cljs$core$IFn$_invoke$arity$1(e_mail)].join('')], null),(function (){var or__4126__auto__ = content;
if(cljs.core.truth_(or__4126__auto__)){
return or__4126__auto__;
} else {
return e_mail;
}
})()], null);
}));

(sablono.core.mail_to16000.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(sablono.core.mail_to16000.cljs$lang$applyTo = (function (seq16018){
var G__16019 = cljs.core.first(seq16018);
var seq16018__$1 = cljs.core.next(seq16018);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__16019,seq16018__$1);
}));


sablono.core.mail_to = sablono.core.wrap_attrs(sablono.core.mail_to16000);
/**
 * Wrap a collection in an unordered list.
 */
sablono.core.unordered_list16024 = (function sablono$core$unordered_list16024(coll){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ul","ul",-1349521403),(function (){var iter__4529__auto__ = (function sablono$core$unordered_list16024_$_iter__16025(s__16026){
return (new cljs.core.LazySeq(null,(function (){
var s__16026__$1 = s__16026;
while(true){
var temp__5735__auto__ = cljs.core.seq(s__16026__$1);
if(temp__5735__auto__){
var s__16026__$2 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_(s__16026__$2)){
var c__4527__auto__ = cljs.core.chunk_first(s__16026__$2);
var size__4528__auto__ = cljs.core.count(c__4527__auto__);
var b__16028 = cljs.core.chunk_buffer(size__4528__auto__);
if((function (){var i__16027 = (0);
while(true){
if((i__16027 < size__4528__auto__)){
var x = cljs.core._nth(c__4527__auto__,i__16027);
cljs.core.chunk_append(b__16028,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"li","li",723558921),x], null));

var G__16286 = (i__16027 + (1));
i__16027 = G__16286;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__16028),sablono$core$unordered_list16024_$_iter__16025(cljs.core.chunk_rest(s__16026__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__16028),null);
}
} else {
var x = cljs.core.first(s__16026__$2);
return cljs.core.cons(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"li","li",723558921),x], null),sablono$core$unordered_list16024_$_iter__16025(cljs.core.rest(s__16026__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__(coll);
})()], null);
});

sablono.core.unordered_list = sablono.core.wrap_attrs(sablono.core.unordered_list16024);
/**
 * Wrap a collection in an ordered list.
 */
sablono.core.ordered_list16029 = (function sablono$core$ordered_list16029(coll){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"ol","ol",932524051),(function (){var iter__4529__auto__ = (function sablono$core$ordered_list16029_$_iter__16030(s__16031){
return (new cljs.core.LazySeq(null,(function (){
var s__16031__$1 = s__16031;
while(true){
var temp__5735__auto__ = cljs.core.seq(s__16031__$1);
if(temp__5735__auto__){
var s__16031__$2 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_(s__16031__$2)){
var c__4527__auto__ = cljs.core.chunk_first(s__16031__$2);
var size__4528__auto__ = cljs.core.count(c__4527__auto__);
var b__16033 = cljs.core.chunk_buffer(size__4528__auto__);
if((function (){var i__16032 = (0);
while(true){
if((i__16032 < size__4528__auto__)){
var x = cljs.core._nth(c__4527__auto__,i__16032);
cljs.core.chunk_append(b__16033,new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"li","li",723558921),x], null));

var G__16288 = (i__16032 + (1));
i__16032 = G__16288;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__16033),sablono$core$ordered_list16029_$_iter__16030(cljs.core.chunk_rest(s__16031__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__16033),null);
}
} else {
var x = cljs.core.first(s__16031__$2);
return cljs.core.cons(new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"li","li",723558921),x], null),sablono$core$ordered_list16029_$_iter__16030(cljs.core.rest(s__16031__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__(coll);
})()], null);
});

sablono.core.ordered_list = sablono.core.wrap_attrs(sablono.core.ordered_list16029);
/**
 * Create an image element.
 */
sablono.core.image16071 = (function sablono$core$image16071(var_args){
var G__16073 = arguments.length;
switch (G__16073) {
case 1:
return sablono.core.image16071.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.image16071.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.image16071.cljs$core$IFn$_invoke$arity$1 = (function (src){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"img","img",1442687358),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"src","src",-1651076051),sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([src], 0))], null)], null);
}));

(sablono.core.image16071.cljs$core$IFn$_invoke$arity$2 = (function (src,alt){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"img","img",1442687358),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"src","src",-1651076051),sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([src], 0)),new cljs.core.Keyword(null,"alt","alt",-3214426),alt], null)], null);
}));

(sablono.core.image16071.cljs$lang$maxFixedArity = 2);


sablono.core.image = sablono.core.wrap_attrs(sablono.core.image16071);
sablono.core._STAR_group_STAR_ = cljs.core.PersistentVector.EMPTY;
/**
 * Create a field name from the supplied argument the current field group.
 */
sablono.core.make_name = (function sablono$core$make_name(name){
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$2((function (p1__16074_SHARP_,p2__16075_SHARP_){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(p1__16074_SHARP_),"[",cljs.core.str.cljs$core$IFn$_invoke$arity$1(p2__16075_SHARP_),"]"].join('');
}),cljs.core.conj.cljs$core$IFn$_invoke$arity$2(sablono.core._STAR_group_STAR_,sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([name], 0))));
});
/**
 * Create a field id from the supplied argument and current field group.
 */
sablono.core.make_id = (function sablono$core$make_id(name){
return cljs.core.reduce.cljs$core$IFn$_invoke$arity$2((function (p1__16076_SHARP_,p2__16077_SHARP_){
return [cljs.core.str.cljs$core$IFn$_invoke$arity$1(p1__16076_SHARP_),"-",cljs.core.str.cljs$core$IFn$_invoke$arity$1(p2__16077_SHARP_)].join('');
}),cljs.core.conj.cljs$core$IFn$_invoke$arity$2(sablono.core._STAR_group_STAR_,sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([name], 0))));
});
/**
 * Creates a new <input> element.
 */
sablono.core.input_field_STAR_ = (function sablono$core$input_field_STAR_(var_args){
var G__16101 = arguments.length;
switch (G__16101) {
case 2:
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2 = (function (type,name){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),type,new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name)], null)], null);
}));

(sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3 = (function (type,name,value){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"type","type",1174270348),type,new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name),new cljs.core.Keyword(null,"value","value",305978217),(function (){var or__4126__auto__ = value;
if(cljs.core.truth_(or__4126__auto__)){
return or__4126__auto__;
} else {
return undefined;
}
})()], null)], null);
}));

(sablono.core.input_field_STAR_.cljs$lang$maxFixedArity = 3);

/**
 * Creates a color input field.
 */
sablono.core.color_field16102 = (function sablono$core$color_field16102(var_args){
var G__16104 = arguments.length;
switch (G__16104) {
case 1:
return sablono.core.color_field16102.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.color_field16102.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.color_field16102.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"color","color",-1642760596,null)),name__15937__auto__);
}));

(sablono.core.color_field16102.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"color","color",-1642760596,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.color_field16102.cljs$lang$maxFixedArity = 2);


sablono.core.color_field = sablono.core.wrap_attrs(sablono.core.color_field16102);

/**
 * Creates a date input field.
 */
sablono.core.date_field16105 = (function sablono$core$date_field16105(var_args){
var G__16107 = arguments.length;
switch (G__16107) {
case 1:
return sablono.core.date_field16105.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.date_field16105.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.date_field16105.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"date","date",177097065,null)),name__15937__auto__);
}));

(sablono.core.date_field16105.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"date","date",177097065,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.date_field16105.cljs$lang$maxFixedArity = 2);


sablono.core.date_field = sablono.core.wrap_attrs(sablono.core.date_field16105);

/**
 * Creates a datetime input field.
 */
sablono.core.datetime_field16109 = (function sablono$core$datetime_field16109(var_args){
var G__16111 = arguments.length;
switch (G__16111) {
case 1:
return sablono.core.datetime_field16109.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.datetime_field16109.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.datetime_field16109.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"datetime","datetime",2135207229,null)),name__15937__auto__);
}));

(sablono.core.datetime_field16109.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"datetime","datetime",2135207229,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.datetime_field16109.cljs$lang$maxFixedArity = 2);


sablono.core.datetime_field = sablono.core.wrap_attrs(sablono.core.datetime_field16109);

/**
 * Creates a datetime-local input field.
 */
sablono.core.datetime_local_field16112 = (function sablono$core$datetime_local_field16112(var_args){
var G__16114 = arguments.length;
switch (G__16114) {
case 1:
return sablono.core.datetime_local_field16112.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.datetime_local_field16112.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.datetime_local_field16112.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"datetime-local","datetime-local",-507312697,null)),name__15937__auto__);
}));

(sablono.core.datetime_local_field16112.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"datetime-local","datetime-local",-507312697,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.datetime_local_field16112.cljs$lang$maxFixedArity = 2);


sablono.core.datetime_local_field = sablono.core.wrap_attrs(sablono.core.datetime_local_field16112);

/**
 * Creates a email input field.
 */
sablono.core.email_field16115 = (function sablono$core$email_field16115(var_args){
var G__16117 = arguments.length;
switch (G__16117) {
case 1:
return sablono.core.email_field16115.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.email_field16115.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.email_field16115.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"email","email",-1238619063,null)),name__15937__auto__);
}));

(sablono.core.email_field16115.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"email","email",-1238619063,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.email_field16115.cljs$lang$maxFixedArity = 2);


sablono.core.email_field = sablono.core.wrap_attrs(sablono.core.email_field16115);

/**
 * Creates a file input field.
 */
sablono.core.file_field16118 = (function sablono$core$file_field16118(var_args){
var G__16120 = arguments.length;
switch (G__16120) {
case 1:
return sablono.core.file_field16118.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.file_field16118.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.file_field16118.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"file","file",370885649,null)),name__15937__auto__);
}));

(sablono.core.file_field16118.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"file","file",370885649,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.file_field16118.cljs$lang$maxFixedArity = 2);


sablono.core.file_field = sablono.core.wrap_attrs(sablono.core.file_field16118);

/**
 * Creates a hidden input field.
 */
sablono.core.hidden_field16139 = (function sablono$core$hidden_field16139(var_args){
var G__16141 = arguments.length;
switch (G__16141) {
case 1:
return sablono.core.hidden_field16139.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.hidden_field16139.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.hidden_field16139.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"hidden","hidden",1328025435,null)),name__15937__auto__);
}));

(sablono.core.hidden_field16139.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"hidden","hidden",1328025435,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.hidden_field16139.cljs$lang$maxFixedArity = 2);


sablono.core.hidden_field = sablono.core.wrap_attrs(sablono.core.hidden_field16139);

/**
 * Creates a month input field.
 */
sablono.core.month_field16142 = (function sablono$core$month_field16142(var_args){
var G__16144 = arguments.length;
switch (G__16144) {
case 1:
return sablono.core.month_field16142.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.month_field16142.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.month_field16142.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"month","month",-319717006,null)),name__15937__auto__);
}));

(sablono.core.month_field16142.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"month","month",-319717006,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.month_field16142.cljs$lang$maxFixedArity = 2);


sablono.core.month_field = sablono.core.wrap_attrs(sablono.core.month_field16142);

/**
 * Creates a number input field.
 */
sablono.core.number_field16145 = (function sablono$core$number_field16145(var_args){
var G__16147 = arguments.length;
switch (G__16147) {
case 1:
return sablono.core.number_field16145.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.number_field16145.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.number_field16145.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"number","number",-1084057331,null)),name__15937__auto__);
}));

(sablono.core.number_field16145.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"number","number",-1084057331,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.number_field16145.cljs$lang$maxFixedArity = 2);


sablono.core.number_field = sablono.core.wrap_attrs(sablono.core.number_field16145);

/**
 * Creates a password input field.
 */
sablono.core.password_field16184 = (function sablono$core$password_field16184(var_args){
var G__16186 = arguments.length;
switch (G__16186) {
case 1:
return sablono.core.password_field16184.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.password_field16184.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.password_field16184.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"password","password",2057553998,null)),name__15937__auto__);
}));

(sablono.core.password_field16184.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"password","password",2057553998,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.password_field16184.cljs$lang$maxFixedArity = 2);


sablono.core.password_field = sablono.core.wrap_attrs(sablono.core.password_field16184);

/**
 * Creates a range input field.
 */
sablono.core.range_field16187 = (function sablono$core$range_field16187(var_args){
var G__16189 = arguments.length;
switch (G__16189) {
case 1:
return sablono.core.range_field16187.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.range_field16187.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.range_field16187.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"range","range",-1014743483,null)),name__15937__auto__);
}));

(sablono.core.range_field16187.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"range","range",-1014743483,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.range_field16187.cljs$lang$maxFixedArity = 2);


sablono.core.range_field = sablono.core.wrap_attrs(sablono.core.range_field16187);

/**
 * Creates a search input field.
 */
sablono.core.search_field16190 = (function sablono$core$search_field16190(var_args){
var G__16192 = arguments.length;
switch (G__16192) {
case 1:
return sablono.core.search_field16190.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.search_field16190.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.search_field16190.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"search","search",-1089495947,null)),name__15937__auto__);
}));

(sablono.core.search_field16190.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"search","search",-1089495947,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.search_field16190.cljs$lang$maxFixedArity = 2);


sablono.core.search_field = sablono.core.wrap_attrs(sablono.core.search_field16190);

/**
 * Creates a tel input field.
 */
sablono.core.tel_field16193 = (function sablono$core$tel_field16193(var_args){
var G__16195 = arguments.length;
switch (G__16195) {
case 1:
return sablono.core.tel_field16193.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.tel_field16193.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.tel_field16193.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"tel","tel",1864669686,null)),name__15937__auto__);
}));

(sablono.core.tel_field16193.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"tel","tel",1864669686,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.tel_field16193.cljs$lang$maxFixedArity = 2);


sablono.core.tel_field = sablono.core.wrap_attrs(sablono.core.tel_field16193);

/**
 * Creates a text input field.
 */
sablono.core.text_field16202 = (function sablono$core$text_field16202(var_args){
var G__16204 = arguments.length;
switch (G__16204) {
case 1:
return sablono.core.text_field16202.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.text_field16202.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.text_field16202.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"text","text",-150030170,null)),name__15937__auto__);
}));

(sablono.core.text_field16202.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"text","text",-150030170,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.text_field16202.cljs$lang$maxFixedArity = 2);


sablono.core.text_field = sablono.core.wrap_attrs(sablono.core.text_field16202);

/**
 * Creates a time input field.
 */
sablono.core.time_field16205 = (function sablono$core$time_field16205(var_args){
var G__16207 = arguments.length;
switch (G__16207) {
case 1:
return sablono.core.time_field16205.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.time_field16205.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.time_field16205.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"time","time",-1268547887,null)),name__15937__auto__);
}));

(sablono.core.time_field16205.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"time","time",-1268547887,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.time_field16205.cljs$lang$maxFixedArity = 2);


sablono.core.time_field = sablono.core.wrap_attrs(sablono.core.time_field16205);

/**
 * Creates a url input field.
 */
sablono.core.url_field16208 = (function sablono$core$url_field16208(var_args){
var G__16210 = arguments.length;
switch (G__16210) {
case 1:
return sablono.core.url_field16208.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.url_field16208.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.url_field16208.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"url","url",1916828573,null)),name__15937__auto__);
}));

(sablono.core.url_field16208.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"url","url",1916828573,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.url_field16208.cljs$lang$maxFixedArity = 2);


sablono.core.url_field = sablono.core.wrap_attrs(sablono.core.url_field16208);

/**
 * Creates a week input field.
 */
sablono.core.week_field16214 = (function sablono$core$week_field16214(var_args){
var G__16216 = arguments.length;
switch (G__16216) {
case 1:
return sablono.core.week_field16214.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.week_field16214.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.week_field16214.cljs$core$IFn$_invoke$arity$1 = (function (name__15937__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$2(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"week","week",314058249,null)),name__15937__auto__);
}));

(sablono.core.week_field16214.cljs$core$IFn$_invoke$arity$2 = (function (name__15937__auto__,value__15938__auto__){
return sablono.core.input_field_STAR_.cljs$core$IFn$_invoke$arity$3(cljs.core.str.cljs$core$IFn$_invoke$arity$1(new cljs.core.Symbol(null,"week","week",314058249,null)),name__15937__auto__,value__15938__auto__);
}));

(sablono.core.week_field16214.cljs$lang$maxFixedArity = 2);


sablono.core.week_field = sablono.core.wrap_attrs(sablono.core.week_field16214);
sablono.core.file_upload = sablono.core.file_field;
/**
 * Creates a check box.
 */
sablono.core.check_box16217 = (function sablono$core$check_box16217(var_args){
var G__16219 = arguments.length;
switch (G__16219) {
case 1:
return sablono.core.check_box16217.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.check_box16217.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return sablono.core.check_box16217.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.check_box16217.cljs$core$IFn$_invoke$arity$1 = (function (name){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),"checkbox",new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name)], null)], null);
}));

(sablono.core.check_box16217.cljs$core$IFn$_invoke$arity$2 = (function (name,checked_QMARK_){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"type","type",1174270348),"checkbox",new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name),new cljs.core.Keyword(null,"checked","checked",-50955819),checked_QMARK_], null)], null);
}));

(sablono.core.check_box16217.cljs$core$IFn$_invoke$arity$3 = (function (name,checked_QMARK_,value){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"type","type",1174270348),"checkbox",new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name),new cljs.core.Keyword(null,"value","value",305978217),value,new cljs.core.Keyword(null,"checked","checked",-50955819),checked_QMARK_], null)], null);
}));

(sablono.core.check_box16217.cljs$lang$maxFixedArity = 3);


sablono.core.check_box = sablono.core.wrap_attrs(sablono.core.check_box16217);
/**
 * Creates a radio button.
 */
sablono.core.radio_button16220 = (function sablono$core$radio_button16220(var_args){
var G__16231 = arguments.length;
switch (G__16231) {
case 1:
return sablono.core.radio_button16220.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.radio_button16220.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return sablono.core.radio_button16220.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.radio_button16220.cljs$core$IFn$_invoke$arity$1 = (function (group){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"type","type",1174270348),"radio",new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(group),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([group], 0)))], null)], null);
}));

(sablono.core.radio_button16220.cljs$core$IFn$_invoke$arity$2 = (function (group,checked_QMARK_){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 4, [new cljs.core.Keyword(null,"type","type",1174270348),"radio",new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(group),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([group], 0))),new cljs.core.Keyword(null,"checked","checked",-50955819),checked_QMARK_], null)], null);
}));

(sablono.core.radio_button16220.cljs$core$IFn$_invoke$arity$3 = (function (group,checked_QMARK_,value){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 5, [new cljs.core.Keyword(null,"type","type",1174270348),"radio",new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(group),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id([sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([group], 0)),"-",sablono.util.as_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([value], 0))].join('')),new cljs.core.Keyword(null,"value","value",305978217),value,new cljs.core.Keyword(null,"checked","checked",-50955819),checked_QMARK_], null)], null);
}));

(sablono.core.radio_button16220.cljs$lang$maxFixedArity = 3);


sablono.core.radio_button = sablono.core.wrap_attrs(sablono.core.radio_button16220);
sablono.core.hash_key = (function sablono$core$hash_key(x){
return goog.string.hashCode(cljs.core.pr_str.cljs$core$IFn$_invoke$arity$variadic(cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([x], 0)));
});
/**
 * Creates a seq of option tags from a collection.
 */
sablono.core.select_options16232 = (function sablono$core$select_options16232(coll){
var iter__4529__auto__ = (function sablono$core$select_options16232_$_iter__16233(s__16234){
return (new cljs.core.LazySeq(null,(function (){
var s__16234__$1 = s__16234;
while(true){
var temp__5735__auto__ = cljs.core.seq(s__16234__$1);
if(temp__5735__auto__){
var s__16234__$2 = temp__5735__auto__;
if(cljs.core.chunked_seq_QMARK_(s__16234__$2)){
var c__4527__auto__ = cljs.core.chunk_first(s__16234__$2);
var size__4528__auto__ = cljs.core.count(c__4527__auto__);
var b__16236 = cljs.core.chunk_buffer(size__4528__auto__);
if((function (){var i__16235 = (0);
while(true){
if((i__16235 < size__4528__auto__)){
var x = cljs.core._nth(c__4527__auto__,i__16235);
cljs.core.chunk_append(b__16236,((cljs.core.sequential_QMARK_(x))?(function (){var vec__16237 = x;
var text = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16237,(0),null);
var val = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16237,(1),null);
var disabled_QMARK_ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16237,(2),null);
var disabled_QMARK___$1 = cljs.core.boolean$(disabled_QMARK_);
if(cljs.core.sequential_QMARK_(val)){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"optgroup","optgroup",1738282218),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"key","key",-1516042587),sablono.core.hash_key(text),new cljs.core.Keyword(null,"label","label",1718410804),text], null),(sablono.core.select_options16232.cljs$core$IFn$_invoke$arity$1 ? sablono.core.select_options16232.cljs$core$IFn$_invoke$arity$1(val) : sablono.core.select_options16232.call(null,val))], null);
} else {
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"disabled","disabled",-1529784218),disabled_QMARK___$1,new cljs.core.Keyword(null,"key","key",-1516042587),sablono.core.hash_key(val),new cljs.core.Keyword(null,"value","value",305978217),val], null),text], null);
}
})():new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"key","key",-1516042587),sablono.core.hash_key(x),new cljs.core.Keyword(null,"value","value",305978217),x], null),x], null)));

var G__16320 = (i__16235 + (1));
i__16235 = G__16320;
continue;
} else {
return true;
}
break;
}
})()){
return cljs.core.chunk_cons(cljs.core.chunk(b__16236),sablono$core$select_options16232_$_iter__16233(cljs.core.chunk_rest(s__16234__$2)));
} else {
return cljs.core.chunk_cons(cljs.core.chunk(b__16236),null);
}
} else {
var x = cljs.core.first(s__16234__$2);
return cljs.core.cons(((cljs.core.sequential_QMARK_(x))?(function (){var vec__16240 = x;
var text = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16240,(0),null);
var val = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16240,(1),null);
var disabled_QMARK_ = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16240,(2),null);
var disabled_QMARK___$1 = cljs.core.boolean$(disabled_QMARK_);
if(cljs.core.sequential_QMARK_(val)){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"optgroup","optgroup",1738282218),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"key","key",-1516042587),sablono.core.hash_key(text),new cljs.core.Keyword(null,"label","label",1718410804),text], null),(sablono.core.select_options16232.cljs$core$IFn$_invoke$arity$1 ? sablono.core.select_options16232.cljs$core$IFn$_invoke$arity$1(val) : sablono.core.select_options16232.call(null,val))], null);
} else {
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"disabled","disabled",-1529784218),disabled_QMARK___$1,new cljs.core.Keyword(null,"key","key",-1516042587),sablono.core.hash_key(val),new cljs.core.Keyword(null,"value","value",305978217),val], null),text], null);
}
})():new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"option","option",65132272),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"key","key",-1516042587),sablono.core.hash_key(x),new cljs.core.Keyword(null,"value","value",305978217),x], null),x], null)),sablono$core$select_options16232_$_iter__16233(cljs.core.rest(s__16234__$2)));
}
} else {
return null;
}
break;
}
}),null,null));
});
return iter__4529__auto__(coll);
});

sablono.core.select_options = sablono.core.wrap_attrs(sablono.core.select_options16232);
/**
 * Creates a drop-down box using the <select> tag.
 */
sablono.core.drop_down16248 = (function sablono$core$drop_down16248(var_args){
var G__16250 = arguments.length;
switch (G__16250) {
case 2:
return sablono.core.drop_down16248.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
case 3:
return sablono.core.drop_down16248.cljs$core$IFn$_invoke$arity$3((arguments[(0)]),(arguments[(1)]),(arguments[(2)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.drop_down16248.cljs$core$IFn$_invoke$arity$2 = (function (name,options){
return sablono.core.drop_down16248.cljs$core$IFn$_invoke$arity$3(name,options,null);
}));

(sablono.core.drop_down16248.cljs$core$IFn$_invoke$arity$3 = (function (name,options,selected){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"select","select",1147833503),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name)], null),sablono.core.select_options(options,selected)], null);
}));

(sablono.core.drop_down16248.cljs$lang$maxFixedArity = 3);


sablono.core.drop_down = sablono.core.wrap_attrs(sablono.core.drop_down16248);
/**
 * Creates a text area element.
 */
sablono.core.text_area16251 = (function sablono$core$text_area16251(var_args){
var G__16253 = arguments.length;
switch (G__16253) {
case 1:
return sablono.core.text_area16251.cljs$core$IFn$_invoke$arity$1((arguments[(0)]));

break;
case 2:
return sablono.core.text_area16251.cljs$core$IFn$_invoke$arity$2((arguments[(0)]),(arguments[(1)]));

break;
default:
throw (new Error(["Invalid arity: ",cljs.core.str.cljs$core$IFn$_invoke$arity$1(arguments.length)].join('')));

}
});

(sablono.core.text_area16251.cljs$core$IFn$_invoke$arity$1 = (function (name){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"textarea","textarea",-650375824),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name)], null)], null);
}));

(sablono.core.text_area16251.cljs$core$IFn$_invoke$arity$2 = (function (name,value){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"textarea","textarea",-650375824),new cljs.core.PersistentArrayMap(null, 3, [new cljs.core.Keyword(null,"name","name",1843675177),sablono.core.make_name(name),new cljs.core.Keyword(null,"id","id",-1388402092),sablono.core.make_id(name),new cljs.core.Keyword(null,"value","value",305978217),(function (){var or__4126__auto__ = value;
if(cljs.core.truth_(or__4126__auto__)){
return or__4126__auto__;
} else {
return undefined;
}
})()], null)], null);
}));

(sablono.core.text_area16251.cljs$lang$maxFixedArity = 2);


sablono.core.text_area = sablono.core.wrap_attrs(sablono.core.text_area16251);
/**
 * Creates a label for an input field with the supplied name.
 */
sablono.core.label16254 = (function sablono$core$label16254(name,text){
return new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"label","label",1718410804),new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"htmlFor","htmlFor",-1050291720),sablono.core.make_id(name)], null),text], null);
});

sablono.core.label = sablono.core.wrap_attrs(sablono.core.label16254);
/**
 * Creates a submit button.
 */
sablono.core.submit_button16255 = (function sablono$core$submit_button16255(text){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),"submit",new cljs.core.Keyword(null,"value","value",305978217),text], null)], null);
});

sablono.core.submit_button = sablono.core.wrap_attrs(sablono.core.submit_button16255);
/**
 * Creates a form reset button.
 */
sablono.core.reset_button16256 = (function sablono$core$reset_button16256(text){
return new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"input","input",556931961),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"type","type",1174270348),"reset",new cljs.core.Keyword(null,"value","value",305978217),text], null)], null);
});

sablono.core.reset_button = sablono.core.wrap_attrs(sablono.core.reset_button16256);
/**
 * Create a form that points to a particular method and route.
 *   e.g. (form-to [:put "/post"]
 *       ...)
 */
sablono.core.form_to16262 = (function sablono$core$form_to16262(var_args){
var args__4742__auto__ = [];
var len__4736__auto___16337 = arguments.length;
var i__4737__auto___16338 = (0);
while(true){
if((i__4737__auto___16338 < len__4736__auto___16337)){
args__4742__auto__.push((arguments[i__4737__auto___16338]));

var G__16339 = (i__4737__auto___16338 + (1));
i__4737__auto___16338 = G__16339;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return sablono.core.form_to16262.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(sablono.core.form_to16262.cljs$core$IFn$_invoke$arity$variadic = (function (p__16265,body){
var vec__16266 = p__16265;
var method = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16266,(0),null);
var action = cljs.core.nth.cljs$core$IFn$_invoke$arity$3(vec__16266,(1),null);
var method_str = clojure.string.upper_case(cljs.core.name(method));
var action_uri = sablono.util.to_uri(action);
return cljs.core.vec(cljs.core.concat.cljs$core$IFn$_invoke$arity$2(((cljs.core.contains_QMARK_(new cljs.core.PersistentHashSet(null, new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"get","get",1683182755),null,new cljs.core.Keyword(null,"post","post",269697687),null], null), null),method))?new cljs.core.PersistentVector(null, 2, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"form","form",-1624062471),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"method","method",55703592),method_str,new cljs.core.Keyword(null,"action","action",-811238024),action_uri], null)], null):new cljs.core.PersistentVector(null, 3, 5, cljs.core.PersistentVector.EMPTY_NODE, [new cljs.core.Keyword(null,"form","form",-1624062471),new cljs.core.PersistentArrayMap(null, 2, [new cljs.core.Keyword(null,"method","method",55703592),"POST",new cljs.core.Keyword(null,"action","action",-811238024),action_uri], null),sablono.core.hidden_field(new cljs.core.PersistentArrayMap(null, 1, [new cljs.core.Keyword(null,"key","key",-1516042587),(3735928559)], null),"_method",method_str)], null)),body));
}));

(sablono.core.form_to16262.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(sablono.core.form_to16262.cljs$lang$applyTo = (function (seq16263){
var G__16264 = cljs.core.first(seq16263);
var seq16263__$1 = cljs.core.next(seq16263);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__16264,seq16263__$1);
}));


sablono.core.form_to = sablono.core.wrap_attrs(sablono.core.form_to16262);

//# sourceMappingURL=sablono.core.js.map
