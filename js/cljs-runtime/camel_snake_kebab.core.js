goog.provide('camel_snake_kebab.core');



























/**
 * Converts the case of a string according to the rule for the first
 *   word, remaining words, and the separator.
 */
camel_snake_kebab.core.convert_case = (function camel_snake_kebab$core$convert_case(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23899 = arguments.length;
var i__4737__auto___23900 = (0);
while(true){
if((i__4737__auto___23900 < len__4736__auto___23899)){
args__4742__auto__.push((arguments[i__4737__auto___23900]));

var G__23901 = (i__4737__auto___23900 + (1));
i__4737__auto___23900 = G__23901;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((4) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((4)),(0),null)):null);
return camel_snake_kebab.core.convert_case.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),(arguments[(1)]),(arguments[(2)]),(arguments[(3)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.convert_case.cljs$core$IFn$_invoke$arity$variadic = (function (first_fn,rest_fn,sep,s,rest){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,first_fn,rest_fn,sep,s,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest], 0));
}));

(camel_snake_kebab.core.convert_case.cljs$lang$maxFixedArity = (4));

/** @this {Function} */
(camel_snake_kebab.core.convert_case.cljs$lang$applyTo = (function (seq23838){
var G__23839 = cljs.core.first(seq23838);
var seq23838__$1 = cljs.core.next(seq23838);
var G__23840 = cljs.core.first(seq23838__$1);
var seq23838__$2 = cljs.core.next(seq23838__$1);
var G__23841 = cljs.core.first(seq23838__$2);
var seq23838__$3 = cljs.core.next(seq23838__$2);
var G__23842 = cljs.core.first(seq23838__$3);
var seq23838__$4 = cljs.core.next(seq23838__$3);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23839,G__23840,G__23841,G__23842,seq23838__$4);
}));

camel_snake_kebab.core.__GT_PascalCase = (function camel_snake_kebab$core$__GT_PascalCase(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23907 = arguments.length;
var i__4737__auto___23908 = (0);
while(true){
if((i__4737__auto___23908 < len__4736__auto___23907)){
args__4742__auto__.push((arguments[i__4737__auto___23908]));

var G__23909 = (i__4737__auto___23908 + (1));
i__4737__auto___23908 = G__23909;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_PascalCase.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_PascalCase.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_PascalCase.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_PascalCase.cljs$lang$applyTo = (function (seq23843){
var G__23844 = cljs.core.first(seq23843);
var seq23843__$1 = cljs.core.next(seq23843);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23844,seq23843__$1);
}));


camel_snake_kebab.core.__GT_PascalCaseString = (function camel_snake_kebab$core$__GT_PascalCaseString(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23910 = arguments.length;
var i__4737__auto___23911 = (0);
while(true){
if((i__4737__auto___23911 < len__4736__auto___23910)){
args__4742__auto__.push((arguments[i__4737__auto___23911]));

var G__23912 = (i__4737__auto___23911 + (1));
i__4737__auto___23911 = G__23912;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_PascalCaseString.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_PascalCaseString.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_PascalCaseString.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_PascalCaseString.cljs$lang$applyTo = (function (seq23845){
var G__23846 = cljs.core.first(seq23845);
var seq23845__$1 = cljs.core.next(seq23845);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23846,seq23845__$1);
}));


camel_snake_kebab.core.__GT_PascalCaseSymbol = (function camel_snake_kebab$core$__GT_PascalCaseSymbol(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23913 = arguments.length;
var i__4737__auto___23914 = (0);
while(true){
if((i__4737__auto___23914 < len__4736__auto___23913)){
args__4742__auto__.push((arguments[i__4737__auto___23914]));

var G__23915 = (i__4737__auto___23914 + (1));
i__4737__auto___23914 = G__23915;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_PascalCaseSymbol.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_PascalCaseSymbol.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_PascalCaseSymbol.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_PascalCaseSymbol.cljs$lang$applyTo = (function (seq23847){
var G__23848 = cljs.core.first(seq23847);
var seq23847__$1 = cljs.core.next(seq23847);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23848,seq23847__$1);
}));


camel_snake_kebab.core.__GT_PascalCaseKeyword = (function camel_snake_kebab$core$__GT_PascalCaseKeyword(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23916 = arguments.length;
var i__4737__auto___23917 = (0);
while(true){
if((i__4737__auto___23917 < len__4736__auto___23916)){
args__4742__auto__.push((arguments[i__4737__auto___23917]));

var G__23918 = (i__4737__auto___23917 + (1));
i__4737__auto___23917 = G__23918;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_PascalCaseKeyword.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_PascalCaseKeyword.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_PascalCaseKeyword.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_PascalCaseKeyword.cljs$lang$applyTo = (function (seq23849){
var G__23850 = cljs.core.first(seq23849);
var seq23849__$1 = cljs.core.next(seq23849);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23850,seq23849__$1);
}));

camel_snake_kebab.core.__GT_Camel_Snake_Case = (function camel_snake_kebab$core$__GT_Camel_Snake_Case(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23919 = arguments.length;
var i__4737__auto___23920 = (0);
while(true){
if((i__4737__auto___23920 < len__4736__auto___23919)){
args__4742__auto__.push((arguments[i__4737__auto___23920]));

var G__23921 = (i__4737__auto___23920 + (1));
i__4737__auto___23920 = G__23921;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_Camel_Snake_Case.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_Camel_Snake_Case.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"_",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_Camel_Snake_Case.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_Camel_Snake_Case.cljs$lang$applyTo = (function (seq23851){
var G__23852 = cljs.core.first(seq23851);
var seq23851__$1 = cljs.core.next(seq23851);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23852,seq23851__$1);
}));


camel_snake_kebab.core.__GT_Camel_Snake_Case_String = (function camel_snake_kebab$core$__GT_Camel_Snake_Case_String(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23922 = arguments.length;
var i__4737__auto___23923 = (0);
while(true){
if((i__4737__auto___23923 < len__4736__auto___23922)){
args__4742__auto__.push((arguments[i__4737__auto___23923]));

var G__23924 = (i__4737__auto___23923 + (1));
i__4737__auto___23923 = G__23924;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_Camel_Snake_Case_String.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_Camel_Snake_Case_String.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_Camel_Snake_Case_String.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_Camel_Snake_Case_String.cljs$lang$applyTo = (function (seq23853){
var G__23854 = cljs.core.first(seq23853);
var seq23853__$1 = cljs.core.next(seq23853);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23854,seq23853__$1);
}));


camel_snake_kebab.core.__GT_Camel_Snake_Case_Symbol = (function camel_snake_kebab$core$__GT_Camel_Snake_Case_Symbol(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23931 = arguments.length;
var i__4737__auto___23932 = (0);
while(true){
if((i__4737__auto___23932 < len__4736__auto___23931)){
args__4742__auto__.push((arguments[i__4737__auto___23932]));

var G__23933 = (i__4737__auto___23932 + (1));
i__4737__auto___23932 = G__23933;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_Camel_Snake_Case_Symbol.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_Camel_Snake_Case_Symbol.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_Camel_Snake_Case_Symbol.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_Camel_Snake_Case_Symbol.cljs$lang$applyTo = (function (seq23855){
var G__23856 = cljs.core.first(seq23855);
var seq23855__$1 = cljs.core.next(seq23855);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23856,seq23855__$1);
}));


camel_snake_kebab.core.__GT_Camel_Snake_Case_Keyword = (function camel_snake_kebab$core$__GT_Camel_Snake_Case_Keyword(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23937 = arguments.length;
var i__4737__auto___23938 = (0);
while(true){
if((i__4737__auto___23938 < len__4736__auto___23937)){
args__4742__auto__.push((arguments[i__4737__auto___23938]));

var G__23939 = (i__4737__auto___23938 + (1));
i__4737__auto___23938 = G__23939;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_Camel_Snake_Case_Keyword.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_Camel_Snake_Case_Keyword.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.capitalize,clojure.string.capitalize,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_Camel_Snake_Case_Keyword.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_Camel_Snake_Case_Keyword.cljs$lang$applyTo = (function (seq23857){
var G__23858 = cljs.core.first(seq23857);
var seq23857__$1 = cljs.core.next(seq23857);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23858,seq23857__$1);
}));

camel_snake_kebab.core.__GT_camelCase = (function camel_snake_kebab$core$__GT_camelCase(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23940 = arguments.length;
var i__4737__auto___23941 = (0);
while(true){
if((i__4737__auto___23941 < len__4736__auto___23940)){
args__4742__auto__.push((arguments[i__4737__auto___23941]));

var G__23942 = (i__4737__auto___23941 + (1));
i__4737__auto___23941 = G__23942;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_camelCase.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_camelCase.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.capitalize,"",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_camelCase.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_camelCase.cljs$lang$applyTo = (function (seq23859){
var G__23860 = cljs.core.first(seq23859);
var seq23859__$1 = cljs.core.next(seq23859);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23860,seq23859__$1);
}));


camel_snake_kebab.core.__GT_camelCaseString = (function camel_snake_kebab$core$__GT_camelCaseString(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23943 = arguments.length;
var i__4737__auto___23944 = (0);
while(true){
if((i__4737__auto___23944 < len__4736__auto___23943)){
args__4742__auto__.push((arguments[i__4737__auto___23944]));

var G__23945 = (i__4737__auto___23944 + (1));
i__4737__auto___23944 = G__23945;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_camelCaseString.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_camelCaseString.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.capitalize,"",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_camelCaseString.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_camelCaseString.cljs$lang$applyTo = (function (seq23861){
var G__23862 = cljs.core.first(seq23861);
var seq23861__$1 = cljs.core.next(seq23861);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23862,seq23861__$1);
}));


camel_snake_kebab.core.__GT_camelCaseSymbol = (function camel_snake_kebab$core$__GT_camelCaseSymbol(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23950 = arguments.length;
var i__4737__auto___23951 = (0);
while(true){
if((i__4737__auto___23951 < len__4736__auto___23950)){
args__4742__auto__.push((arguments[i__4737__auto___23951]));

var G__23952 = (i__4737__auto___23951 + (1));
i__4737__auto___23951 = G__23952;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_camelCaseSymbol.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_camelCaseSymbol.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.capitalize,"",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_camelCaseSymbol.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_camelCaseSymbol.cljs$lang$applyTo = (function (seq23863){
var G__23864 = cljs.core.first(seq23863);
var seq23863__$1 = cljs.core.next(seq23863);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23864,seq23863__$1);
}));


camel_snake_kebab.core.__GT_camelCaseKeyword = (function camel_snake_kebab$core$__GT_camelCaseKeyword(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23953 = arguments.length;
var i__4737__auto___23954 = (0);
while(true){
if((i__4737__auto___23954 < len__4736__auto___23953)){
args__4742__auto__.push((arguments[i__4737__auto___23954]));

var G__23955 = (i__4737__auto___23954 + (1));
i__4737__auto___23954 = G__23955;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_camelCaseKeyword.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_camelCaseKeyword.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.capitalize,"",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_camelCaseKeyword.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_camelCaseKeyword.cljs$lang$applyTo = (function (seq23865){
var G__23866 = cljs.core.first(seq23865);
var seq23865__$1 = cljs.core.next(seq23865);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23866,seq23865__$1);
}));

camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE = (function camel_snake_kebab$core$__GT_SCREAMING_SNAKE_CASE(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23956 = arguments.length;
var i__4737__auto___23957 = (0);
while(true){
if((i__4737__auto___23957 < len__4736__auto___23956)){
args__4742__auto__.push((arguments[i__4737__auto___23957]));

var G__23958 = (i__4737__auto___23957 + (1));
i__4737__auto___23957 = G__23958;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.upper_case,clojure.string.upper_case,"_",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE.cljs$lang$applyTo = (function (seq23867){
var G__23868 = cljs.core.first(seq23867);
var seq23867__$1 = cljs.core.next(seq23867);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23868,seq23867__$1);
}));


camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_STRING = (function camel_snake_kebab$core$__GT_SCREAMING_SNAKE_CASE_STRING(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23959 = arguments.length;
var i__4737__auto___23960 = (0);
while(true){
if((i__4737__auto___23960 < len__4736__auto___23959)){
args__4742__auto__.push((arguments[i__4737__auto___23960]));

var G__23961 = (i__4737__auto___23960 + (1));
i__4737__auto___23960 = G__23961;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_STRING.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_STRING.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.upper_case,clojure.string.upper_case,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_STRING.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_STRING.cljs$lang$applyTo = (function (seq23869){
var G__23870 = cljs.core.first(seq23869);
var seq23869__$1 = cljs.core.next(seq23869);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23870,seq23869__$1);
}));


camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_SYMBOL = (function camel_snake_kebab$core$__GT_SCREAMING_SNAKE_CASE_SYMBOL(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23962 = arguments.length;
var i__4737__auto___23963 = (0);
while(true){
if((i__4737__auto___23963 < len__4736__auto___23962)){
args__4742__auto__.push((arguments[i__4737__auto___23963]));

var G__23964 = (i__4737__auto___23963 + (1));
i__4737__auto___23963 = G__23964;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_SYMBOL.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_SYMBOL.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.upper_case,clojure.string.upper_case,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_SYMBOL.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_SYMBOL.cljs$lang$applyTo = (function (seq23871){
var G__23872 = cljs.core.first(seq23871);
var seq23871__$1 = cljs.core.next(seq23871);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23872,seq23871__$1);
}));


camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_KEYWORD = (function camel_snake_kebab$core$__GT_SCREAMING_SNAKE_CASE_KEYWORD(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23965 = arguments.length;
var i__4737__auto___23966 = (0);
while(true){
if((i__4737__auto___23966 < len__4736__auto___23965)){
args__4742__auto__.push((arguments[i__4737__auto___23966]));

var G__23967 = (i__4737__auto___23966 + (1));
i__4737__auto___23966 = G__23967;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_KEYWORD.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_KEYWORD.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.upper_case,clojure.string.upper_case,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_KEYWORD.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_SCREAMING_SNAKE_CASE_KEYWORD.cljs$lang$applyTo = (function (seq23873){
var G__23874 = cljs.core.first(seq23873);
var seq23873__$1 = cljs.core.next(seq23873);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23874,seq23873__$1);
}));

camel_snake_kebab.core.__GT_snake_case = (function camel_snake_kebab$core$__GT_snake_case(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23968 = arguments.length;
var i__4737__auto___23969 = (0);
while(true){
if((i__4737__auto___23969 < len__4736__auto___23968)){
args__4742__auto__.push((arguments[i__4737__auto___23969]));

var G__23970 = (i__4737__auto___23969 + (1));
i__4737__auto___23969 = G__23970;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_snake_case.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_snake_case.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"_",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_snake_case.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_snake_case.cljs$lang$applyTo = (function (seq23875){
var G__23876 = cljs.core.first(seq23875);
var seq23875__$1 = cljs.core.next(seq23875);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23876,seq23875__$1);
}));


camel_snake_kebab.core.__GT_snake_case_string = (function camel_snake_kebab$core$__GT_snake_case_string(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23971 = arguments.length;
var i__4737__auto___23972 = (0);
while(true){
if((i__4737__auto___23972 < len__4736__auto___23971)){
args__4742__auto__.push((arguments[i__4737__auto___23972]));

var G__23973 = (i__4737__auto___23972 + (1));
i__4737__auto___23972 = G__23973;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_snake_case_string.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_snake_case_string.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_snake_case_string.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_snake_case_string.cljs$lang$applyTo = (function (seq23877){
var G__23878 = cljs.core.first(seq23877);
var seq23877__$1 = cljs.core.next(seq23877);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23878,seq23877__$1);
}));


camel_snake_kebab.core.__GT_snake_case_symbol = (function camel_snake_kebab$core$__GT_snake_case_symbol(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23977 = arguments.length;
var i__4737__auto___23978 = (0);
while(true){
if((i__4737__auto___23978 < len__4736__auto___23977)){
args__4742__auto__.push((arguments[i__4737__auto___23978]));

var G__23979 = (i__4737__auto___23978 + (1));
i__4737__auto___23978 = G__23979;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_snake_case_symbol.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_snake_case_symbol.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_snake_case_symbol.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_snake_case_symbol.cljs$lang$applyTo = (function (seq23879){
var G__23880 = cljs.core.first(seq23879);
var seq23879__$1 = cljs.core.next(seq23879);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23880,seq23879__$1);
}));


camel_snake_kebab.core.__GT_snake_case_keyword = (function camel_snake_kebab$core$__GT_snake_case_keyword(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23986 = arguments.length;
var i__4737__auto___23987 = (0);
while(true){
if((i__4737__auto___23987 < len__4736__auto___23986)){
args__4742__auto__.push((arguments[i__4737__auto___23987]));

var G__23988 = (i__4737__auto___23987 + (1));
i__4737__auto___23987 = G__23988;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_snake_case_keyword.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_snake_case_keyword.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"_",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_snake_case_keyword.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_snake_case_keyword.cljs$lang$applyTo = (function (seq23881){
var G__23882 = cljs.core.first(seq23881);
var seq23881__$1 = cljs.core.next(seq23881);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23882,seq23881__$1);
}));

camel_snake_kebab.core.__GT_kebab_case = (function camel_snake_kebab$core$__GT_kebab_case(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23989 = arguments.length;
var i__4737__auto___23990 = (0);
while(true){
if((i__4737__auto___23990 < len__4736__auto___23989)){
args__4742__auto__.push((arguments[i__4737__auto___23990]));

var G__23991 = (i__4737__auto___23990 + (1));
i__4737__auto___23990 = G__23991;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_kebab_case.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_kebab_case.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"-",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_kebab_case.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_kebab_case.cljs$lang$applyTo = (function (seq23883){
var G__23884 = cljs.core.first(seq23883);
var seq23883__$1 = cljs.core.next(seq23883);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23884,seq23883__$1);
}));


camel_snake_kebab.core.__GT_kebab_case_string = (function camel_snake_kebab$core$__GT_kebab_case_string(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23994 = arguments.length;
var i__4737__auto___23995 = (0);
while(true){
if((i__4737__auto___23995 < len__4736__auto___23994)){
args__4742__auto__.push((arguments[i__4737__auto___23995]));

var G__23996 = (i__4737__auto___23995 + (1));
i__4737__auto___23995 = G__23996;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_kebab_case_string.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_kebab_case_string.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"-",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_kebab_case_string.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_kebab_case_string.cljs$lang$applyTo = (function (seq23885){
var G__23886 = cljs.core.first(seq23885);
var seq23885__$1 = cljs.core.next(seq23885);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23886,seq23885__$1);
}));


camel_snake_kebab.core.__GT_kebab_case_symbol = (function camel_snake_kebab$core$__GT_kebab_case_symbol(var_args){
var args__4742__auto__ = [];
var len__4736__auto___23997 = arguments.length;
var i__4737__auto___23998 = (0);
while(true){
if((i__4737__auto___23998 < len__4736__auto___23997)){
args__4742__auto__.push((arguments[i__4737__auto___23998]));

var G__23999 = (i__4737__auto___23998 + (1));
i__4737__auto___23998 = G__23999;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_kebab_case_symbol.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_kebab_case_symbol.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"-",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_kebab_case_symbol.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_kebab_case_symbol.cljs$lang$applyTo = (function (seq23887){
var G__23888 = cljs.core.first(seq23887);
var seq23887__$1 = cljs.core.next(seq23887);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23888,seq23887__$1);
}));


camel_snake_kebab.core.__GT_kebab_case_keyword = (function camel_snake_kebab$core$__GT_kebab_case_keyword(var_args){
var args__4742__auto__ = [];
var len__4736__auto___24000 = arguments.length;
var i__4737__auto___24001 = (0);
while(true){
if((i__4737__auto___24001 < len__4736__auto___24000)){
args__4742__auto__.push((arguments[i__4737__auto___24001]));

var G__24002 = (i__4737__auto___24001 + (1));
i__4737__auto___24001 = G__24002;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_kebab_case_keyword.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_kebab_case_keyword.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,clojure.string.lower_case,clojure.string.lower_case,"-",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_kebab_case_keyword.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_kebab_case_keyword.cljs$lang$applyTo = (function (seq23889){
var G__23890 = cljs.core.first(seq23889);
var seq23889__$1 = cljs.core.next(seq23889);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23890,seq23889__$1);
}));

camel_snake_kebab.core.__GT_HTTP_Header_Case = (function camel_snake_kebab$core$__GT_HTTP_Header_Case(var_args){
var args__4742__auto__ = [];
var len__4736__auto___24003 = arguments.length;
var i__4737__auto___24004 = (0);
while(true){
if((i__4737__auto___24004 < len__4736__auto___24003)){
args__4742__auto__.push((arguments[i__4737__auto___24004]));

var G__24005 = (i__4737__auto___24004 + (1));
i__4737__auto___24004 = G__24005;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_HTTP_Header_Case.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_HTTP_Header_Case.cljs$core$IFn$_invoke$arity$variadic = (function (s__23803__auto__,rest__23804__auto__){
var convert_case__23805__auto__ = (function (p1__23802__23806__auto__){
return cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,camel_snake_kebab.internals.misc.capitalize_http_header,camel_snake_kebab.internals.misc.capitalize_http_header,"-",p1__23802__23806__auto__,cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23804__auto__], 0));
});
return camel_snake_kebab.internals.alter_name.alter_name(s__23803__auto__,convert_case__23805__auto__);
}));

(camel_snake_kebab.core.__GT_HTTP_Header_Case.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_HTTP_Header_Case.cljs$lang$applyTo = (function (seq23891){
var G__23892 = cljs.core.first(seq23891);
var seq23891__$1 = cljs.core.next(seq23891);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23892,seq23891__$1);
}));


camel_snake_kebab.core.__GT_HTTP_Header_Case_String = (function camel_snake_kebab$core$__GT_HTTP_Header_Case_String(var_args){
var args__4742__auto__ = [];
var len__4736__auto___24006 = arguments.length;
var i__4737__auto___24007 = (0);
while(true){
if((i__4737__auto___24007 < len__4736__auto___24006)){
args__4742__auto__.push((arguments[i__4737__auto___24007]));

var G__24008 = (i__4737__auto___24007 + (1));
i__4737__auto___24007 = G__24008;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_HTTP_Header_Case_String.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_HTTP_Header_Case_String.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.identity(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,camel_snake_kebab.internals.misc.capitalize_http_header,camel_snake_kebab.internals.misc.capitalize_http_header,"-",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_HTTP_Header_Case_String.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_HTTP_Header_Case_String.cljs$lang$applyTo = (function (seq23893){
var G__23894 = cljs.core.first(seq23893);
var seq23893__$1 = cljs.core.next(seq23893);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23894,seq23893__$1);
}));


camel_snake_kebab.core.__GT_HTTP_Header_Case_Symbol = (function camel_snake_kebab$core$__GT_HTTP_Header_Case_Symbol(var_args){
var args__4742__auto__ = [];
var len__4736__auto___24009 = arguments.length;
var i__4737__auto___24010 = (0);
while(true){
if((i__4737__auto___24010 < len__4736__auto___24009)){
args__4742__auto__.push((arguments[i__4737__auto___24010]));

var G__24011 = (i__4737__auto___24010 + (1));
i__4737__auto___24010 = G__24011;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_HTTP_Header_Case_Symbol.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_HTTP_Header_Case_Symbol.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.symbol.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,camel_snake_kebab.internals.misc.capitalize_http_header,camel_snake_kebab.internals.misc.capitalize_http_header,"-",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_HTTP_Header_Case_Symbol.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_HTTP_Header_Case_Symbol.cljs$lang$applyTo = (function (seq23895){
var G__23896 = cljs.core.first(seq23895);
var seq23895__$1 = cljs.core.next(seq23895);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23896,seq23895__$1);
}));


camel_snake_kebab.core.__GT_HTTP_Header_Case_Keyword = (function camel_snake_kebab$core$__GT_HTTP_Header_Case_Keyword(var_args){
var args__4742__auto__ = [];
var len__4736__auto___24012 = arguments.length;
var i__4737__auto___24017 = (0);
while(true){
if((i__4737__auto___24017 < len__4736__auto___24012)){
args__4742__auto__.push((arguments[i__4737__auto___24017]));

var G__24024 = (i__4737__auto___24017 + (1));
i__4737__auto___24017 = G__24024;
continue;
} else {
}
break;
}

var argseq__4743__auto__ = ((((1) < args__4742__auto__.length))?(new cljs.core.IndexedSeq(args__4742__auto__.slice((1)),(0),null)):null);
return camel_snake_kebab.core.__GT_HTTP_Header_Case_Keyword.cljs$core$IFn$_invoke$arity$variadic((arguments[(0)]),argseq__4743__auto__);
});

(camel_snake_kebab.core.__GT_HTTP_Header_Case_Keyword.cljs$core$IFn$_invoke$arity$variadic = (function (s__23808__auto__,rest__23809__auto__){
return cljs.core.keyword.cljs$core$IFn$_invoke$arity$1(cljs.core.apply.cljs$core$IFn$_invoke$arity$variadic(camel_snake_kebab.internals.misc.convert_case,camel_snake_kebab.internals.misc.capitalize_http_header,camel_snake_kebab.internals.misc.capitalize_http_header,"-",cljs.core.name(s__23808__auto__),cljs.core.prim_seq.cljs$core$IFn$_invoke$arity$2([rest__23809__auto__], 0)));
}));

(camel_snake_kebab.core.__GT_HTTP_Header_Case_Keyword.cljs$lang$maxFixedArity = (1));

/** @this {Function} */
(camel_snake_kebab.core.__GT_HTTP_Header_Case_Keyword.cljs$lang$applyTo = (function (seq23897){
var G__23898 = cljs.core.first(seq23897);
var seq23897__$1 = cljs.core.next(seq23897);
var self__4723__auto__ = this;
return self__4723__auto__.cljs$core$IFn$_invoke$arity$variadic(G__23898,seq23897__$1);
}));


//# sourceMappingURL=camel_snake_kebab.core.js.map
