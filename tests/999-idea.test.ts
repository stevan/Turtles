
// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Bool = { type : 'BOOL', value : boolean }
type Num  = { type : 'NUM',  value : number  }
type Str  = { type : 'STR',  value : string  }
type Sym  = { type : 'SYM',  ident : string  }

type Atom = Bool | Num | Str | Sym

type Pair = { type : 'PAIR', first : Value, second : Value }

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Value, tail : List }

type List = Cons | Nil

type Value = Atom | Pair | List

// -----------------------------------------------------------------------------
// Core Runtime
// -----------------------------------------------------------------------------

type ParseExpr = Value | ParseExpr[];

const SPLITTER = /'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g

const $Parser = {

    tokenize : (source : string) : List =>
        $.List.create(...((source.match(SPLITTER) ?? []).map((t) => $.str(t)))),

    parse : (source : string) : Value => {
        const fastTokenize = (src : string) : string[] => src.match(SPLITTER) ?? []

        const parseTokens = (tokens : string[]) : [ ParseExpr, string[] ] => {
            let token = tokens[0];
            if (token == undefined) throw new Error('Undefined Token');
            let rest = tokens.slice(1);
            if (token == '(') return parseList( rest, [] );
            switch (true) {
            case token == 'true'       : return [ $.bool(true),         rest ];
            case token == 'false'      : return [ $.bool(false),        rest ];
            case !isNaN(Number(token)) : return [ $.num(Number(token)), rest ];
            case token.startsWith('"') : return [ $.str(token),         rest ];
            default                    : return [ $.sym(token),         rest ];
            }
        }

        const parseList = (tokens : string[], acc : ParseExpr[]) : [ ParseExpr[], string[] ] => {
            if (tokens[0] === ')') return [ acc, tokens.slice(1) ];
            let [ expr, remaining ] = parseTokens( tokens );
            return parseList( remaining, [ ...acc, expr ] );
        }

        const buildTree = (expr : ParseExpr) : Value  =>
            (Array.isArray(expr)) ? $.List.create( ...expr.map(buildTree) ) : expr;

        let [ expr, rest ] = parseTokens( fastTokenize( source ) );
        return buildTree( expr );
    }
}

const $ = {
    typeOf : (v : Value) : string => v.type,

    isTrue  : (v : Value) : v is Bool    => $.isBool(v) && v.value == true,
    isFalse : (v : Value) : v is Bool    => $.isBool(v) && v.value == false,
    isBool  : (v : Value) : v is Bool    => $.typeOf(v) == 'BOOL',
    isNum   : (v : Value) : v is Num     => $.typeOf(v) == 'NUM',
    isStr   : (v : Value) : v is Str     => $.typeOf(v) == 'STR',
    isSym   : (v : Value) : v is Sym     => $.typeOf(v) == 'SYM',
    isAtom  : (v : Value) : v is Atom    => $.isBool(v) || $.isNum(v) || $.isStr(v) || $.isSym(v),

    isPair  : (v : Value) : v is Pair => $.typeOf(v) == 'PAIR',
    isNil   : (v : Value) : v is Nil  => $.typeOf(v) == 'NIL',
    isCons  : (v : Value) : v is Cons => $.typeOf(v) == 'CONS',
    isList  : (v : Value) : v is List => $.isNil(v) || $.isCons(v),

    bool : (value : boolean) : Bool => { return { type : 'BOOL', value } },
    num  : (value : number)  : Num  => { return { type : 'NUM',  value } },
    str  : (value : string)  : Str  => { return { type : 'STR',  value } },
    sym  : (ident : string)  : Sym  => { return { type : 'SYM',  ident } },

    pair : (first : Value, second : Value) : Pair => {
        return { type : 'PAIR', first, second }
    },

    nil    : () : Nil => { return { type : 'NIL'} },
    cons   : (head : Value, tail : List = $.nil()) : Cons => {
        return { type : 'CONS', head, tail }
    },

    // Lists
    List : {
        create : (...args : Value[]) : List => {
            let list : List = $.nil();
            while (args.length > 0) {
                list = $.cons( args.pop() as Value, list );
            }
            return list;
        },

        head : (l : Cons) : Value => l.head,
        tail : (l : Cons) : List  => l.tail,

        flatten : (l : List) : Value[] => $.isNil(l) ? [] : [ l.head, ...$.List.flatten(l.tail) ],
        length  : (l : List) : number  => $.isNil(l) ? 0  : 1 + $.List.length(l.tail),

        reduce : (l : List, f : (i : Value, acc : List) => List, acc : List) : List =>
            $.isNil(l)
                ? acc
                : $.List.reduce( l.tail, f, f( l.head, acc ) ),

        map : (l : List, f : (i : Value) => Value) : List =>
            $.List.reduce(l, (i, acc) => $.cons(f(i), acc), $.nil()),

        grep : (l : List, f : (i : Value) => boolean) : List =>
            $.List.reduce(l, (i, acc) => f(i) ? acc : $.cons(i, acc), $.nil()),

        find : (l : List, f : (i : Value) => boolean) : Value =>
            $.isNil(l)
                ? $.nil()
                : f(l.head) ? l.head : $.List.find( l.tail, f ),
    },

    pprint : (v : Value) : string => {
        switch (true) {
        case $.isNil(v)   : return '()';
        case $.isTrue(v)  : return '#t';
        case $.isFalse(v) : return '#f';
        case $.isNum(v)   : return v.value.toString();
        case $.isStr(v)   : return `"${v.value}"`;
        case $.isSym(v)   : return v.ident;
        case $.isPair(v)  : return `(${$.pprint(v.first)} : ${$.pprint(v.second)})`;
        case $.isCons(v)  : return $.isNil(v.tail)
            ? `(${$.pprint(v.head)})`
            : `(${$.pprint(v.head)} ${$.List.flatten(v.tail).map((e) => $.pprint(e)).join(' ')})`;
        default: throw new Error(`Unknown value type (${JSON.stringify(v)})`);
        }
    }
}

// -----------------------------------------------------------------------------
// Interpeter AST
// -----------------------------------------------------------------------------

type Tag    = Sym
type Tagged = Cons

const Tags = {
    Env     : $.sym('`env'),
    Val     : $.sym('`val'),
    Var     : $.sym('`var'),
    Bind    : $.sym('`bind'),
    Apply   : $.sym('`apply'),
    Lambda  : $.sym('`lambda'),
    Closure : $.sym('`closure'),
}

const isTag = (t : Value) : t is Tag => $.isSym(t) && (t.ident == '`var'
                                                    || t.ident == '`val'
                                                    || t.ident == '`bind'
                                                    || t.ident == '`apply'
                                                    || t.ident == '`lambda'
                                                    || t.ident == '`closure');


const isTagged    = (v : Value)            : v is Tagged => $.isCons(v) && isTag(v.head);
const isNotTagged = (v : Value)            : v is Value  => !isTagged(v);
const hasTag      = (v : Value, tag : Tag) : v is Tagged => isTagged(v) && $.isSym(v.head) && v.head.ident == tag.ident;

const tagged = (tag : Tag, tail : List = $.nil()) : Cons => $.cons(tag, tail);

// ...

// -----------------------------------------------------------------------------

type Env     = Tagged
type Val     = Tagged
type Var     = Tagged
type Bind    = Tagged
type Apply   = Tagged
type Lambda  = Tagged
type Closure = Tagged

const $Env = {
    create : (bindings : List = $.nil()) : Env => tagged(Tags.Env, bindings),

    set : (env : Env, symbol : Sym, value : Value) : Env => {
        return $Env.create( $.cons( $.pair( symbol, value ), env.tail ) )
    },

    get : (env : Env, symbol : Sym) : Value => {
        return $.List.find( env.tail, (b) => {
            if (!($.isPair(b) && $.isSym(b.first))) throw new Error('Expected pair!');
            return b.first.ident == symbol.ident;
        })
    },
}

// -----------------------------------------------------------------------------

const isEnv     = (v : Value) : v is Env     => hasTag(v, Tags.Env);
const isVal     = (v : Value) : v is Val     => hasTag(v, Tags.Val);
const isVar     = (v : Value) : v is Var     => hasTag(v, Tags.Var);
const isBind    = (v : Value) : v is Bind    => hasTag(v, Tags.Bind);
const isApply   = (v : Value) : v is Apply   => hasTag(v, Tags.Apply);
const isLambda  = (v : Value) : v is Lambda  => hasTag(v, Tags.Lambda);
const isClosure = (v : Value) : v is Closure => hasTag(v, Tags.Closure);

const Val     = (value    : Value)               : Val     => tagged(Tags.Val,     $.cons(value));
const Var     = (symbol   : Sym)                 : Var     => tagged(Tags.Var,     $.cons(symbol));
const Bind    = (symbol   : Sym)                 : Bind    => tagged(Tags.Bind,    $.cons(symbol));
const Lambda  = (params   : List,   body : List) : Lambda  => tagged(Tags.Lambda,  $.cons(params, $.cons(body)));
const Apply   = (func     : Value,  args : List) : Apply   => tagged(Tags.Apply,   $.cons(func,   $.cons(args)));
const Closure = (lambda   : Tagged, env  : Env)  : Closure => tagged(Tags.Closure, $.cons(lambda, $.cons(env)));

const $Compiler = {
    compile : (e : Value) : List => {
        //console.log('COMPILE :', $.pprint(e));
        switch (true) {
        case $.isSym(e)  : return Var(e);
        case $.isAtom(e) : return Val(e);
        case $.isPair(e) : return Val(e);
        case $.isCons(e) :
            switch (true) {
            case $.isSym(e.head):
                if (e.head.ident == 'lambda') {
                    //console.log('LAMBDA  :', $.pprint(e));
                    let form = e.tail as Cons;
                    return Lambda(
                        $.List.map(form.head as List, (p) => Bind(p as Sym)),
                        $Compiler.compile( (form.tail as Cons).head ) as List
                    );
                }
            default:
                //console.log('CHECK???? :', $.pprint(e));
                let head = $Compiler.compile( e.head );
                //console.log('HEAD-is   :', $.pprint(head));
                if (isVar(head) || isLambda(head)) {
                    //console.log('APPLY   :', $.pprint(e));
                    return Apply(head, $.List.map(e.tail, (a) => $Compiler.compile(a)));
                } else {
                    //console.log('LIST   :', $.pprint(e));
                    let tail = e.tail;
                    if ($.isNil(tail)) {
                        return $.cons(head);
                    } else {
                        return $.cons(head, $Compiler.compile(tail))
                    }
                }
            }
        default :
            throw new Error(`HMMM ${JSON.stringify(e)}`)
        }
    },
}



let source = `
    ((lambda (x y) (+ x y)) 10 20)
`;

let parsed = $Parser.parse(source);
console.log('parsed :', $.pprint(parsed));

let compiled = $Compiler.compile(parsed);
console.log('compiled :', $.pprint(compiled));
//console.log('compiled :', JSON.stringify(compiled, null, 4));








// let tree = parse('(lambda (x y) (+ x y))');
// console.log('DEPARSE', $.pprint(tree));
// console.log('PARSED', JSON.stringify(tree, null, 4));
















/*

let env = $Env.create();
console.log($.pprint(env));

env = $Env.set( env, $.sym('x'), $.num(10) );
console.log($.pprint(env));
console.log('x = ', $.pprint($Env.get(env, $.sym('x'))));
console.log('y = ', $.pprint($Env.get(env, $.sym('y'))));
console.log('z = ', $.pprint($Env.get(env, $.sym('z'))));

env = $Env.set( env, $.sym('y'), $.num(20) );
console.log($.pprint(env));
console.log('x = ', $.pprint($Env.get(env, $.sym('x'))));
console.log('y = ', $.pprint($Env.get(env, $.sym('y'))));
console.log('z = ', $.pprint($Env.get(env, $.sym('z'))));

env = $Env.set( env, $.sym('z'), $.num(30) );
console.log($.pprint(env));
console.log('x = ', $.pprint($Env.get(env, $.sym('x'))));
console.log('y = ', $.pprint($Env.get(env, $.sym('y'))));
console.log('z = ', $.pprint($Env.get(env, $.sym('z'))));

*/








