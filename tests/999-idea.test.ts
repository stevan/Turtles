
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

const parse = (source : string) : Value => {

    const tokenize = (src : string) : string[] => src.match(/'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g) ?? []

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
        (Array.isArray(expr)) ? $.list( ...expr.map(buildTree) ) : expr;

    let [ expr, rest ] = parseTokens( tokenize( source ) );
    return buildTree( expr );
}

const $ = {

    typeOf : (v : Value) : string => v.type,

    isTrue  : (v : Value) : v is Bool => $.isBool(v) && v.value == true,
    isFalse : (v : Value) : v is Bool => $.isBool(v) && v.value == false,
    isBool  : (v : Value) : v is Bool => $.typeOf(v) == 'BOOL',
    isNum   : (v : Value) : v is Num  => $.typeOf(v) == 'NUM',
    isStr   : (v : Value) : v is Str  => $.typeOf(v) == 'STR',
    isSym   : (v : Value) : v is Sym  => $.typeOf(v) == 'SYM',
    isAtom  : (v : Value) : v is Atom => $.isBool(v) || $.isNum(v) || $.isStr(v) || $.isSym(v),

    isPair  : (v : Value) : v is Pair => $.typeOf(v) == 'PAIR',
    isNil   : (v : Value) : v is Nil  => $.typeOf(v) == 'NIL',
    isCons  : (v : Value) : v is Cons => $.typeOf(v) == 'CONS',
    isList  : (v : Value) : v is List => $.isNil(v) || $.isCons(v),

    bool : (value : boolean) : Bool => { return { type : 'BOOL', value } },
    num  : (value : number)  : Num  => { return { type : 'NUM',  value } },
    str  : (value : string)  : Str  => { return { type : 'STR',  value } },
    sym  : (ident : string)  : Sym  => { return { type : 'SYM',  ident } },

    nil    : () : Nil => { return { type : 'NIL'} },
    cons   : (head : Value, tail : List = $.nil()) : Cons => {
        return { type : 'CONS', head, tail }
    },

    list : (...args : Value[]) : List => {
        let list : List = $.nil();
        while (args.length > 0) {
            list = $.cons( args.pop() as Value, list );
        }
        return list;
    },

    head : (l : Cons) : Value => l.head,
    tail : (l : Cons) : List  => l.tail,

    flatten : (l : List) : Value[] => $.isNil(l) ? [] : [ l.head, ...$.flatten(l.tail) ],
    length  : (l : List) : number  => $.isNil(l) ? 0  : 1 + $.length(l.tail),

    reduce : (l : List, f : (i : Value, acc : List) => List, acc : List) : List =>
        $.isNil(l)
            ? acc
            : $.reduce( l.tail, f, f( l.head, acc ) ),

    map : (l : List, f : (i : Value) => Value) : List =>
        $.reduce(l, (i, acc) => $.cons(f(i), acc), $.nil()),


    grep : (l : List, f : (i : Value) => boolean) : List =>
        $.reduce(l, (i, acc) => f(i) ? acc : $.cons(i, acc), $.nil()),


    pprint : (v : Value) : string => {
        switch (true) {
        case $.isNil(v)   : return '()';
        case $.isTrue(v)  : return '#t';
        case $.isFalse(v) : return '#f';
        case $.isNum(v)   : return v.value.toString();
        case $.isStr(v)   : return `"${v.value}"`;
        case $.isSym(v)   : return v.ident;
        case $.isPair(v)  : return `(${$.pprint(v.first)} : ${$.pprint(v.second)})`;
        case $.isCons(v)  : return `(${$.flatten(v).map((i) => $.pprint(i)).join(' ')})`;
        default: throw new Error(`Unknown value type (${JSON.stringify(v)})`);
        }
    }
}


// -----------------------------------------------------------------------------
// Interpeter AST
// -----------------------------------------------------------------------------

type Tag    = Sym
type Tagged = Cons

const isTag = (t : Value) : t is Tag => $.isSym(t) && (t.ident == 'Var'
                                                   ||  t.ident == 'Apply'
                                                   ||  t.ident == 'Lambda'
                                                   ||  t.ident == 'Closure');



const isTagged    = (v : Value)            : v is Tagged => $.isCons(v) && isTag(v.head);
const isNotTagged = (v : Value)            : v is Value  => !isTagged(v);
const hasTag      = (v : Value, tag : Tag) : v is Tagged => isTagged(v) && $.isSym(v.head) && v.head.ident == tag.ident;

const tagged = (tag : Tag, tail : List = $.nil()) : Cons => $.cons(tag, tail);

const Tags = {
    Env     : $.sym('Env'),
    Var     : $.sym('Var'),
    Apply   : $.sym('Apply'),
    Lambda  : $.sym('Lambda'),
    Closure : $.sym('Closure'),
}

// ...

const isEnv     = (v : Value) : v is Cons => hasTag(v, Tags.Env);

const isVar     = (v : Value) : v is Cons => hasTag(v, Tags.Var);
const isApply   = (v : Value) : v is Cons => hasTag(v, Tags.Apply);

const isLambda  = (v : Value) : v is Cons => hasTag(v, Tags.Lambda);
const isClosure = (v : Value) : v is Cons => hasTag(v, Tags.Closure);

const isTerm  = (v : Value) : v is Cons  => isVar(v) || isApply(v);
const isValue = (v : Value) : v is Value => isLambda(v) || isClosure(v) || isNotTagged(v);

// -----------------------------------------------------------------------------

const Env     = () : Cons => tagged(Tags.Env);
const Var     = (symbol : Sym)                : Cons => tagged(Tags.Var,     $.cons(symbol));
const Apply   = (func   : Value, args : List) : Cons => tagged(Tags.Apply,   $.cons(func,   args));
const Lambda  = (params : List,  body : Cons) : Cons => tagged(Tags.Lambda,  $.cons(params, body));
const Closure = (lambda : Cons,  env  : List) : Cons => tagged(Tags.Closure, $.cons(lambda,  env));


let tree = parse('(lambda (x y) (+ x y))');


console.log('DEPARSE', $.pprint(tree));

























