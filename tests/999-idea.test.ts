
// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type Native = { type : 'NATIVE', body : (env : Cons) => Value }

type Bool = { type : 'BOOL', value : boolean }
type Num  = { type : 'NUM',  value : number  }
type Str  = { type : 'STR',  value : string  }
type Sym  = { type : 'SYM',  ident : string  }

type Atom = Bool | Num | Str | Native

type Pair = { type : 'PAIR', first : Value, second : Value }

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Value, tail : List }

type List = Cons | Nil

type Value = Atom | Sym | Pair | List

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

    isTrue   : (v : Value) : v is Bool    => $.isBool(v) && v.value == true,
    isFalse  : (v : Value) : v is Bool    => $.isBool(v) && v.value == false,
    isBool   : (v : Value) : v is Bool    => $.typeOf(v) == 'BOOL',
    isNum    : (v : Value) : v is Num     => $.typeOf(v) == 'NUM',
    isStr    : (v : Value) : v is Str     => $.typeOf(v) == 'STR',
    isNative : (v : Value) : v is Native  => $.typeOf(v) == 'NATIVE',
    isAtom   : (v : Value) : v is Atom    => $.isBool(v) || $.isNum(v) || $.isStr(v) || $.isNative(v),

    isSym   : (v : Value) : v is Sym  => $.typeOf(v) == 'SYM',
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
        case $.isNative(v): return `&:native`;
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

const Tags = {
    Env     : '`env',
    Val     : '`val',
    Var     : '`var',
    Bind    : '`bind',
    Apply   : '`apply',
    Lambda  : '`lambda',
    Closure : '`closure',
}

const isTag = (tag : Sym) : boolean => {
    return tag.ident == '`env'
        || tag.ident == '`val'
        || tag.ident == '`var'
        || tag.ident == '`bind'
        || tag.ident == '`apply'
        || tag.ident == '`lambda'
        || tag.ident == '`closure'
}

const hasTag = (v : Value, tag : string) : boolean => {
    return $.isCons(v) && $.isSym(v.head) && v.head.ident == tag;
}

const isEnv     = (v : Value) : v is Cons => hasTag(v, Tags.Env);
const isVal     = (v : Value) : v is Cons => hasTag(v, Tags.Val);
const isVar     = (v : Value) : v is Cons => hasTag(v, Tags.Var);
const isBind    = (v : Value) : v is Cons => hasTag(v, Tags.Bind);
const isApply   = (v : Value) : v is Cons => hasTag(v, Tags.Apply);
const isLambda  = (v : Value) : v is Cons => hasTag(v, Tags.Lambda);
const isClosure = (v : Value) : v is Cons => hasTag(v, Tags.Closure);

const Val     = (value    : Value) : Cons => $.cons($.sym(Tags.Val),     $.cons(value));
const Var     = (symbol   : Sym)   : Cons => $.cons($.sym(Tags.Var),     $.cons(symbol));
const Bind    = (symbol   : Sym)   : Cons => $.cons($.sym(Tags.Bind),    $.cons(symbol));
const Lambda  = (func     : List)  : Cons => $.cons($.sym(Tags.Lambda),  $.cons(func));
const Apply   = (call     : List)  : Cons => $.cons($.sym(Tags.Apply),   $.cons(call));
const Closure = (capture  : List)  : Cons => $.cons($.sym(Tags.Closure), $.cons(capture));

const $Env = {
    create : (bindings : List = $.nil()) : Cons => $.cons($.sym(Tags.Env), bindings),

    set : (env : Cons, symbol : Sym, value : Value) : Cons => {
        return $Env.create( $.cons( $.pair( symbol, value ), env.tail ) )
    },

    get : (env : Cons, symbol : Sym) : Value => {
        let bind = $.List.find( env.tail, (b) => {
            if (!($.isPair(b) && $.isSym(b.first))) throw new Error('Expected pair!');
            return b.first.ident == symbol.ident;
        });

        if ($.isPair(bind)) return bind.second;
        return bind;
    },
}

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
                        $.cons(
                            $.List.map(form.head as List, (p) => Bind(p as Sym)),
                            $.cons($Compiler.compile( (form.tail as Cons).head ) as List)
                        )
                    );
                }
            default:
                //console.log('CHECK???? :', $.pprint(e));
                let head = $Compiler.compile( e.head );
                //console.log('HEAD-is   :', $.pprint(head));
                if (isVar(head) || isLambda(head)) {
                    //console.log('APPLY   :', $.pprint(e));
                    return Apply(
                        $.cons(
                            head,
                            $.List.map(e.tail, (a) => $Compiler.compile(a))
                        )
                    );
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

/*

(`apply (
    (`lambda (
        ((`bind y) (`bind x))
        (`apply ( (`var +) (`var y) (`var x) ) )
    ))
    (`val 20)
    (`val 10)
))

*/

const $Interpreter = {

    exec : (expr : Value, env : Cons) : Value => {
        console.log(`EXEC  | ${$.pprint(expr)}`);
        switch (true) {
        case $.isCons(expr):
            let tag = expr.head;
            if ($.isSym(tag) && isTag(tag)) {
                let body = expr.tail;
                if (!$.isCons(body)) throw new Error(`Expected Body List for Tag:${$.pprint(tag)} got ${$.pprint(body)}`);
                let arg = body.head;
                console.log(`  TAG + ${$.pprint(tag)} -> ${$.pprint(arg)}`);
                switch (tag.ident) {
                case Tags.Apply   : return $Interpreter.apply( $Interpreter.evaluate( arg as Cons, env ) as Cons, env );
                case Tags.Val     : return arg;
                case Tags.Var     : return $Interpreter.evaluate( arg, env );
                case Tags.Closure :
                    let abs   = (arg as Cons).head;
                    let local = ((arg as Cons).tail as Cons).head;
                    if ($.isNative(abs)) {
                        return abs.body( local as Cons );
                    } else {
                        throw new Error(`CLOSURE TODO - ${$.pprint(expr)}`)
                    }
                default:
                    throw new Error(`TODO - ${$.pprint(expr)}`)
                }
            } else {
                console.log(`NOTAG - ${$.pprint(expr)}`);
                return $Interpreter.evaluate( expr, env );
            }
        default:
            throw new Error(`RUN! ${JSON.stringify(expr)}`);
        }
    },

    evaluate : (expr : Value, env : Cons) : Value => {
        console.log(`EVAL  | ${$.pprint(expr)}`);
        switch (true) {
        case $.isSym(expr)  : return $Env.get( env, expr );
        case $.isAtom(expr) : return expr;
        case $.isPair(expr) : return expr;
        case $.isNil(expr)  : return expr;
        case $.isCons(expr) :
            let head = $Interpreter.exec( expr.head, env ) as List;
            if ($.isNil(expr.tail)) {
                return $.cons( head );
            } else {
                return $.cons(
                    head,
                    $Interpreter.exec( expr.tail, env ) as List
                );
            }
        default:
            throw new Error(`EVAL! ${JSON.stringify(expr)}`);
        }
    },

    apply : (expr : Cons, env : Cons) : Value => {
        console.log(`APPLY | ${$.pprint(expr)}`);
        let app  = expr.head;
        let args = expr.tail;

        let local = env;
        if (!$.isNil(args)) {
            local = $Env.set( env, $.sym('@_'), args );
        }

        switch (true) {
        case $.isNative(app):
            return $Interpreter.exec( Closure( $.cons( app, $.cons(local) ) ), env );
        case isLambda(app):
            return $Interpreter.exec( Closure( $.cons( app, $.cons(local) ) ), env );
        case isClosure(app):
            return $Interpreter.exec( Closure( $.cons( app, $.cons(local) ) ), env );
        default:
            throw new Error(`APPLY! ${$.pprint(expr)}`);
        }
    },
}

// -----------------------------------------------------------------------------

let source = `
    (+ 10 20)
`;
console.log(`source :[\n${source}\n]`);

let parsed = $Parser.parse(source);
console.log('parsed      :', $.pprint(parsed));

let compiled = $Compiler.compile(parsed);
console.log('compiled    :', $.pprint(compiled));
//console.log('compiled :', JSON.stringify(compiled, null, 4));

let env = $Env.create();
env = $Env.set( env, $.sym('+'), {
        type : 'NATIVE',
        body : (env : Cons) : Value => {
            let [ lhs, rhs ] = $.List.flatten( $Env.get( env, $.sym('@_') ) as List );
            if (lhs == undefined || !$.isNum(lhs)) throw new Error('Expected lhs to be Num');
            if (rhs == undefined || !$.isNum(rhs)) throw new Error('Expected rhs to be Num');
            return $.num(lhs.value + rhs.value);
        }
    }
);
console.log('environment :', $.pprint(env));

console.group('... run');
let result = $Interpreter.exec( compiled, env );
console.groupEnd();

console.log('executed    : ', $.pprint(result));
//console.log('executed :', JSON.stringify(result, null, 4));










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








