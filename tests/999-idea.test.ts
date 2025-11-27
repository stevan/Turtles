
// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

type NativeFunc = (env : Cons) => Value;

type Native = { type : 'NATIVE', params : List, body : NativeFunc }
type Lambda = { type : 'LAMBDA', params : List, body : List }

type Bool = { type : 'BOOL', value : boolean }
type Num  = { type : 'NUM',  value : number  }
type Str  = { type : 'STR',  value : string  }
type Sym  = { type : 'SYM',  ident : string  }

type Atom = Bool | Num | Str | Native | Lambda

type Pair = { type : 'PAIR', first : Value, second : Value }

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Value, tail : List }

type List = Cons | Nil

type Value = Atom | Sym | Pair | List

// -----------------------------------------------------------------------------
// Core Runtime
// -----------------------------------------------------------------------------

const $ = {
    typeOf : (v : Value) : string => v.type,

    isTrue   : (v : Value) : v is Bool    => $.isBool(v) && v.value == true,
    isFalse  : (v : Value) : v is Bool    => $.isBool(v) && v.value == false,
    isBool   : (v : Value) : v is Bool    => $.typeOf(v) == 'BOOL',
    isNum    : (v : Value) : v is Num     => $.typeOf(v) == 'NUM',
    isStr    : (v : Value) : v is Str     => $.typeOf(v) == 'STR',
    isNative : (v : Value) : v is Native  => $.typeOf(v) == 'NATIVE',
    isLambda : (v : Value) : v is Lambda  => $.typeOf(v) == 'LAMBDA',
    isAtom   : (v : Value) : v is Atom    => $.isBool(v) || $.isNum(v) || $.isStr(v) || $.isNative(v) || $.isLambda(v),

    isSym   : (v : Value) : v is Sym  => $.typeOf(v) == 'SYM',
    isPair  : (v : Value) : v is Pair => $.typeOf(v) == 'PAIR',
    isNil   : (v : Value) : v is Nil  => $.typeOf(v) == 'NIL',
    isCons  : (v : Value) : v is Cons => $.typeOf(v) == 'CONS',
    isList  : (v : Value) : v is List => $.isNil(v) || $.isCons(v),

    pair : (first : Value, second : Value) : Pair => {
        return { type : 'PAIR', first, second }
    },

    nil    : () : Nil => { return { type : 'NIL'} },
    cons   : (head : Value, tail : List = $.nil()) : Cons => {
        return { type : 'CONS', head, tail }
    },

    // expect native args ...

    bool : (value : boolean) : Bool => { return { type : 'BOOL', value } },
    num  : (value : number)  : Num  => { return { type : 'NUM',  value } },
    str  : (value : string)  : Str  => { return { type : 'STR',  value } },
    sym  : (ident : string)  : Sym  => { return { type : 'SYM',  ident } },

    // builtins
    native : (params : List, body : NativeFunc) : Native => {
        return { type : 'NATIVE', params, body }
    },

    lambda : (params : List, body : List) : Lambda => {
        return { type : 'LAMBDA', params, body }
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
        case $.isLambda(v): return `(lambda ${
            $List.flatten(v.params).map((e) => $.pprint(e)).join(' ')
        } ${
            $List.flatten(v.body).map((e) => $.pprint(e)).join(' ')
        })`;
        case $.isCons(v)  : return $.isNil(v.tail)
            ? `(${$.pprint(v.head)})`
            : `(${$.pprint(v.head)} ${
                    $List.flatten(v.tail).map((e) => $.pprint(e)).join(' ')
            })`;
        default: throw new Error(`Unknown value type (${JSON.stringify(v)})`);
        }
    }
}

// parser ...

type ParseExpr = Value | ParseExpr[];

const SPLITTER = /'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g

const $Parser = {

    tokenize : (source : string) : List =>
        $List.create(...((source.match(SPLITTER) ?? []).map((t) => $.str(t)))),

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
            (Array.isArray(expr)) ? $List.create( ...expr.map(buildTree) ) : expr;

        let [ expr, rest ] = parseTokens( fastTokenize( source ) );
        return buildTree( expr );
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

const Val     = (value    : Value)  : Cons => $.cons($.sym(Tags.Val),     $.cons(value));
const Var     = (symbol   : Sym)    : Cons => $.cons($.sym(Tags.Var),     $.cons(symbol));
const Bind    = (symbol   : Sym)    : Cons => $.cons($.sym(Tags.Bind),    $.cons(symbol));
const Lambda  = (func     : Lambda) : Cons => $.cons($.sym(Tags.Lambda),  $.cons(func));
const Apply   = (call     : List)   : Cons => $.cons($.sym(Tags.Apply),   $.cons(call));
const Closure = (capture  : List)   : Cons => $.cons($.sym(Tags.Closure), $.cons(capture));

    // Lists
const $List = {
    create : (...args : Value[]) : List => {
        let list : List = $.nil();
        while (args.length > 0) {
            list = $.cons( args.pop() as Value, list );
        }
        return list;
    },

    head : (l : Value) : Value => {
        if (!$.isCons(l)) throw new Error(`Can only call (head) on Cons not ${$.pprint(l)}`);
        return l.head;
    },

    tail : (l : Value) : List  => {
        if (!$.isCons(l)) throw new Error(`Can only call (tail) on Cons not ${$.pprint(l)}`);
        return l.tail;
    },

    flatten : (l : List) : Value[] => $.isNil(l) ? [] : [ l.head, ...$List.flatten(l.tail) ],
    length  : (l : List) : number  => $.isNil(l) ? 0  : 1 + $List.length(l.tail),

    reduce : (l : List, f : (i : Value, acc : List) => List, acc : List) : List =>
        $.isNil(l)
            ? acc
            : $List.reduce( l.tail, f, f( l.head, acc ) ),

    map : (l : List, f : (i : Value) => Value) : List =>
        $List.reduce(l, (i, acc) => $.cons(f(i), acc), $.nil()),

    grep : (l : List, f : (i : Value) => boolean) : List =>
        $List.reduce(l, (i, acc) => f(i) ? acc : $.cons(i, acc), $.nil()),

    find : (l : List, f : (i : Value) => boolean) : Value =>
        $.isNil(l)
            ? $.nil()
            : f(l.head) ? l.head : $List.find( l.tail, f ),
};

const $Env = {
    create : (bindings : List = $.nil()) : Cons => $.cons($.sym(Tags.Env), bindings),

    set : (env : Cons, symbol : Sym, value : Value) : Cons => {
        return $Env.create( $.cons( $.pair( symbol, value ), env.tail ) )
    },

    get : (env : Cons, symbol : Sym) : Value => {
        let bind = $List.find( env.tail, (b) => {
            if (!($.isPair(b) && $.isSym(b.first))) throw new Error('Expected pair!');
            return b.first.ident == symbol.ident;
        });

        if ($.isPair(bind)) return bind.second;
        return bind;
    },
}

const $Compiler = {
    compile : (e : Value) : Value => {
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
                    let form = $List.tail( e );
                    return Lambda( $.lambda(
                        $.cons( $List.head( form ) ),
                        // compile the lambda body
                        $.cons(
                            $Compiler.compile(
                                $List.head($List.tail(form))
                            )
                        )
                    ) );
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
                            $List.map(e.tail, (a) => $Compiler.compile(a))
                        )
                    );
                } else {
                    //console.log('LIST   :', $.pprint(e));
                    let tail = e.tail;
                    if ($.isNil(tail)) {
                        return $.cons(head);
                    } else {
                        return $.cons(head, $Compiler.compile(tail) as List)
                    }
                }
            }
        default :
            throw new Error(`HMMM ${JSON.stringify(e)}`)
        }
    },
}

const $Interpreter = {

    exec : (expr : Value, env : Cons) : Value => {
        console.log(`EXEC  | ${$.pprint(expr)}`);
        switch (true) {
        case $.isCons(expr):
            let tag = expr.head;
            if ($.isSym(tag) && isTag(tag)) {
                let body = $List.head(expr.tail);
                console.log(`  TAG + ${$.pprint(tag)} -> ${$.pprint(body)}`);
                switch (tag.ident) {
                case Tags.Val     : return body;
                case Tags.Var     : return $Interpreter.evaluate( body, env );
                case Tags.Apply   :
                    return $Interpreter.apply(
                        $Interpreter.evaluate( body, env ),
                        env
                    );
                case Tags.Lambda  : return body;
                case Tags.Closure :
                    let abs   = $List.head(body);
                    let local = $List.head($List.tail(body));
                    switch (true) {
                    case $.isNative(abs):
                        return abs.body( local as Cons );
                    case $.isLambda(abs):
                        console.log(`<> EXEC LAMBDA ==> ${$.pprint($List.head(abs.body))} env: ${$.pprint(local)}`);
                        return $Interpreter.exec( $List.head(abs.body), local as Cons );
                    default:
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
            let head = $Interpreter.exec( expr.head, env );
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

    apply : (expr : Value, env : Cons) : Value => {
        console.log(`APPLY | ${$.pprint(expr)}`);
        let app    = $List.head(expr);
        let args   = $List.tail(expr);

        if (isClosure(app)) {
            console.log(`!!! GOT CLOSURE !!! ${$.pprint(app)} args: ${$.pprint(args)} env: ${$.pprint(env)}`);
            return $Interpreter.exec( Closure( $.cons( app, $.cons(env) ) ), env );
        }

        switch (true) {
        case ($.isNative(app) || $.isLambda(app)):
            let params = $List.flatten(app.params);
            let evaled = $List.flatten(args);

            if ($.isCons(params[0] as Value)) {
                params = $List.flatten( params[0] as List );
            }

            console.log('PARAMS!!!!!!!!!!!!!', params);

            let local = env;
            while (params.length > 0) {
                let p = params.shift() as Sym;
                let a = evaled.shift() as Value;
                console.log(`setting ${$.pprint(p)} to ${$.pprint(a)}`);
                local = $Env.set( local, p, a );
                console.log(`local is ${$.pprint(local)}`);
            }

            console.log(`!!! GOT ABS !!! ${$.pprint(app)} args: ${$.pprint(args)} env: ${$.pprint(local)}`);
            return $Interpreter.exec( Closure( $.cons( app, $.cons(local) ) ), env );
        default:
            throw new Error(`APPLY! ${$.pprint(expr)}`);
        }
    },
}

// -----------------------------------------------------------------------------

let source = `
    ((lambda (x y) (+ x y)) 10 20)
`;
console.log(`source :[\n${source}\n]`);

let parsed = $Parser.parse(source);
console.log('parsed      :', $.pprint(parsed));

let compiled = $Compiler.compile(parsed);
console.log('compiled    :', $.pprint(compiled));
//console.log('compiled :', JSON.stringify(compiled, null, 4));

let env = $Env.create();
env = $Env.set( env, $.sym('+'), $.native(
        $.cons( $.sym('lhs'), $.cons( $.sym('rhs') ) ),
        (env : Cons) : Value => {
            let lhs = $Env.get( env, $.sym('lhs') );
            let rhs = $Env.get( env, $.sym('rhs') );
            if (!$.isNum(lhs)) throw new Error('Expected lhs to be Num');
            if (!$.isNum(rhs)) throw new Error('Expected rhs to be Num');
            return $.num(lhs.value + rhs.value);
        }
    )
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








