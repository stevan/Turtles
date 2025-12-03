

// -----------------------------------------------------------------------------
// Terms
// -----------------------------------------------------------------------------

type Term = { kind : 'NIL' }
          | { kind : 'TRUE' }
          | { kind : 'FALSE' }
          | { kind : 'SYM', value : string }
          | { kind : 'STR', value : string }
          | { kind : 'NUM', value : number }
          | { kind : 'PAIR',    fst : Term, snd : Term }
          | { kind : 'LAMBDA',  param : Term, body : Term }
          | { kind : 'NATIVE',  body : (arg : Term) => Term }
          | { kind : 'FEXPR',   body : (arg : Term, env : Term) => Term }
          | { kind : 'CLOSURE', abs : Term, env : Term }

function Nil   () : Term { return { kind : 'NIL'   } }
function True  () : Term { return { kind : 'TRUE'  } }
function False () : Term { return { kind : 'FALSE' } }

function Sym (value : string) : Term {
    return { kind : 'SYM', value }
}

function Str (value : string) : Term {
    return { kind : 'STR', value }
}

function Num (value : number) : Term {
    return { kind : 'NUM', value }
}

function Pair (fst : Term, snd : Term) : Term {
    return { kind : 'PAIR', fst, snd }
}

function Lambda (param : Term, body : Term) : Term {
    return { kind : 'LAMBDA', param, body }
}

function Closure (abs : Term, env : Term) : Term {
    return { kind : 'CLOSURE', abs, env }
}

function Native (body : (arg : Term) => Term) : Term {
    return { kind : 'NATIVE', body }
}

function FExpr (body : (arg : Term, env : Term) => Term) : Term {
    return { kind : 'FEXPR', body }
}

function Env (sym : Term, val : Term, env : Term = Nil()) : Term {
    return Pair(Pair(sym, val), env);
}

// -----------------------------------------------------------------------------
// Core API
// -----------------------------------------------------------------------------

function __eq (lhs : Term, rhs : Term) : boolean {
    if (lhs.kind == rhs.kind) {
        switch (lhs.kind) {
        case 'NIL'   :
        case 'TRUE'  :
        case 'FALSE' : return true;
        case 'SYM'   :
        case 'STR'   :
        case 'NUM'   :
            if (lhs.kind != rhs.kind) return false;
            return lhs.value == rhs.value;
        case 'PAIR'  :
            if (lhs.kind != rhs.kind) return false;
            return __eq( lhs.fst, rhs.fst )
                && __eq( lhs.snd, rhs.snd );
        case 'LAMBDA':
            if (lhs.kind != rhs.kind) return false;
            return __eq( lhs.param, rhs.param )
                && __eq( lhs.body, rhs.body );
        case 'CLOSURE':
            if (lhs.kind != rhs.kind) return false;
            return __eq( lhs.env, rhs.env )
                && __eq( lhs.abs, rhs.abs );
        case 'NATIVE':
        case 'FEXPR' :
            if (lhs.kind != rhs.kind) return false;
            // dunno, best guess really, let JS sort it out
            return lhs.body === rhs.body;
        default:
            throw new Error('Cannot eq a non-Term');
        }
    }
    return false;
}

function __isPair (t : Term) : boolean {
    return t.kind == 'PAIR'
}

function __isNil (t : Term) : boolean {
    return t.kind == 'NIL'
}

function __isList (t : Term) : boolean {
    return t.kind == 'NIL' || (t.kind == 'PAIR' && __isList(t.snd))
}

function __makeList (...args : Term[]) : Term {
    let list : Term = Nil();
    while (args.length > 0) {
        list = Pair( args.pop() as Term, list );
    }
    return list;
}

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

type ParseExpr = Term | ParseExpr[];

function __parse (source : string) : Term {

    const SPLITTER = /'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g

    const tokenize = (src : string) : string[] => src.match(SPLITTER) ?? [];

    const parseTokens = (tokens : string[]) : [ ParseExpr, string[] ] => {
        let token = tokens[0];
        if (token == undefined) throw new Error('Undefined Token');
        let rest = tokens.slice(1);
        if (token == '(') return parseList( rest, [] );
        switch (true) {
        case token == 'true'       : return [ True(),             rest ];
        case token == 'false'      : return [ False(),            rest ];
        case !isNaN(Number(token)) : return [ Num(Number(token)), rest ];
        case token.startsWith('"') : return [ Str(token),         rest ];
        default                    : return [ Sym(token),         rest ];
        }
    }

    const parseList = (tokens : string[], acc : ParseExpr[]) : [ ParseExpr[], string[] ] => {
        if (tokens[0] === ')') return [ acc, tokens.slice(1) ];
        let [ expr, remaining ] = parseTokens( tokens );
        return parseList( remaining, [ ...acc, expr ] );
    }

    const buildTree = (expr : ParseExpr) : Term  =>
        (Array.isArray(expr)) ? __makeList( ...expr.map(buildTree) ) : expr;

    let [ expr, rest ] = parseTokens( tokenize( source ) );
    return buildTree( expr );
}

function __deparse (t : Term) : string {
    switch (t.kind) {
    case 'NIL'     : return '()';
    case 'TRUE'    : return 'true';
    case 'FALSE'   : return 'false';
    case 'SYM'     : return '`'+t.value;
    case 'STR'     : return `"${t.value}"`;
    case 'NUM'     : return t.value.toString();
    case 'PAIR'    :
        if (__isList(t)) {
            if (__isNil(t.snd)) {
                return `(${__deparse(t.fst)})`;
            } else {
                return `(${__deparse(t.fst)}::${__deparse(t.snd)})`;
            }
        } else {
            return `(${__deparse(t.fst)} * ${__deparse(t.snd)})`;
        }
    case 'LAMBDA'  : return `(λ ${__deparse(t.param)} . ${__deparse(t.body)})`
    case 'CLOSURE' : return `[ (${__deparse(t.env)}) ~ ${__deparse(t.abs)} ]`
    case 'NATIVE'  : return `&:native`
    case 'FEXPR'   : return `@:fexpr`
    default:
        throw new Error('Cannot show a non-Term');
    }
}

// -----------------------------------------------------------------------------
// Runtime
// -----------------------------------------------------------------------------

function __lookup (t : Term, env : Term) : Term {
    console.log(` /lookup/ ? ${__deparse(t)} IN ${__deparse(env)}`);
    if (t.kind   != 'SYM')  throw new Error(`lookup(t, env) t must be a symbol not ${t.kind}`);
    if (env.kind == 'NIL')  throw new Error(`Cannot find ${__deparse(t)} in Env`);
    if (env.kind != 'PAIR') throw new Error(`lookup(t, env) env must be a pair not ${env.kind}`);
    let curr = env.fst;
    if (curr.kind != 'PAIR') throw new Error(`Expected pair in Env not ${curr.kind}`);
    console.log(` /lookup/ ? ${__deparse(t)} EQ ${__deparse(curr.fst)}`);
    if (__eq(t, curr.fst)) {
        console.log(` !lookup! @ ${__deparse(t)} => ${__deparse(curr.snd)}`);
        return curr.snd;
    }
    return __lookup( t, env.snd );
}

function __define (sym : Term, t : Term, env : Term) : Term {
    if (sym.kind != 'SYM')  throw new Error(`define(sym, t, env) sym must be a symbol not ${t.kind}`);
    //if (env.kind != 'PAIR') throw new Error(`define(sym, t, env) env must be a pair not ${env.kind}`);
    return Env( sym, t, env );
}

function __eval (t : Term, env : Term) : Term {
    console.log(`EVAL:${t.kind} ${__deparse(t)}`);
    console.log('   %:ENV => ', __deparse(env));
    switch (t.kind) {
    case 'NIL'     :
    case 'TRUE'    :
    case 'FALSE'   :
    case 'NUM'     :
    case 'STR'     :
    case 'NATIVE'  :
    case 'FEXPR'   : return t;
    case 'SYM'     : return __lookup( t, env );
    case 'CLOSURE' :
    case 'LAMBDA'  : return Closure( t, env );
    case 'PAIR'    :
        let head = __eval( t.fst, env );
        let tail = t.snd;
        // FEXPRs
        if (head.kind == 'FEXPR') {
            return __eval( head.body( tail, env ), env );
        }

        let call  : Term = head;
        let arg   : Term = __eval( tail, env );
        let local : Term = env;

        // unpack closures
        if (call.kind == 'CLOSURE') {
            console.log(` ^unpack ~ ${__deparse(call)}`);
            local = call.env;
            call  = call.abs;
        }

        // run what we got ...
        switch (call.kind) {
        case 'NATIVE':
            return call.body( arg );
        case 'LAMBDA':
            let param = call.param;
            if (param.kind != 'SYM') throw new Error(`Expected SYM for lambda param, but got ${param.kind}`);
            if (arg.kind   != 'PAIR') throw new Error(`Expected PAIR for lambda arg, but got ${arg.kind}`);
            return __eval( call.body, Env( param, arg.fst, local ) );
        default:
            return Pair( call, arg );
        }
    default:
        throw new Error('Cannot eval a non-Term');
    }
}

function __initEnv () : Term {
    let env : Term = Nil();

    env = __define(
        Sym('+'), Native((arg : Term) : Term => {
            console.log(`   >:CALL &:(+) w/ ${__deparse(arg)}`);
            if (arg.kind != 'PAIR') throw new Error(`Expected Pair in &:(+) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (rhs.kind != 'PAIR') throw new Error(`Expected rhs to be Pair in &:(+) not(${rhs.kind})`);
            rhs = rhs.fst;
            if (lhs.kind != 'NUM') throw new Error(`Expected lhs to be Num in &:(+) not(${lhs.kind})`);
            if (rhs.kind != 'NUM') throw new Error(`Expected rhs to be Num in &:(+) not(${rhs.kind})`);
            return Num( lhs.value + rhs.value );
        }),
        env
    );

    env = __define(
        Sym('lambda'), FExpr((arg : Term, env : Term) : Term => {
            console.log(`   >:CALL @:(lambda) w/ ${__deparse(arg)}`);
            if (arg.kind != 'PAIR') throw new Error(`Expected Pair as arg in @:(lambda) not(${arg.kind})`);
            let param = arg.fst;
            if (param.kind != 'PAIR') throw new Error(`Expected Pair as param in @:(lambda) not(${param.kind})`);
            let body  = arg.snd;
            if (body.kind != 'PAIR') throw new Error(`Expected Pair as body in @:(lambda) not(${body.kind})`);
            return Lambda( param.fst, body.fst );
        }),
        env
    );

    return env;
}

// -----------------------------------------------------------------------------

let env = __initEnv();

console.log(__deparse(__eval(__parse(`

    (((lambda (x) (lambda (y) (+ x y))) 10) 20)

`), env)));












