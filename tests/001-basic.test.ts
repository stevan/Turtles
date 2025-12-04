

// -----------------------------------------------------------------------------
// Terms
// -----------------------------------------------------------------------------

type NativeFunc  = (arg : Term) => Term;
type NativeFExpr = (arg : Term, env : Term) => [ Term, Term ];

type NIL     = { kind : 'NIL' }
type TRUE    = { kind : 'TRUE' }
type FALSE   = { kind : 'FALSE' }
type SYM     = { kind : 'SYM', value : string }
type STR     = { kind : 'STR', value : string }
type NUM     = { kind : 'NUM', value : number }
type PAIR    = { kind : 'PAIR',    fst : Term, snd : Term }
type LAMBDA  = { kind : 'LAMBDA',  param : Term, body : Term }
type NATIVE  = { kind : 'NATIVE',  body : NativeFunc }
type FEXPR   = { kind : 'FEXPR',   body : NativeFExpr }
type CLOSURE = { kind : 'CLOSURE', abs : Term, env : Term }

type Term = NIL
          | TRUE
          | FALSE
          | SYM
          | STR
          | NUM
          | PAIR
          | LAMBDA
          | NATIVE
          | FEXPR
          | CLOSURE

function Nil   () : NIL   { return { kind : 'NIL'   } }
function True  () : TRUE  { return { kind : 'TRUE'  } }
function False () : FALSE { return { kind : 'FALSE' } }

function Sym (value : string) : SYM {
    return { kind : 'SYM', value }
}

function Str (value : string) : STR {
    return { kind : 'STR', value }
}

function Num (value : number) : NUM {
    return { kind : 'NUM', value }
}

function Pair (fst : Term, snd : Term) : PAIR {
    return { kind : 'PAIR', fst, snd }
}

function Lambda (param : Term, body : Term) : LAMBDA {
    return { kind : 'LAMBDA', param, body }
}

function Closure (abs : Term, env : Term) : CLOSURE {
    return { kind : 'CLOSURE', abs, env }
}

function Native (body : NativeFunc) : NATIVE {
    return { kind : 'NATIVE', body }
}

function FExpr (body : NativeFExpr) : FEXPR {
    return { kind : 'FEXPR', body }
}

function Env (sym : Term, val : Term, env : Term = Nil()) : Term {
    return Pair(Pair(sym, val), env);
}

// -----------------------------------------------------------------------------
// Core API
// -----------------------------------------------------------------------------

function __isNil   (t : Term) : t is NIL   { return t.kind == 'NIL'   }
function __isTrue  (t : Term) : t is TRUE  { return t.kind == 'TRUE'  }
function __isFalse (t : Term) : t is FALSE { return t.kind == 'FALSE' }

function __isNum (t : Term) : t is NUM { return t.kind == 'NUM' }
function __isStr (t : Term) : t is STR { return t.kind == 'STR' }
function __isSym (t : Term) : t is SYM { return t.kind == 'SYM' }

function __isPair (t : Term) : t is PAIR { return t.kind == 'PAIR' }

function __isLambda  (t : Term) : t is LAMBDA  { return t.kind == 'LAMBDA'  }
function __isNative  (t : Term) : t is NATIVE  { return t.kind == 'NATIVE'  }
function __isFExpr   (t : Term) : t is FEXPR   { return t.kind == 'FEXPR'   }
function __isClosure (t : Term) : t is CLOSURE { return t.kind == 'CLOSURE' }

function __isList (t : Term) : boolean {
    return __isNil(t) || (__isPair(t) && __isList(t.snd))
}

function __makeList (...args : Term[]) : Term {
    let list : Term = Nil();
    while (args.length > 0) {
        list = Pair( args.pop() as Term, list );
    }
    return list;
}

function __eq (lhs : Term, rhs : Term) : boolean {
    if (lhs.kind == rhs.kind) {
        switch (true) {
        case __isNil(lhs)   :
        case __isTrue(lhs)  :
        case __isFalse(lhs) : return true;
        case __isSym(lhs)   :
        case __isStr(lhs)   :
        case __isNum(lhs)   :
            if (lhs.kind != rhs.kind) return false;
            return lhs.value == rhs.value;
        case __isPair(lhs)  :
            if (lhs.kind != rhs.kind) return false;
            return __eq( lhs.fst, rhs.fst )
                && __eq( lhs.snd, rhs.snd );
        case __isLambda(lhs):
            if (lhs.kind != rhs.kind) return false;
            return __eq( lhs.param, rhs.param )
                && __eq( lhs.body, rhs.body );
        case __isClosure(lhs):
            if (lhs.kind != rhs.kind) return false;
            return __eq( lhs.env, rhs.env )
                && __eq( lhs.abs, rhs.abs );
        case __isNative(lhs):
        case __isFExpr(lhs) :
            if (lhs.kind != rhs.kind) return false;
            return lhs.body === rhs.body;
        default:
            throw new Error('Cannot eq a non-Term');
        }
    }
    return false;
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
    switch (true) {
    case __isNil(t)   : return '()';
    case __isTrue(t)  : return 'true';
    case __isFalse(t) : return 'false';
    case __isSym(t)   : return '`'+t.value;
    case __isStr(t)   : return `"${t.value}"`;
    case __isNum(t)   : return t.value.toString();
    case __isPair(t)  :
        if (__isList(t)) {
            if (__isNil(t.snd)) {
                return `(${__deparse(t.fst)})`;
            } else {
                return `(${__deparse(t.fst)}::${__deparse(t.snd)})`;
            }
        } else {
            return `(${__deparse(t.fst)} * ${__deparse(t.snd)})`;
        }
    case __isLambda(t)  : return `(λ ${__deparse(t.param)} . ${__deparse(t.body)})`
    case __isClosure(t) : return `[ (${__showLocalEnv(t.env)}) ~ ${__deparse(t.abs)} ]`
    case __isNative(t)  : return `&:native`
    case __isFExpr(t)   : return `@:fexpr`
    default:
        throw new Error('Cannot show a non-Term');
    }
}

// -----------------------------------------------------------------------------
// Environment
// -----------------------------------------------------------------------------

function __showEnv (env : Term) : string {
    if (__isNil(env))       return '';
    if (!__isPair(env))     throw new Error(`showEnv(env) env must be a pair not ${env.kind}`);
    if (!__isPair(env.fst)) throw new Error(`Expected pair in Env not ${env.fst.kind}`);
    let curr = env.fst;
    return `${__deparse(curr.fst)}:${curr.snd.kind.substr(0, 4).toLowerCase()} ${__showEnv( env.snd )}`
}

function __showLocalEnv (env : Term) : string {
    if (__isNil(env))       return '';
    if (!__isPair(env))     throw new Error(`showEnv(env) env must be a pair not ${env.kind}`);
    if (!__isPair(env.fst)) throw new Error(`Expected pair in Env not ${env.fst.kind}`);
    let curr = env.fst;
    if (__isNative(curr.snd) || __isFExpr(curr.snd)) return '~';
    return `${__deparse(curr.fst)}:${curr.snd.kind.substr(0, 4).toLowerCase()} ${__showEnv( env.snd )}`
}

function __lookup (t : Term, env : Term) : Term {
    if (__isNil(env))       throw new Error(`Cannot find ${__deparse(t)} in an empty Env`);
    if (!__isPair(env))     throw new Error(`lookup(t, env) env must be a pair not ${env.kind}`);
    if (!__isPair(env.fst)) throw new Error(`Expected pair in Env not ${env.fst.kind}`);
    if (!__isSym(t))        throw new Error(`lookup(t, env) t must be a symbol not ${t.kind}`);
    console.log(` /lookup/ ? ${__deparse(t)} IN ${__showEnv(env)}`);
    let curr = env.fst;
    console.log(` /lookup/ ? ${__deparse(t)} EQ ${__deparse(curr.fst)}`);
    if (__eq(t, curr.fst)) {
        console.log(` !lookup! @ ${__deparse(t)} => ${__deparse(curr.snd)}`);
        return curr.snd;
    }
    return __lookup( t, env.snd );
}

function __define (sym : Term, t : Term, env : Term) : Term {
    if (!__isSym(sym)) throw new Error(`define(sym, t, env) sym must be a symbol not ${t.kind}`);
    return Env( sym, t, env );
}

// -----------------------------------------------------------------------------
// Runtime
// -----------------------------------------------------------------------------


function __eval (t : Term, env : Term) : Term {
    console.log('-'.repeat(80));
    console.log(`EVAL:${t.kind} ${__deparse(t)}`);
    console.log('   %:ENV => ', __showLocalEnv(env));
    switch (true) {
    case __isNil(t)     :
    case __isTrue(t)    :
    case __isFalse(t)   :
    case __isNum(t)     :
    case __isStr(t)     :
    case __isNative(t)  :
    case __isFExpr(t)   : return t;
    case __isSym(t)     : return __lookup( t, env );
    case __isClosure(t) :
    case __isLambda(t)  : return Closure( t, env );
    case __isPair(t)    :
        let head = __eval( t.fst, env );
        let tail = t.snd;
        // FEXPRs
        if (__isFExpr(head)) {
            return __eval( ...(head.body( tail, env )) );
        }

        let call  : Term = head;
        let arg   : Term = __eval( tail, env );
        let local : Term = env;

        // unpack closures
        if (__isClosure(call)) {
            console.log(` ^unpack ~ ${__deparse(call)}`);
            local = call.env;
            call  = call.abs;
        }

        // run what we got ...
        switch (true) {
        case __isNative(call) :
            return call.body( arg );
        case __isLambda(call) :
            let param = call.param;
            if (!__isSym(param)) throw new Error(`Expected SYM for lambda param, but got ${param.kind}`);
            if (!__isPair(arg))  throw new Error(`Expected PAIR for lambda arg, but got ${arg.kind}`);
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

    const liftComparisonBinOp = (op : string, f : (n : number | string, m : number | string) => boolean) : NativeFunc => {
        return (arg : Term) : Term => {
            console.log(`   >:CALL &:(${op}) w/ ${__deparse(arg)}`);
            if (!__isPair(arg)) throw new Error(`Expected Pair in &:(${op}) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!__isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(${op}) not(${rhs.kind})`);
            rhs = rhs.fst;
            if (!(__isNum(lhs) || __isStr(lhs))) throw new Error(`Expected lhs to be Num/Str in &:(${op}) not(${lhs.kind})`);
            if (!(__isNum(rhs) || __isStr(rhs))) throw new Error(`Expected rhs to be Num/Str in &:(${op}) not(${rhs.kind})`);
            return f( lhs.value, rhs.value ) ? True() : False();
        }
    }

    const liftNumericBinOp = (op : string, f : (n : number, m : number) => number) : NativeFunc => {
        return (arg : Term) : Term => {
            console.log(`   >:CALL &:(${op}) w/ ${__deparse(arg)}`);
            if (!__isPair(arg)) throw new Error(`Expected Pair in &:(${op}) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!__isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(${op}) not(${rhs.kind})`);
            rhs = rhs.fst;
            if (!__isNum(lhs)) throw new Error(`Expected lhs to be Num in &:(${op}) not(${lhs.kind})`);
            if (!__isNum(rhs)) throw new Error(`Expected rhs to be Num in &:(${op}) not(${rhs.kind})`);
            return Num( f( lhs.value, rhs.value ) );
        }
    }

    // equality for all ...
    env = __define(
        Sym('=='), Native((arg : Term) : Term => {
            console.log(`   >:CALL &:(==) w/ ${__deparse(arg)}`);
            if (!__isPair(arg)) throw new Error(`Expected Pair in &:(==) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!__isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(==) not(${rhs.kind})`);
            return __eq( lhs, rhs.fst ) ? True() : False();
        }),
        env
    );

    env = __define(
        Sym('!='), Native((arg : Term) : Term => {
            console.log(`   >:CALL &:(!=) w/ ${__deparse(arg)}`);
            if (!__isPair(arg)) throw new Error(`Expected Pair in &:(!=) not(${arg.kind})`);
            let lhs = arg.fst;
            let rhs = arg.snd;
            if (!__isPair(rhs)) throw new Error(`Expected rhs to be Pair in &:(!=) not(${rhs.kind})`);
            return __eq( lhs, rhs.fst ) ? False() : True();
        }),
        env
    );

    // just for strings and numbers
    env = __define( Sym('>'),  Native(liftComparisonBinOp('>',  (n, m) => n >  m)), env );
    env = __define( Sym('>='), Native(liftComparisonBinOp('>=', (n, m) => n >= m)), env );
    env = __define( Sym('<'),  Native(liftComparisonBinOp('<',  (n, m) => n <  m)), env );
    env = __define( Sym('<='), Native(liftComparisonBinOp('<=', (n, m) => n <= m)), env );

    // just for numbers
    env = __define( Sym('+'), Native(liftNumericBinOp('+', (n, m) => n + m)), env );
    env = __define( Sym('-'), Native(liftNumericBinOp('-', (n, m) => n - m)), env );
    env = __define( Sym('*'), Native(liftNumericBinOp('*', (n, m) => n * m)), env );
    env = __define( Sym('/'), Native(liftNumericBinOp('/', (n, m) => n / m)), env );
    env = __define( Sym('%'), Native(liftNumericBinOp('%', (n, m) => n % m)), env );

    // lambda special form
    env = __define(
        Sym('lambda'), FExpr((arg : Term, env : Term) : [ Term, Term ] => {
            console.log(`   >:CALL @:(lambda) w/ ${__deparse(arg)}`);
            if (!__isPair(arg))     throw new Error(`Expected Pair as arg in @:(lambda) not(${arg.kind})`);
            if (!__isPair(arg.fst)) throw new Error(`Expected Pair as param in @:(lambda) not(${arg.fst.kind})`);
            if (!__isPair(arg.snd)) throw new Error(`Expected Pair as body in @:(lambda) not(${arg.snd.kind})`);
            let param = arg.fst;
            let body  = arg.snd;
            return [ Lambda( param.fst, body.fst ), env ];
        }),
        env
    );

    env = __define(
        Sym('let'), FExpr((arg : Term, env : Term) : [ Term, Term ] => {
            console.log(`   >:CALL @:(let) w/ ${__deparse(arg)}`);
            if (!__isPair(arg))     throw new Error(`Expected Pair as arg in @:(let) not(${arg.kind})`);
            if (!__isPair(arg.fst)) throw new Error(`Expected Pair as bind in @:(let) not(${arg.fst.kind})`);
            let bind = arg.fst;
            if (!__isSym(bind.fst))  throw new Error(`Expected Sym as bind/sym in @:(let) not(${bind.fst.kind})`);
            if (!__isPair(bind.snd)) throw new Error(`Expected Pair as bind/value in @:(let) not(${bind.snd.kind})`);
            let sym   = bind.fst;
            let value = bind.snd;
            let body  = arg.snd;
            if (!__isPair(body))  throw new Error(`Expected Pair as body in @:(let) not(${body.kind})`);
            return [ body.fst, __define( sym, value.fst, env ) ];
        }),
        env
    );

    return env;
}

// -----------------------------------------------------------------------------

let env = __initEnv();

console.log(__deparse(__eval(__parse(`

    (let (x 2)
    (let (y 10)
        ((lambda (x) (== y x)) (* 5 x))
    ))

`), env)));


/*
(== (((lambda (x) (lambda (y) (+ x y))) 10) 23) 30)
*/









