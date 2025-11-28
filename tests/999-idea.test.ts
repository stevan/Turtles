
import { Console } from 'console';

// -----------------------------------------------------------------------------
// Types
// -----------------------------------------------------------------------------

enum CoreType {
    BOOL   = 'BOOL',
    NUM    = 'NUM',
    STR    = 'STR',
    SYM    = 'SYM',
    NIL    = 'NIL',
    CONS   = 'CONS',
    PAIR   = 'PAIR',
    NATIVE = 'NATIVE',
    LAMBDA = 'LAMBDA',
}

interface Value { type : CoreType }

interface Atom extends Value {}
interface Bool extends Atom  { type : CoreType.BOOL, value : boolean }
interface Num  extends Atom  { type : CoreType.NUM,  value : number  }
interface Str  extends Atom  { type : CoreType.STR,  value : string  }

type NativeFunc = (env : Cons) => Value;

interface Callable extends Atom {}
interface Native   extends Callable { type : CoreType.NATIVE,  params : List, body : NativeFunc }
interface Lambda   extends Callable { type : CoreType.LAMBDA,  params : List, body : List }

interface Sym  extends Value  { type : CoreType.SYM,  ident : string  }
interface Pair extends Value  { type : CoreType.PAIR, first : Value, second : Value }

interface Nil  extends Value { type : CoreType.NIL }
interface Cons extends Value { type : CoreType.CONS, head : Value, tail : List }

type List = Nil | Cons

// -----------------------------------------------------------------------------
// Core Namespace
// -----------------------------------------------------------------------------

namespace $ {

    export const typeOf = (v : Value) : string => v.type;

    export const isTrue   = (v : Value) : v is Bool    => isBool(v) && v.value == true;
    export const isFalse  = (v : Value) : v is Bool    => isBool(v) && v.value == false;
    export const isBool   = (v : Value) : v is Bool    => typeOf(v) == CoreType.BOOL;
    export const isNum    = (v : Value) : v is Num     => typeOf(v) == CoreType.NUM;
    export const isStr    = (v : Value) : v is Str     => typeOf(v) == CoreType.STR;
    export const isNative = (v : Value) : v is Native  => typeOf(v) == CoreType.NATIVE;
    export const isLambda = (v : Value) : v is Lambda  => typeOf(v) == CoreType.LAMBDA;
    export const isAtom   = (v : Value) : v is Atom    => isBool(v) || isNum(v) || isStr(v) || isNative(v) || isLambda(v);

    export const isSym   = (v : Value) : v is Sym  => typeOf(v) == CoreType.SYM;
    export const isPair  = (v : Value) : v is Pair => typeOf(v) == CoreType.PAIR;
    export const isNil   = (v : Value) : v is Nil  => typeOf(v) == CoreType.NIL;
    export const isCons  = (v : Value) : v is Cons => typeOf(v) == CoreType.CONS;
    export const isList  = (v : Value) : v is List => isNil(v) || isCons(v);

    export const pair = (first : Value, second : Value) : Pair => {
        return { type : CoreType.PAIR, first, second }
    }

    export const nil    = () : Nil => { return { type : CoreType.NIL} }
    export const cons   = (head : Value, tail : List = nil()) : Cons => {
        return { type : CoreType.CONS, head, tail }
    }

    export const head = (l : Value) : Value => {
        if (!isCons(l)) throw new Error(`Can only call (head) on Cons not ${pprint(l)}`);
        return l.head;
    }

    export const tail = (l : Value) : List  => {
        if (!isCons(l)) throw new Error(`Can only call (tail) on Cons not ${pprint(l)}`);
        return l.tail;
    }

    export const first = (v : Value) : Value => {
        switch (true) {
        case isCons(v): return v.head;
        case isPair(v): return v.first;
        default:
            throw new Error(`Can only call (first) on Cons/Pair not ${pprint(v)}`);
        }
    }

    export const second = (v : Value) : Value => {
        switch (true) {
        case isCons(v): return first(v.tail);
        case isPair(v): return v.second;
        default:
            throw new Error(`Can only call (second) on Cons/Pair not ${pprint(v)}`);
        }
    }

    // expect native args ...

    export const bool = (value : boolean) : Bool => { return { type : CoreType.BOOL, value } }
    export const num  = (value : number)  : Num  => { return { type : CoreType.NUM,  value } }
    export const str  = (value : string)  : Str  => { return { type : CoreType.STR,  value } }
    export const sym  = (ident : string)  : Sym  => { return { type : CoreType.SYM,  ident } }

    // builtins
    export const native = (params : List, body : NativeFunc) : Native => {
        return { type : CoreType.NATIVE, params, body }
    }

    export const lambda = (params : List, body : List) : Lambda => {
        return { type : CoreType.LAMBDA, params, body }
    }

    export const pprint = (v : Value) : string => {
        switch (true) {
        case isNil(v)   : return '()';
        case isTrue(v)  : return '#t';
        case isFalse(v) : return '#f';
        case isNum(v)   : return v.value.toString();
        case isStr(v)   : return `"${v.value}"`;
        case isSym(v)   : return v.ident;
        case isPair(v)  : return `(${pprint(v.first)} : ${pprint(v.second)})`;
        case isNative(v): return `&:((${
            Lists.flatten(v.params).map((e) => pprint(e)).join(' ')
        }) #native)`;
        case isLambda(v): return `λ:((${
            Lists.flatten(v.params).map((e) => pprint(e)).join(' ')
        }) (${
            Lists.flatten(v.body).map((e) => pprint(e)).join(' ')
        }))`;
        case isCons(v)  : return isNil(v.tail)
            ? `(${pprint(v.head)})`
            : `(${pprint(v.head)} ${
                    Lists.flatten(v.tail).map((e) => pprint(e)).join(' ')
            })`;
        default: throw new Error(`Unknown value type (${JSON.stringify(v)})`);
        }
    }
}

// lists ...

namespace Lists {
    export const create = (...args : Value[]) : List => {
        let list : List = $.nil();
        while (args.length > 0) {
            list = $.cons( args.pop() as Value, list );
        }
        return list;
    }

    export const flatten = (l : List) : Value[] => $.isNil(l) ? [] : [ l.head, ...Lists.flatten(l.tail) ];
    export const length  = (l : List) : number  => $.isNil(l) ? 0  : 1 + Lists.length(l.tail);

    export const reduce = (l : List, f : (i : Value, acc : List) => List, acc : List) : List =>
        $.isNil(l)
            ? acc
            : Lists.reduce( l.tail, f, f( l.head, acc ) );

    export const map = (l : List, f : (i : Value) => Value) : List =>
        Lists.reduce(l, (i, acc) => $.cons(f(i), acc), $.nil());

    export const grep = (l : List, f : (i : Value) => boolean) : List =>
        Lists.reduce(l, (i, acc) => f(i) ? acc : $.cons(i, acc), $.nil());

    export const find = (l : List, f : (i : Value) => boolean) : Value =>
        $.isNil(l)
            ? $.nil()
            : f(l.head) ? l.head : Lists.find( l.tail, f );
}

// -----------------------------------------------------------------------------
// Runtime
// -----------------------------------------------------------------------------

const DEBUG      = process.env['DEBUG'] == "1" ? true : false
const TERM_WIDTH = process.stdout.columns;
const MAX_WIDTH  = TERM_WIDTH - 1

const Logger = new Console({
    stdout         : process.stdout,
    stderr         : process.stderr,
    inspectOptions : {
        depth       : 20,
        breakLength : TERM_WIDTH,
    },
})

const Dumper = new Console({
    stdout           : process.stdout,
    stderr           : process.stderr,
    inspectOptions   : {
        compact      : true,
        breakLength  : Infinity,
        depth        : 2,
    },
    groupIndentation : 4,
})

const DUMP = (v : Value) : string => {
    switch (true) {
    case Env.isEnv(v) : return Env.concise(v);
    case isClosure(v) :
        let tag  = $.first(v);
        let body = $.second(v) as Cons;
        return `[ ${$.pprint(tag)} ${$.pprint($.first(body))} @ ${Env.concise($.second(body))} ]`;
    default:
        return $.pprint(v);
    }
}

const HEADER = (label : string, width : number = MAX_WIDTH) : void =>
    Logger.log(`== ${label} ${'='.repeat( width - (label.length + 4) )}`);
const FOOTER = (width : number = MAX_WIDTH) : void => Logger.log('-'.repeat(width));

const LOG   = (d : number, ...args : any[]) : void => {
    if (DEBUG) {
        Logger.log([(d == 0 ? '' : '  '.repeat(d)), ...args].join(''));
    }
}

// parser ...

namespace Parser {

    type ParseExpr = Value | ParseExpr[];

    const SPLITTER = /'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g

    export const tokenize = (src : string) : string[] => src.match(SPLITTER) ?? [];

    export const parse = (source : string) : Value => {
        let [ expr, rest ] = parseTokens( tokenize( source ) );
        return buildTree( expr );
    }

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
        (Array.isArray(expr)) ? Lists.create( ...expr.map(buildTree) ) : expr;
}

// compiler ...

namespace Compiler {
    export const compile = (e : Value, d : number = 0) : Value => {
        LOG(d, 'COMPILE :', $.pprint(e));
        switch (true) {
        case $.isSym(e)  : return Var(e);
        case $.isAtom(e) : return Val(e);
        case $.isPair(e) : return Val(e);
        case $.isCons(e) :
            let top = $.head(e);
            switch (true) {
            case $.isSym(top):
                if (top.ident == 'lambda') {
                    LOG(d+1, 'LAMBDA  :', $.pprint(e));
                    let form = $.tail(e);
                    return Lambda( $.lambda(
                        $.first(form) as List,
                        $.cons( compile( $.second(form), d+2 ) )
                    ) );
                }
            default:
                LOG(d+1, 'CHECK???? :', $.pprint(e));
                let head = compile( top, d+2 );
                let rest = $.tail(e);
                LOG(d+1, 'HEAD-is   :', $.pprint(head));
                if (isVar(head) || isLambda(head)) {
                    LOG(d+2, 'APPLY   :', $.pprint(e));
                    return Apply(
                        $.cons(
                            head,
                            Lists.map(rest, (a) => compile(a, d+3))
                        )
                    );
                } else {
                    LOG(d+2, 'LIST   :', $.pprint(e));
                    if ($.isNil(rest)) {
                        return $.cons(head);
                    } else {
                        return $.cons(head, compile(rest, d+3) as List)
                    }
                }
            }
        default :
            throw new Error(`HMMM ${JSON.stringify(e)}`)
        }
    }
}

// environment ...

namespace Env {
    export const isEnv = (v : Value) : v is Cons => hasTag(v, Tags.Env);

    export const create = (bindings : List = $.nil()) : Cons =>
        $.cons($.sym(Tags.Env), bindings);

    export const set = (env : Cons, symbol : Sym, value : Value) : Cons => {
        return create(
            $.cons(
                $.pair( symbol, value ),
                env.tail
            )
        )
    }

    export const get = (env : Cons, symbol : Sym) : Value => {
        let bind = Lists.find( env.tail, (b) => {
            return symbol.ident == ($.first(b) as Sym).ident;
        });
        return $.second(bind);
    }

    export const init = () : Cons => {
        let env = create();

        env = set( env, $.sym('+'), liftNumericBinOp((n, m) => n + m));
        env = set( env, $.sym('-'), liftNumericBinOp((n, m) => n - m));
        env = set( env, $.sym('*'), liftNumericBinOp((n, m) => n * m));
        env = set( env, $.sym('/'), liftNumericBinOp((n, m) => n / m));
        env = set( env, $.sym('%'), liftNumericBinOp((n, m) => n % m));

        return env;
    }

    export const concise = (env : Value) : string => {
        if (!isEnv(env)) throw new Error('Expected Env');
        return `${$.pprint($.head(env))} % ${$.pprint(
            Lists.grep($.tail(env), (e) => {
                return $.isPair(e) && $.isSym(e.first) && $.isNative(e.second)
            })
        )}`;
    }

    export const pprint = (env : Value) : string => {
        if (!isEnv(env)) throw new Error('Expected Env');
        return `${$.pprint($.head(env))} % \n  ${
            Lists.flatten($.tail(env)).map((e) => $.pprint(e)).join('\n  ')
        }`;
    }

    // utils ...

    const liftNumericBinOp = (f : (n : number, m : number) => number) : Native => {
        return $.native(
            $.cons( $.sym('lhs'), $.cons( $.sym('rhs') ) ),
            (env : Cons) : Value => {
                let lhs = Env.get( env, $.sym('lhs') );
                let rhs = Env.get( env, $.sym('rhs') );
                if (!$.isNum(lhs)) throw new Error('Expected lhs to be Num');
                if (!$.isNum(rhs)) throw new Error('Expected rhs to be Num');
                return $.num( f(lhs.value, rhs.value) );
            }
        )
    }
}

// Interpeter AST

const Tags = {
    Env     : '`Env',
    Val     : '`Val',
    Var     : '`Var',
    Apply   : '`Apply',
    Lambda  : '`Lambda',
    Closure : '`Closure',
}

const isTag = (tag : Sym) : boolean => {
    return tag.ident == '`Env'
        || tag.ident == '`Val'
        || tag.ident == '`Var'
        || tag.ident == '`Apply'
        || tag.ident == '`Lambda'
        || tag.ident == '`Closure'
}

const hasTag = (v : Value, tag : string) : boolean => {
    return $.isCons(v) && $.isSym(v.head) && v.head.ident == tag;
}

const isVal     = (v : Value) : v is Cons => hasTag(v, Tags.Val);
const isVar     = (v : Value) : v is Cons => hasTag(v, Tags.Var);
const isApply   = (v : Value) : v is Cons => hasTag(v, Tags.Apply);
const isLambda  = (v : Value) : v is Cons => hasTag(v, Tags.Lambda);
const isClosure = (v : Value) : v is Cons => hasTag(v, Tags.Closure);

const Val     = (value    : Value)  : Cons => $.cons($.sym(Tags.Val),     $.cons(value));
const Var     = (symbol   : Sym)    : Cons => $.cons($.sym(Tags.Var),     $.cons(symbol));
const Lambda  = (func     : Lambda) : Cons => $.cons($.sym(Tags.Lambda),  $.cons(func));
const Apply   = (call     : List)   : Cons => $.cons($.sym(Tags.Apply),   $.cons(call));
const Closure = (capture  : List)   : Cons => $.cons($.sym(Tags.Closure), $.cons(capture));

namespace Interpreter {

    export const run = (source : string) : Value => {
        HEADER('PARSE:');
        LOG(0, `source :[\n${source}\n]`);
        let parsed = Parser.parse(source);
        FOOTER();
        LOG(0, `parsed : ${$.pprint(parsed)}`);
        HEADER('COMPILE:');
        let compiled = Compiler.compile(parsed);
        FOOTER();
        LOG(0, `compiled : ${$.pprint(compiled)}`);
        HEADER('EXECUTE:');
        let env = Env.init();
        LOG(0, `environment : ${Env.pprint(env)}`);
        FOOTER();
        let result = exec( compiled, env );
        HEADER('RESULT:');
        LOG(0, `  << result : ${$.pprint(result)}`);
        LOG(0, `environment : ${Env.pprint(env)}`);
        FOOTER();
        return result;
    }

    export const exec = (expr : Value, env : Cons, d : number = 0) : Value => {
        LOG(d, `EXEC  | ${DUMP(expr)}`);
        switch (true) {
        case $.isCons(expr):
            let tag = expr.head;
            if ($.isSym(tag) && isTag(tag)) {
                let body = $.head(expr.tail);
                LOG(d, ` +TAG : ${$.pprint(tag)}`);
                switch (tag.ident) {
                case Tags.Val     : return body;
                case Tags.Var     : return evaluate( body, env, d+1 );
                case Tags.Apply   :
                    return apply(
                        evaluate( body, env, d+1 ), env, d+2
                    );
                case Tags.Lambda  : return body;
                case Tags.Closure :
                    let abs   = $.head(body);
                    let local = $.head($.tail(body)) as Cons;
                    switch (true) {
                    case $.isNative(abs):
                        return abs.body( local );
                    case $.isLambda(abs):
                        LOG(d, `LAMBDA ==> ${$.pprint($.head(abs.body))} env: ${Env.concise(local)}`);
                        return exec( $.head(abs.body), local, d+1 );
                    default:
                        throw new Error(`CLOSURE TODO - ${$.pprint(expr)}`)
                    }
                default:
                    throw new Error(`TODO - ${$.pprint(expr)}`)
                }
            } else {
                LOG(d, ` LIST > ${$.pprint(expr)}`);
                return evaluate( expr, env, d+1 );
            }
        default:
            throw new Error(`RUN! ${JSON.stringify(expr)}`);
        }
    }

    export const evaluate = (expr : Value, env : Cons, d : number = 0) : Value => {
        LOG(d, `EVAL  @ ${DUMP(expr)}`);
        switch (true) {
        case $.isSym(expr)  : return Env.get( env, expr );
        case $.isAtom(expr) : return expr;
        case $.isPair(expr) : return expr;
        case $.isNil(expr)  : return expr;
        case $.isCons(expr) :
            let head = exec( $.head(expr), env, d+1 );
            if ($.isNil($.tail(expr))) {
                return $.cons( head );
            } else {
                return $.cons(
                    head,
                    evaluate( $.tail(expr), env, d+1 ) as List
                );
            }
        default:
            throw new Error(`EVAL! ${JSON.stringify(expr)}`);
        }
    }

    export const apply = (expr : Value, env : Cons, d : number = 0) : Value => {
        LOG(d, `APPLY | ${DUMP(expr)}`);
        let app    = $.head(expr);
        let args   = $.tail(expr);

        if (isClosure(app)) {
            throw new Error(`!!! GOT CLOSURE !!! ${$.pprint(app)} args: ${$.pprint(args)} env: ${Env.concise(env)}`);
        }

        switch (true) {
        case ($.isNative(app) || $.isLambda(app)):

            LOG(d, ` BIND > params: ${$.pprint(app.params)} with args:${$.pprint(args)}`);
            let local  = env;
            let params = Lists.flatten(app.params);
            let evaled = Lists.flatten(args);
            while (params.length > 0) {
                let p = params.shift() as Sym;
                let a = evaled.shift() as Value;
                LOG(d+1, `- setting ${$.pprint(p)} to ${$.pprint(a)}`);
                local = Env.set( local, p, a );
            }
            LOG(d, ` BIND < local: ${Env.concise(local)}`);

            return exec( Closure( $.cons( app, $.cons(local) ) ), env, d+1 );
        default:
            throw new Error(`APPLY! ${$.pprint(expr)}`);
        }
    }
}

// -----------------------------------------------------------------------------

let result = Interpreter.run(`
    ((lambda (x y) (+ x y)) 10 20)
`);











