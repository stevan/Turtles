// -----------------------------------------------------------------------------
// Core Types
// -----------------------------------------------------------------------------

type True   = { type : '*T*', value : true  }
type False  = { type : '*F*', value : false }

type Bool = True | False

type Int = { type : 'INT', value : number  }
type Flt = { type : 'FLT', value : number  }
type Str = { type : 'STR', value : string  }

type Literal = Int | Flt | Str | Bool

type Nil  = { type : 'NIL' }
type Cons = { type : 'CONS', head : Expr, tail : List }

type List = Cons | Nil

type NativeFunc  = ( args : Expr[] ) => Expr
type NativeFExpr = ( args : Expr[], env: Environment ) => Expr

type Lambda  = { type : 'LAMBDA',  params : List, body : List }
type Native  = { type : 'NATIVE',  params : List, body : NativeFunc  }
type FExpr   = { type : 'FEXPR',   params : List, body : NativeFExpr }

type Callable = Lambda | Native | FExpr

type Ident = string

type Word    = { type : 'WORD',    ident : Ident }
type Var     = { type : 'VAR',     ident : Ident }
type Special = { type : 'SPECIAL', ident : Ident }

type Identifier = Word | Var | Special

type Expr = Literal | List | Callable | Identifier

// -----------------------------------------------------------------------------
// Type Checkers & Assertions
// -----------------------------------------------------------------------------

namespace TypeUtil {
    export function isWord       (v : any) : v is Word       { return v.type == 'WORD' }
    export function isVar        (v : any) : v is Var        { return v.type == 'VAR'  }
    export function isInt        (v : any) : v is Int        { return v.type == 'INT'  }
    export function isFlt        (v : any) : v is Flt        { return v.type == 'FLT'  }
    export function isStr        (v : any) : v is Str        { return v.type == 'STR'  }
    export function isNil        (v : any) : v is Nil        { return v.type == 'NIL'  }
    export function isCons       (v : any) : v is Cons       { return v.type == 'CONS' }
    export function isTrue       (v : any) : v is True       { return v.type == '*T*'  }
    export function isFalse      (v : any) : v is False      { return v.type == '*F*'  }
    export function isFExpr      (v : any) : v is FExpr      { return v.type == 'FEXPR'   }
    export function isNative     (v : any) : v is Native     { return v.type == 'NATIVE'  }
    export function isLambda     (v : any) : v is Lambda     { return v.type == 'LAMBDA'  }
    export function isSpecial    (v : any) : v is Special    { return v.type == 'SPECIAL' }
    export function isList       (v : any) : v is List       { return isCons(v)    || isNil(v)    }
    export function isBool       (v : any) : v is Bool       { return isTrue(v)    || isFalse(v)  }
    export function isCallable   (v : any) : v is Callable   { return isNative(v)  || isLambda(v) || isFExpr(v) }
    export function isIdentifier (v : any) : v is Identifier { return isWord(v)    || isVar(v)    || isSpecial(v) }
    export function isLiteral    (v : any) : v is Literal    { return isInt(v)  || isFlt(v) || isStr(v)  || isBool(v) }
    export function isExpr       (v : any) : v is Expr       { return isList(v) || isLiteral(v) || isCallable(v) || isIdentifier(v) }

    export function assertInt        (v : any) : asserts v is Int        { if (!isInt(v))        throw new Error("Not Int")        }
    export function assertFlt        (v : any) : asserts v is Flt        { if (!isFlt(v))        throw new Error("Not Flt")        }
    export function assertStr        (v : any) : asserts v is Str        { if (!isStr(v))        throw new Error("Not Str")        }
    export function assertTrue       (v : any) : asserts v is True       { if (!isTrue(v))       throw new Error("Not True")       }
    export function assertFalse      (v : any) : asserts v is False      { if (!isFalse(v))      throw new Error("Not False")      }
    export function assertBool       (v : any) : asserts v is Bool       { if (!isBool(v))       throw new Error("Not Bool")       }
    export function assertNil        (v : any) : asserts v is Nil        { if (!isNil(v))        throw new Error("Not Nil")        }
    export function assertCons       (v : any) : asserts v is Cons       { if (!isCons(v))       throw new Error("Not Cons")       }
    export function assertList       (v : any) : asserts v is List       { if (!isList(v))       throw new Error("Not List")       }
    export function assertFExpr      (v : any) : asserts v is FExpr      { if (!isFExpr(v))      throw new Error("Not FExpr")      }
    export function assertLambda     (v : any) : asserts v is Lambda     { if (!isLambda(v))     throw new Error("Not Lambda")     }
    export function assertNative     (v : any) : asserts v is Native     { if (!isNative(v))     throw new Error("Not Native")     }
    export function assertSpecial    (v : any) : asserts v is Special    { if (!isSpecial(v))    throw new Error("Not Special")    }
    export function assertVar        (v : any) : asserts v is Var        { if (!isVar(v))        throw new Error("Not Var")        }
    export function assertWord       (v : any) : asserts v is Word       { if (!isWord(v))       throw new Error("Not Word")       }
    export function assertCallable   (v : any) : asserts v is Callable   { if (!isCallable(v))   throw new Error("Not Callable")   }
    export function assertIdentifier (v : any) : asserts v is Identifier { if (!isIdentifier(v)) throw new Error("Not Identifier") }
    export function assertLiteral    (v : any) : asserts v is Literal    { if (!isLiteral(v))    throw new Error("Not Literal")    }
    export function assertExpr       (v : any) : asserts v is Expr       { if (!isExpr(v))       throw new Error("Not Expr")       }
}

// -----------------------------------------------------------------------------
// Constructors
// -----------------------------------------------------------------------------

function True  () : True  { return { type : '*T*', value : true  } }
function False () : False { return { type : '*F*', value : false } }

function Int (value : number) : Int { return { type : 'INT', value } }
function Flt (value : number) : Flt { return { type : 'FLT', value } }
function Str (value : string) : Str { return { type : 'STR', value } }

function Nil  ()                         : Nil  { return { type : 'NIL'} }
function Cons (head : Expr, tail : List) : List { return { type : 'CONS', head, tail } }

function Lambda (params : List, body : List)        : Lambda { return { type : 'LAMBDA', params, body } }
function Native (params : List, body : NativeFunc)  : Native { return { type : 'NATIVE', params, body } }
function FExpr  (params : List, body : NativeFExpr) : FExpr  { return { type : 'FEXPR',  params, body } }

function Word    (ident : Ident) : Word    { return { type : 'WORD',    ident } }
function Var     (ident : Ident) : Var     { return { type : 'VAR',     ident } }
function Special (ident : Ident) : Special { return { type : 'SPECIAL', ident } }

// -----------------------------------------------------------------------------
// List helpers
// -----------------------------------------------------------------------------

namespace ListUtil {

    export function create (...items : Expr[]) : List {
        let list : List = Nil();
        while (items.length > 0) {
            let next = items.pop();
            TypeUtil.assertExpr(next);
            list = Cons(next, list);
        }
        return list;
    }

    export function head (l : List) : Expr {
        if (TypeUtil.isNil(l)) return l;
        return l.head;
    }

    export function tail (l : List) : List {
        if (TypeUtil.isNil(l)) return l;
        return l.tail;
    }

    export function map (l : List, f : (i : Expr) => Expr) : List {
        if (TypeUtil.isNil(l)) return l;
        return Cons( f( head(l) ), map( tail(l), f ) );
    }

    export function flatten (l : List) : Expr[] {
        if (TypeUtil.isNil(l)) return [];
        return [ head(l), ...flatten( tail(l) ) ]
    }
}



// -----------------------------------------------------------------------------
// Parser & Deparser
// -----------------------------------------------------------------------------

namespace Parser {

    export function format (expr : Expr) : string {
        switch (true) {
        case TypeUtil.isTrue(expr)    : return 'true';
        case TypeUtil.isFalse(expr)   : return 'false';
        case TypeUtil.isInt(expr)     :
        case TypeUtil.isFlt(expr)     : return expr.value.toString();
        case TypeUtil.isStr(expr)     : return expr.value;
        case TypeUtil.isNil(expr)     : return '()';
        case TypeUtil.isCons(expr)    : return `(${ ListUtil.flatten(expr).map(format).join(' ') })`;
        case TypeUtil.isWord(expr)    :
        case TypeUtil.isVar(expr)     :
        case TypeUtil.isSpecial(expr) : return expr.ident;
        case TypeUtil.isLambda(expr)  : return `(lambda ${format(expr.params)} ${format(expr.body)})`;
        case TypeUtil.isNative(expr)  : return `(native ${format(expr.params)} #:native)`;
        case TypeUtil.isFExpr(expr)   : return  `(fexpr ${format(expr.params)} @:fexpr)`;
        default:
            return 'XXX'
        }
    }

    // -----------------------------------------------------------------------------
    // Parser
    // -----------------------------------------------------------------------------

    type SExpr = Expr | SExpr[];

    function tokenizer ( src : string ) : string[] {
        return src.replace(/\(/g, ' ( ')
                  .replace(/\)/g, ' ) ')
                  .trim()
                  .split(/\s+/)
                  .filter(Boolean);
    }

    function lexer ( token : string ) : Expr {
        switch (true) {
        case token == 'true'  : return True();
        case token == 'false' : return False();
        case !isNaN(Number(token)) && Math.trunc(Number(token)) == Number(token) : return Int(Number(token));
        case !isNaN(Number(token)) && Math.trunc(Number(token)) != Number(token) : return Flt(Number(token));
        case  isNaN(Number(token)) : return Word(token);
        default:
            throw new Error(`Huh?`)
        }
    }

    function parseList ( ts : string[], acc : SExpr[] ) : [ SExpr[], string[] ] {
        if (ts[0] === ')') return [ acc, ts.slice(1) ];
        let [ expr, remaining ] = parseTokens(ts);
        return parseList( remaining, [ ...acc, expr ] );
    }

    function parseTokens ( tokens : string[] ) : [ SExpr, string[] ] {
        let token = tokens[0];
        if (token == undefined) throw new Error('Undefined Token');

        let rest = tokens.slice(1);
        if (token == '(') return parseList( rest, [] );

        return [ lexer(token), rest ];
    }

    function buildTree ( sexpr : SExpr ) : Expr {
        if (Array.isArray(sexpr)) {
            return ListUtil.create( ...sexpr.map(buildTree) )
        } else {
            TypeUtil.assertExpr(sexpr)
            return sexpr;
        }
    }

    export function parse ( src : string ) : Expr {
        let [ sexpr, rest ] = parseTokens( tokenizer( src ) );
        return buildTree( sexpr );
    }
}

// -----------------------------------------------------------------------------
// Environment
// -----------------------------------------------------------------------------

type MaybeEnvironment = Environment | undefined;

class Environment {
    public parent   : MaybeEnvironment;
    public bindings : Map<Ident, Expr> = new Map<Ident, Expr>();

    constructor (parent : MaybeEnvironment = undefined) {
        this.parent = parent;
    }

    lookup (name : Identifier) : Expr {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (key : Identifier, val : Expr) : void {
        this.bindings.set(key.ident, val);
    }

    derive () : Environment { return new Environment( this ) }
    depth  () : number      { return 1 + (this.parent?.depth() ?? 0) }

    DUMP () : string {
        return `%E[${this.depth()}]{${ [ ...this.bindings.keys() ].map((e) => ("`" + e)).join(', ') }} `
            + (this.parent == undefined ? '' : ` ^(${ this.parent?.DUMP() })`)
    }
}

// -----------------------------------------------------------------------------
// Interpreter
// -----------------------------------------------------------------------------

const DEBUG = true;
const LOG   = (d : number, msg : string, e : any = undefined) =>
    console.log(`LOG[ ${d.toString().padStart(2, '0')} ] ${msg} - `, e ? `${Parser.format(e)}` : '...' );

const DUMP = (label : string, expr : Expr, env : Environment) => {
    console.group(`-- ${label} `, '-'.repeat(80 - (label.length + 5)));
    console.log('%.ENV  : ', env.DUMP());
    console.log('@.EXPR : ', Parser.format(expr));
    console.groupEnd();
    console.log('-'.repeat(80));
}


function run (expr : Expr, env : Environment) : Expr {
    return evaluate( expr, env );
}

function evaluate (expr : Expr, env : Environment, depth : number = 0) : Expr {
    if (DEBUG) DUMP( 'TICK', expr, env );
    switch (true) {
    case TypeUtil.isCons(expr):
        if (DEBUG) LOG(depth, 'Got CONS');
        let top = evaluate(ListUtil.head(expr), env, depth + 1);
        if (DEBUG) LOG(depth, 'EVAL(h)', top);
        switch (true) {
        case TypeUtil.isFExpr(top):
            if (DEBUG) LOG(depth, '++ APPLY *FEXPR*', top);
            return top.body( ListUtil.flatten( ListUtil.tail(expr) ), env );
        case TypeUtil.isNative(top):
            if (DEBUG) LOG(depth, '++ APPLY *NATIVE*', top);
            return top.body( ListUtil.flatten( evaluate(ListUtil.tail(expr), env, depth + 1) as List ) );
        case TypeUtil.isLambda(top):
            if (DEBUG) LOG(depth, '++ APPLY *LAMBDA*', top);
            let params = ListUtil.flatten(top.params);
            let args   = ListUtil.flatten(evaluate(ListUtil.tail(expr), env, depth + 1) as List);
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                TypeUtil.assertIdentifier(param);
                TypeUtil.assertExpr(arg);
                localE.assign(param, arg);
            }
            return evaluate( top.body, localE );
        default:
            if (DEBUG) LOG(depth, '*LIST*');
            return Cons( top, evaluate(ListUtil.tail(expr), env, depth + 1) as List );
        }
    case TypeUtil.isIdentifier(expr):
        if (DEBUG) LOG(depth, 'Got Idenfifier = Var | Word | Special', expr);
        return env.lookup(expr);
    case TypeUtil.isLiteral(expr):
        if (DEBUG) LOG(depth, 'Got Literal', expr);
        return expr;
    case TypeUtil.isNil(expr):
        if (DEBUG) LOG(depth, '()', expr);
        return expr;
    default:
        throw new Error('WTF!');
    }
}

// -----------------------------------------------------------------------------
// Built-in Environment
// -----------------------------------------------------------------------------

let env = new Environment();

env.assign( Special('lambda'), FExpr(
    ListUtil.create( Var('params'), Var('body') ),
    (args : Expr[], env : Environment) : Expr => {
        let [ params, body ] = args;
        TypeUtil.assertList(params);
        TypeUtil.assertList(body);
        return Lambda( params, body );
    }
));

env.assign( Special('if'), FExpr(
    ListUtil.create( Var('cond'), Var('then'), Var('else') ),
    (args : Expr[], env : Environment) : Expr => {
        let [ cond, thenBranch, elseBranch ] = args;
        TypeUtil.assertCons(cond);
        TypeUtil.assertCons(thenBranch);
        TypeUtil.assertCons(elseBranch);
        let result = evaluate( cond, env );
        return evaluate( TypeUtil.isFalse(result) ? elseBranch : thenBranch, env );
    }
));

env.assign( Word('=='), Native(
    ListUtil.create( Var('n'), Var('m') ),
    (args : Expr[]) : Expr => {
        let [ lhs, rhs ] = args;
        TypeUtil.assertLiteral(lhs);
        TypeUtil.assertLiteral(rhs);
        return (lhs.value == rhs.value) ? True() : False();
    }
));

env.assign( Word('+'), Native(
    ListUtil.create( Var('n'), Var('m') ),
    (args : Expr[]) : Expr => {
        let [ lhs, rhs ] = args;
        TypeUtil.assertInt(lhs);
        TypeUtil.assertInt(rhs);
        return Int(lhs.value + rhs.value);
    }
));

env.assign( Word('*'), Native(
    ListUtil.create( Var('n'), Var('m') ),
    (args : Expr[]) : Expr => {
        let [ lhs, rhs ] = args;
        TypeUtil.assertInt(lhs);
        TypeUtil.assertInt(rhs);
        return Int(lhs.value * rhs.value);
    }
));

// -----------------------------------------------------------------------------
// Demo Program
// -----------------------------------------------------------------------------

let program = Parser.parse(`
    ((lambda (x y)
        (if (== x 10)
            (+ x y)
            (* x y))) 11 20)
`);

console.log(Parser.format(program));

DUMP( 'RESULT', run( program, env ), env );

// -----------------------------------------------------------------------------


