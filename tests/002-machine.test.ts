

// -----------------------------------------------------------------------------
// Core Types
// -----------------------------------------------------------------------------

// Literals

export type Bool = { type : 'BOOL', value : boolean }
export type Num  = { type : 'NUM',  value : number  }
export type Str  = { type : 'STR',  value : string  }

export type Literal = Num | Str | Bool

// Lists

export type Nil  = { type : 'NIL' }
export type Cons = { type : 'CONS', head : Expr, tail : List }

export type List = Cons | Nil

// Callables

export type NativeFunc  = ( env : Environment ) => Value
export type NativeFExpr = ( args : Expr[], env : Environment ) => Value

export type Native  = { type : 'NATIVE',  params : List, body : NativeFunc  }
export type FExpr   = { type : 'FEXPR',   params : List, body : NativeFExpr }
export type Closure = { type : 'CLOSURE', params : List, body : List, env : Environment }

export type Callable = Closure | Native | FExpr

// ...

export type Value = Literal | List | Callable

// -----------------------------------------------------------------------------
// Expressions
// -----------------------------------------------------------------------------

// Named Things

export type Ident = string

export type Word = { type : 'WORD', ident : Ident }
export type Var  = { type : 'VAR',  ident : Ident }

export type Identifier = Word | Var

// Callings things

export type Apply = { type : 'APPLY', call : Identifier, args : List }
export type Const = { type : 'CONST', literal : Literal }

// ...

export type Expr = Apply | Const | Identifier | Value

// -----------------------------------------------------------------------------

namespace AST {
    export function True  () : Bool { return { type : 'BOOL', value : true  } }
    export function False () : Bool { return { type : 'BOOL', value : false } }

    export function Num (value : number) : Num { return { type : 'NUM', value } }
    export function Str (value : string) : Str { return { type : 'STR', value } }

    export function Nil  () : Nil  { return { type : 'NIL'} }
    export function Cons (head : Expr, tail : List) : List { return { type : 'CONS', head, tail } }

    export function Native  (params : List, body : NativeFunc)  : Native  { return { type : 'NATIVE', params, body } }
    export function FExpr   (params : List, body : NativeFExpr) : FExpr   { return { type : 'FEXPR',  params, body } }
    export function Closure (params : List, body : List, env : Environment) : Closure {
        return { type : 'CLOSURE', params, body, env }
    }

    export function Word (ident : Ident) : Word { return { type : 'WORD', ident } }
    export function Var  (ident : Ident) : Var  { return { type : 'VAR',  ident } }

    export function Apply (call : Identifier, args : List) : Apply { return { type : 'APPLY', call, args } }
    export function Const (literal : Literal)              : Const { return { type : 'CONST', literal } }
}

namespace ListUtil {
    export function make (...items : Expr[]) : List {
        let list : List = AST.Nil();
        while (items.length > 0) {
            let next = items.pop() as Expr;
            list = AST.Cons(next, list);
        }
        return list;
    }

    export function head (l : List) : Expr {
        if (l.type == 'NIL') return l;
        return l.head;
    }

    export function tail (l : List) : List {
        if (l.type == 'NIL') return l;
        return l.tail;
    }

    export function map (l : List, f : (i : Expr) => Expr) : List {
        if (l.type == 'NIL') return l;
        return AST.Cons( f( head(l) ), map( tail(l), f ) );
    }

    export function flatten (l : List) : Expr[] {
        if (l.type == 'NIL') return [];
        return [ head(l), ...flatten( tail(l) ) ]
    }
}

// -----------------------------------------------------------------------------

namespace Parser {
    type SExpr = Expr | SExpr[];

    export function parse (src : string, keywords : string[]) : Expr {
        let [ sexpr, rest ] = parseTokens( tokenizer( src ), keywords );
        return buildTree( sexpr );
    }

    function tokenizer (src : string) : string[] {
        return src.replace(/\(/g, ' ( ')
                  .replace(/\)/g, ' ) ')
                  .trim()
                  .split(/\s+/)
                  .filter(Boolean);
    }

    function lexer (token : string, keywords : string[]) : Expr {
        switch (true) {
        case token == 'true'       : return AST.True();
        case token == 'false'      : return AST.False();
        case !isNaN(Number(token)) : return AST.Num(Number(token));
        case  isNaN(Number(token)) :
            if (keywords.includes(token)) {
                return AST.Word(token);
            } else {
                return AST.Var(token);
            }
        default:
            throw new Error(`Huh?`)
        }
    }

    function parseTokens (tokens : string[], keywords : string[]) : [ SExpr, string[] ] {
        let token = tokens[0];
        if (token == undefined) throw new Error('Undefined Token');

        let rest = tokens.slice(1);
        if (token == '(') return parseList( rest, [], keywords );

        return [ lexer(token, keywords), rest ];
    }

    function parseList (ts : string[], acc : SExpr[], keywords : string[]) : [ SExpr[], string[] ] {
        if (ts[0] === ')') return [ acc, ts.slice(1) ];
        let [ expr, remaining ] = parseTokens(ts, keywords);
        return parseList( remaining, [ ...acc, expr ], keywords );
    }

    function buildTree (sexpr : SExpr) : Expr {
        if (Array.isArray(sexpr)) {
            return ListUtil.make( ...sexpr.map(buildTree) )
        } else {
            return sexpr;
        }
    }

    export function format (expr : Expr) : string {
        switch (expr.type) {
        case 'BOOL'    : return expr.value ? 'true' : 'false';
        case 'NUM'     : return expr.value.toString();
        case 'STR'     : return `"${expr.value}"`;
        case 'NIL'     : return '()';
        case 'CONS'    : return `(${ ListUtil.flatten(expr).map(format).join(' ') })`;

        case 'WORD'    : return expr.ident;
        case 'VAR'     : return expr.ident;

        case 'CLOSURE' : return `(lambda ${format(expr.params)} ${format(expr.body)})`;
        case 'NATIVE'  : return `(native ${format(expr.params)} #:native)`;
        case 'FEXPR'   : return `(fexpr ${format(expr.params)} @:fexpr)`;

        case 'APPLY'   : return `(${format(expr.call)} ${format(expr.args)})`;
        case 'CONST'   : return format(expr.literal);
        default:
            return 'XXX'
        }
    }

}

// -----------------------------------------------------------------------------

export type MaybeEnvironment = Environment | undefined;

export class Environment {
    public parent   : MaybeEnvironment;
    public bindings : Map<string, Value> = new Map<string, Value>();

    constructor (parent : MaybeEnvironment = undefined) {
        this.parent = parent;
    }

    lookup (name : Identifier) : Value {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (name : Identifier, value : Value) : void {
        this.bindings.set(name.ident, value);
    }

    derive () : Environment { return new Environment( this ) }
    depth  () : number      { return 1 + (this.parent?.depth() ?? 0) }

    DUMP () : string {
        return `%E[${this.depth()}]{${ [ ...this.bindings.keys() ].map((e) => ("`" + e)).join(', ') }} `
            + (this.parent == undefined ? '' : ` ^(${ this.parent?.DUMP() })`)
    }
}

// -----------------------------------------------------------------------------

class Machine {

    //step (state : State) : State {}

    // gets an expression to evaluated
    // focuses on building, calling,
    // and finding things.
    //
    // focuses on:
    // - looking up variables
    // - building lambdas
    // - initiating function calls
    //
    // mostly just c & e
    // mostly create and read
    //evaluate (state : Evaluate) : Continue {}

    // gets a value to be returned and
    // performs any cleanup necessary
    // and delivers the value to
    // a waiting continuation.
    //
    // focuses on
    // - return values to waiting callers
    // - stack frames? scope cleanups?
    // - terminal if k is empty
    //
    // mostly just k & e
    // mostly create and write/update
    //kontinue (state : Continue) : Evaluate {}
}
