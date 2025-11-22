

// -----------------------------------------------------------------------------
// Parser Constructs these things ...
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

// Named Things

export type Word = { type : 'WORD', ident : string }
export type Var  = { type : 'VAR',  ident : string }

export type Ident = Word | Var

// Callings things

export type Apply = { type : 'APPLY', call : Ident, args : List }

// -----------------------------------------------------------------------------
// Runtime either has these things or evalutes to them
// -----------------------------------------------------------------------------

// Callables

export type NativeFunc  = ( env : Env ) => Expr
export type NativeFExpr = ( args : Expr[], env : Env ) => Expr

export type Native = { type : 'NATIVE',  params : List, body : NativeFunc  }
export type FExpr  = { type : 'FEXPR',   params : List, body : NativeFExpr }
export type Lambda = { type : 'LAMBDA', params : List, body : List, env : Env }

export type Callable = Lambda | Native | FExpr

// -----------------------------------------------------------------------------
// VM uses these things ...
// -----------------------------------------------------------------------------

export type Term  = Literal | List | Callable // cannot be evaluated
export type Abs   = Ident | Apply             // must be evaluated
export type Expr  = Abs | Term

// -----------------------------------------------------------------------------

namespace AST {
    export function True  () : Bool { return { type : 'BOOL', value : true  } }
    export function False () : Bool { return { type : 'BOOL', value : false } }

    export function Num (value : number) : Num { return { type : 'NUM', value } }
    export function Str (value : string) : Str { return { type : 'STR', value } }

    export function Nil  () : Nil  { return { type : 'NIL'} }
    export function Cons (head : Expr, tail : List) : List { return { type : 'CONS', head, tail } }

    export function Native (params : List, body : NativeFunc)  : Native  { return { type : 'NATIVE', params, body } }
    export function FExpr  (params : List, body : NativeFExpr) : FExpr   { return { type : 'FEXPR',  params, body } }
    export function Lambda (params : List, body : List, env : Env) : Lambda {
        return { type : 'LAMBDA', params, body, env }
    }

    export function Word (ident : string) : Word { return { type : 'WORD', ident } }
    export function Var  (ident : string) : Var  { return { type : 'VAR',  ident } }

    export function Apply (call : Ident, args : List) : Apply { return { type : 'APPLY', call, args } }
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

    export function parse (src : string) : Expr {
        let [ sexpr, rest ] = parseTokens( tokenizer( src ) );
        return buildTree( sexpr );
    }

    function tokenizer (src : string) : string[] {
        return src.replace(/\(/g, ' ( ')
                  .replace(/\)/g, ' ) ')
                  .trim()
                  .split(/\s+/)
                  .filter(Boolean);
    }

    function lexer (token : string) : Expr {
        switch (true) {
        case token == 'true'       : return AST.True();
        case token == 'false'      : return AST.False();
        case !isNaN(Number(token)) : return AST.Num(Number(token));
        case  isNaN(Number(token)) : return AST.Var(token);
        default:
            throw new Error(`Huh?`)
        }
    }

    function parseTokens (tokens : string[]) : [ SExpr, string[] ] {
        let token = tokens[0];
        if (token == undefined) throw new Error('Undefined Token');

        let rest = tokens.slice(1);
        if (token == '(') return parseList( rest, [] );

        return [ lexer(token), rest ];
    }

    function parseList (ts : string[], acc : SExpr[]) : [ SExpr[], string[] ] {
        if (ts[0] === ')') return [ acc, ts.slice(1) ];
        let [ expr, remaining ] = parseTokens(ts);
        return parseList( remaining, [ ...acc, expr ] );
    }

    function buildTree (sexpr : SExpr) : Expr {
        if (Array.isArray(sexpr)) {
            let list = sexpr.map(buildTree);

            if (list.length == 0) return AST.Nil();

            if (list[0] != undefined && list[0].type == 'VAR') {
                let [ call, ...args ] = list;
                return AST.Apply( AST.Word( call.ident ), ListUtil.make( ...args ) );
            } else {
                return ListUtil.make( ...list )
            }
        } else {
            return sexpr;
        }
    }

    export function format (expr : Expr) : string {
        switch (expr.type) {
        case 'BOOL'   : return expr.value ? 'true' : 'false';
        case 'NUM'    : return expr.value.toString();
        case 'STR'    : return `"${expr.value}"`;
        case 'NIL'    : return '()';
        case 'CONS'   : return `(${ ListUtil.flatten(expr).map(format).join(' ') })`;

        case 'WORD'   : return expr.ident;
        case 'VAR'    : return expr.ident;

        case 'LAMBDA' : return `(lambda ${format(expr.params)} ${format(expr.body)})`;
        case 'NATIVE' : return `(native ${format(expr.params)} #:native)`;
        case 'FEXPR'  : return `(fexpr ${format(expr.params)} @:fexpr)`;

        case 'APPLY'  : return `(${format(expr.call)} ${ ListUtil.flatten(expr.args).map(format).join(' ') })`;
        default:
            return 'XXX'
        }
    }

}

//let ast = Parser.parse(`(foo 1 (+ 2 10) (bar 2 3))`);
//console.log(JSON.stringify(ast, null, 4));
//console.log(Parser.format(ast));

// -----------------------------------------------------------------------------

export type MaybeEnv = Env | undefined;

export class Env {
    public parent   : MaybeEnv;
    public bindings : Map<string, Expr> = new Map<string, Expr>();

    constructor (parent : MaybeEnv = undefined) {
        this.parent = parent;
    }

    lookup (name : Ident) : Expr {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (name : Ident, value : Expr) : void {
        this.bindings.set(name.ident, value);
    }

    derive () : Env    { return new Env( this ) }
    depth  () : number { return 1 + (this.parent?.depth() ?? 0) }

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
