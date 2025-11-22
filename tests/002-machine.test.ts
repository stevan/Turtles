

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

export type Sym = { type : 'SYM', ident : string }

// Callables

export type NativeFunc  = ( env : Env ) => Expr
export type NativeFExpr = ( args : Expr[], env : Env ) => Expr

export type Native = { type : 'NATIVE', params : List, body : NativeFunc  }
export type FExpr  = { type : 'FEXPR',  params : List, body : NativeFExpr }
export type Lambda = { type : 'LAMBDA', params : List, body : Expr, env : Env }

export type Callable = Lambda | Native | FExpr

// all of them at once ...

export type Expr  = Literal | List | Sym | Callable

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
    export function Lambda (params : List, body : Expr, env : Env) : Lambda {
        return { type : 'LAMBDA', params, body, env }
    }

    export function Sym (ident : string) : Sym { return { type : 'SYM',  ident } }
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
        case  isNaN(Number(token)) : return AST.Sym(token);
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
            return ListUtil.make( ...sexpr.map(buildTree) )
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
        case 'SYM'    : return expr.ident;
        case 'LAMBDA' : return `(lambda ${format(expr.params)} ${format(expr.body)})`;
        case 'NATIVE' : return `(native ${format(expr.params)} #:native)`;
        case 'FEXPR'  : return `(fexpr ${format(expr.params)} @:fexpr)`;
        default:
            return 'XXX'
        }
    }

}

// -----------------------------------------------------------------------------

export type MaybeEnv = Env | undefined;

export class Env {
    public parent   : MaybeEnv;
    public bindings : Map<string, Expr> = new Map<string, Expr>();

    constructor (parent : MaybeEnv = undefined) {
        this.parent = parent;
    }

    lookup (name : Sym) : Expr {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (name : Sym, value : Expr) : void {
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
// Builtin Helpers
// -----------------------------------------------------------------------------

function assertSym (v : any) : asserts v is Sym {
    if (v.type != 'SYM') throw new Error(`Expected Sym and got ${JSON.stringify(v)}`);
}

function assertList (v : any) : asserts v is List {
    if (v.type != 'CONS' && v.type != 'NIL')
        throw new Error(`Expected List and got ${JSON.stringify(v)}`);
}

function assertCallable (v : any) : asserts v is Callable {
    if (!isCallable(v)) throw new Error(`Expected Callable and got ${JSON.stringify(v)}`);
}

function isCallable (v : any) : v is Callable {
    return v.type == 'FEXPR' || v.type == 'NATIVE' || v.type == 'LAMBDA'
}

function assertLiteral (v : any) : asserts v is Literal {
    if (v.type != 'NUM' && v.type != 'STR' && v.type != 'BOOL')
        throw new Error(`Expected Literal and got ${JSON.stringify(v)}`);
}

function assertNum (v : any) : asserts v is Num {
    if (v.type != 'NUM') throw new Error(`Expected Num and got ${JSON.stringify(v)}`);
}

function assertBool (v : any) : asserts v is Bool {
    if (v.type != 'BOOL') throw new Error(`Expected Bool and got ${JSON.stringify(v)}`);
}

// ...

type Predicate = (lhs : string | number | boolean, rhs : string | number | boolean) => boolean
type NumBinOp  = (lhs : number, rhs : number) => number

function liftPredicate (pred : Predicate) : Native {
    return AST.Native(
        ListUtil.make( AST.Sym('n'), AST.Sym('m') ),
        (env: Env) : Expr => {
            let lhs = env.lookup(AST.Sym('n'));
            let rhs = env.lookup(AST.Sym('m'));
            assertLiteral(lhs);
            assertLiteral(rhs);
            return pred(lhs.value, rhs.value) ? AST.True() : AST.False();
        }
    )
}

function liftNumBinOp (binop : NumBinOp) : Native {
    return AST.Native(
        ListUtil.make( AST.Sym('n'), AST.Sym('m') ),
        (env: Env) : Expr => {
            let lhs = env.lookup(AST.Sym('n'));
            let rhs = env.lookup(AST.Sym('m'));
            assertNum(lhs);
            assertNum(rhs);
            return AST.Num( binop(lhs.value, rhs.value) );
        }
    )
}

// -----------------------------------------------------------------------------

/*

# The two data structures
$cxs  = [ [$expr, $scope], ... ]  # Context stack (C+E)
$opq  = [ [OP, @args], ... ]       # Operation queue (K)

# The execution loop
while ($opq->[-1][0] ne 'HOST') {
  my ($op, @args) = @{pop @$opq};
  $step_func{$op}->($cxs, $opq, @args);
}

*/

interface Context {
    expr_stack : Expr[];
    scope      : Env;

    evaluate(expr : Expr) : Expr;
    derive() : Context;
}


type Operation = [ string, Expr[] ]


class Machine {

    run (expr : Expr, env : Env) : void {

        let queue : Operation[] = [
            [ 'HALT', [] ],
            [ 'EVAL', [ expr ] ]
        ];

        while (queue.length > 0) {
            console.log('__ TICK '+('_'.repeat(72)));

            let [ op, exprs ] = queue.pop() as Operation;

            let caller = queue.at(-1) as Operation;

            switch (op) {
            case 'EVAL':
                let [ expr, ...rest ] = exprs;
                queue.push(this.evaluate( expr as Expr, env ));
                if (rest.length > 0) queue.push([ 'JUST', rest ])
                break;
            case 'APPLY':
                let [ call, ...args ] = exprs;
                if (isCallable(call)) {
                    queue.push( this.apply( call as Native, args, env ) );
                } else {
                    queue.push([ 'JUST', exprs ]);
                }
                break;
            case 'CALL':
                let cons = exprs.at(0) as Cons;
                queue.push(
                    [ 'APPLY', [] ],
                    [ 'EVAL',  [ cons.head ] ],
                    [ 'EVAL',  [ cons.tail ] ],
                );
                break;
            case 'JUST':
                caller[1].push( ...exprs );
                break;
            }

            console.log('='.repeat(80));
            console.log(` %ENV :`, env.DUMP());
            console.log('QUEUE :', queue.map(([op, args]) => `${op}[${
                args.map(Parser.format).join(', ')
            }]`).join('; '));
            console.log('-'.repeat(80));
        }

    }


    evaluate (expr : Expr, env : Env) : Operation {
        console.log(`>> EVAL [${expr.type}]`, Parser.format(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  :
            return [ 'JUST', [ expr ] ];
        case 'SYM'   :
            return [ 'JUST', [ env.lookup(expr) ] ];
        case 'CONS'  :
            return [ 'CALL', [ expr ] ]
        case 'NIL'   :
            return [ 'NULL', [ expr ] ]
        default:
            throw new Error('FUCK!');
        }
    }

    apply (call : Callable, args : Expr[], env : Env) : Operation {
        console.log(`>> APPLY ${Parser.format(call)} -> `, args.map(Parser.format));

        const makeLocalEnv = () => {
            let params = ListUtil.flatten(call.params);
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                let arg   = args[i];
                assertSym(param);
                if (arg == undefined) throw new Error('BAD ARG!');
                localE.assign( param, arg as Expr );
            }
            console.log(`(local) %ENV :`, localE.DUMP());
            return localE;
        }

        switch (call.type) {
        case 'FEXPR':
            return [ 'EVAL', [ call.body( args, env ) ] ];
        case 'NATIVE':
            return [ 'JUST', [ call.body( makeLocalEnv() ) ] ];
        case 'LAMBDA':
            return [ 'CALL', [ call.body ] ]; // BROKEN!
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    createRootEnvironment () : Env {
        let env = new Env();

        env.assign( AST.Sym('=='), liftPredicate((n, m) => n == m));
        env.assign( AST.Sym('!='), liftPredicate((n, m) => n != m));

        env.assign( AST.Sym('>'),  liftPredicate((n, m) => n >  m));
        env.assign( AST.Sym('>='), liftPredicate((n, m) => n >= m));
        env.assign( AST.Sym('<='), liftPredicate((n, m) => n <= m));
        env.assign( AST.Sym('<'),  liftPredicate((n, m) => n <  m));

        env.assign( AST.Sym('+'),  liftNumBinOp((n, m) => n + m));
        env.assign( AST.Sym('-'),  liftNumBinOp((n, m) => n - m));
        env.assign( AST.Sym('*'),  liftNumBinOp((n, m) => n * m));
        env.assign( AST.Sym('/'),  liftNumBinOp((n, m) => n / m));
        env.assign( AST.Sym('%'),  liftNumBinOp((n, m) => n % m));

        return env;
    }
}

let ast = Parser.parse(`(+ 10 20)`);

//console.log(JSON.stringify(ast, null, 4));
console.log(Parser.format(ast));

let m   = new Machine();
let env = m.createRootEnvironment();

m.run( ast, env );



class Interpreter {

    evaluate (expr : Expr, env : Env) : Expr {
        console.log(`%ENV `, env.DUMP());
        console.log(`EVAL (${expr.type})`, Parser.format(expr));
        switch (expr.type) {
        case 'NUM'   :
        case 'STR'   :
        case 'BOOL'  : return expr;
        case 'SYM'   : return env.lookup(expr);
        case 'NIL'   : return expr;
        case 'CONS'  :
            let head = this.evaluate(expr.head, env);
            if (isCallable(head)) {
                return this.apply( head, expr.tail, env );
            } else {
                return AST.Cons( head, this.evaluate(expr.tail, env) as List );
            }
        default:
            throw new Error('WTF!');
        }
    }

    // apply a function, builtin or fexpr
    apply (call : Callable, args : List, env : Env) : Expr {
        console.log(`APPLY ${Parser.format(call)} -> `, Parser.format(args));

        const evalArgs     = () => ListUtil.flatten( this.evaluate( args, env ) as List );
        const makeLocalEnv = () => {
            let params = ListUtil.flatten(call.params);
            let args   = evalArgs();
            let localE = env.derive();
            for (let i = 0; i < params.length; i++) {
                let param = params[i];
                assertSym(param);
                // FIXME ...
                let arg = args[i] as Expr;
                localE.assign( param, arg );
            }
            return localE;
        }

        switch (call.type) {
        case 'FEXPR':
            return call.body( ListUtil.flatten( args ), env );
        case 'NATIVE':
            return call.body( makeLocalEnv() );
        case 'LAMBDA':
            return this.evaluate( call.body, makeLocalEnv() );
        default:
            throw new Error(`Unknown Callable Type`);
        }
    }

    createRootEnvironment () : Env {
        let env = new Env();

        env.assign( AST.Sym('lambda'), AST.FExpr(
            ListUtil.make( AST.Sym('params'), AST.Sym('body') ),
            (args : Expr[], env : Env) : Expr => {
                let [ params, body ] = args;
                assertList(params);
                assertList(body);
                return AST.Lambda( params as List, body as Expr, env );
            }
        ));

        env.assign( AST.Sym('if'), AST.FExpr(
            ListUtil.make( AST.Sym('cond'), AST.Sym('then'), AST.Sym('else') ),
            (args : Expr[], env : Env) : Expr => {
                let [ cond, thenBranch, elseBranch ] = args;
                assertList(cond);
                assertList(thenBranch);
                assertList(elseBranch);
                let result = this.evaluate( cond as Expr, env );
                assertBool(result);
                return this.evaluate( result.value ? elseBranch : thenBranch, env );
            }
        ));

        env.assign( AST.Sym('=='), liftPredicate((n, m) => n == m));
        env.assign( AST.Sym('!='), liftPredicate((n, m) => n != m));

        env.assign( AST.Sym('>'),  liftPredicate((n, m) => n >  m));
        env.assign( AST.Sym('>='), liftPredicate((n, m) => n >= m));
        env.assign( AST.Sym('<='), liftPredicate((n, m) => n <= m));
        env.assign( AST.Sym('<'),  liftPredicate((n, m) => n <  m));

        env.assign( AST.Sym('+'),  liftNumBinOp((n, m) => n + m));
        env.assign( AST.Sym('-'),  liftNumBinOp((n, m) => n - m));
        env.assign( AST.Sym('*'),  liftNumBinOp((n, m) => n * m));
        env.assign( AST.Sym('/'),  liftNumBinOp((n, m) => n / m));
        env.assign( AST.Sym('%'),  liftNumBinOp((n, m) => n % m));

        return env;
    }
}

// -----------------------------------------------------------------------------
/*

let ast = Parser.parse(`((lambda (x y) (+ x y)) 10 20)`);

//console.log(JSON.stringify(ast, null, 4));
console.log(Parser.format(ast));

let i   = new Interpreter();
let env = i.createRootEnvironment();
let got = i.evaluate( ast, env );

//console.log(JSON.stringify(got, null, 4));
console.log(Parser.format(got));

*/
