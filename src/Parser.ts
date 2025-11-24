
import * as Types from './Types'
import * as AST   from './AST'
import * as Util  from './Util'

type SExpr = Types.Expr | SExpr[];

export function parse (src : string) : Types.Expr {
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

function lexer (token : string) : Types.Expr {
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

function buildTree (sexpr : SExpr) : Types.Expr {
    if (Array.isArray(sexpr)) {
        return Util.List.make( ...sexpr.map(buildTree) )
    } else {
        return sexpr;
    }
}

export function format (expr : Types.Expr) : string {
    switch (expr.type) {
    case 'BOOL'   : return expr.value ? 'true' : 'false';
    case 'NUM'    : return expr.value.toString();
    case 'STR'    : return `"${expr.value}"`;
    case 'NIL'    : return '()';
    case 'CONS'   : return `(${ Util.List.flatten(expr).map(format).join(' ') })`;
    case 'SYM'    : return expr.ident;
    case 'LAMBDA' : return `(lambda ${format(expr.params)} ${format(expr.body)})`;
    case 'NATIVE' : return `(#:native ${format(expr.params)})`;
    case 'FEXPR'  : return `(@:fexpr ${format(expr.params)})`;
    case 'COND'   : return `?(${[ expr.cond, expr.ifTrue, expr.ifFalse ].map(format).join(' ') })`;
    default:
        return 'XXX'
    }
}
