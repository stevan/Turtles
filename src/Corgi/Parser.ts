
import * as AST      from './AST'
import * as ASTUtil  from './ASTUtil'
import * as TypeUtil from './TypeUtil'
import * as ListUtil from './ListUtil'

// -----------------------------------------------------------------------------
// Formatter
// -----------------------------------------------------------------------------

export function format (expr : AST.Expr) : string {
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

export function parse ( src : string ) : AST.Expr {
    let [ sexpr, rest ] = parseTokens( tokenizer( src ) );
    return buildTree( sexpr );
}

type SExpr = AST.Expr | SExpr[];

function tokenizer ( src : string ) : string[] {
    return src.replace(/\(/g, ' ( ')
              .replace(/\)/g, ' ) ')
              .trim()
              .split(/\s+/)
              .filter(Boolean);
}

function lexer ( token : string ) : AST.Expr {
    switch (true) {
    case token == 'true'  : return ASTUtil.True();
    case token == 'false' : return ASTUtil.False();
    case !isNaN(Number(token)) && Math.trunc(Number(token)) == Number(token) : return ASTUtil.Int(Number(token));
    case !isNaN(Number(token)) && Math.trunc(Number(token)) != Number(token) : return ASTUtil.Flt(Number(token));
    case  isNaN(Number(token)) : return ASTUtil.Word(token);
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

function buildTree ( sexpr : SExpr ) : AST.Expr {
    if (Array.isArray(sexpr)) {
        return ListUtil.create( ...sexpr.map(buildTree) )
    } else {
        TypeUtil.assertExpr(sexpr)
        return sexpr;
    }
}
