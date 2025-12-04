
import * as Core        from './Core'
import * as Environment from './Environment'

// -----------------------------------------------------------------------------
// Parser
// -----------------------------------------------------------------------------

type ParseExpr = Core.Term | ParseExpr[];

export function parse (source : string) : Core.Term {

    const SPLITTER = /'(?:[^'\\]|\\.)*'|[()]|[^\s()']+/g

    const tokenize = (src : string) : string[] => src.match(SPLITTER) ?? [];

    const parseTokens = (tokens : string[]) : [ ParseExpr, string[] ] => {
        let token = tokens[0];
        if (token == undefined) throw new Error('Undefined Token');
        let rest = tokens.slice(1);
        if (token == '(') return parseList( rest, [] );
        switch (true) {
        case token == 'true'       : return [ Core.True(),             rest ];
        case token == 'false'      : return [ Core.False(),            rest ];
        case !isNaN(Number(token)) : return [ Core.Num(Number(token)), rest ];
        case token.startsWith('"') : return [ Core.Str(token),         rest ];
        default                    : return [ Core.Sym(token),         rest ];
        }
    }

    const parseList = (tokens : string[], acc : ParseExpr[]) : [ ParseExpr[], string[] ] => {
        if (tokens[0] === ')') return [ acc, tokens.slice(1) ];
        let [ expr, remaining ] = parseTokens( tokens );
        return parseList( remaining, [ ...acc, expr ] );
    }

    const buildTree = (expr : ParseExpr) : Core.Term  =>
        (Array.isArray(expr)) ? Core.makeList( ...expr.map(buildTree) ) : expr;

    let [ expr, rest ] = parseTokens( tokenize( source ) );
    return buildTree( expr );
}

export function deparse (t : Core.Term) : string {
    switch (true) {
    case Core.isNil(t)   : return '()';
    case Core.isTrue(t)  : return 'true';
    case Core.isFalse(t) : return 'false';
    case Core.isSym(t)   : return '`'+t.value;
    case Core.isStr(t)   : return `"${t.value}"`;
    case Core.isNum(t)   : return t.value.toString();
    case Core.isPair(t)  :
        if (Core.isList(t)) {
            if (Core.isNil(t.snd)) {
                return `(${deparse(t.fst)})`;
            } else {
                return `(${deparse(t.fst)}::${deparse(t.snd)})`;
            }
        } else {
            return `(${deparse(t.fst)} * ${deparse(t.snd)})`;
        }
    case Core.isLambda(t)  : return `(λ ${deparse(t.param)} . ${deparse(t.body)})`
    case Core.isClosure(t) : return `[ (${Environment.showLocalEnv(t.env)}) ~ ${deparse(t.abs)} ]`
    case Core.isNative(t)  : return `&:native`
    case Core.isFExpr(t)   : return `@:fexpr`
    default:
        throw new Error('Cannot show a non-Term');
    }
}

// -----------------------------------------------------------------------------
