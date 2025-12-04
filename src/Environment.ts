
import * as Core   from './Core'
import * as Parser from './Parser'

// -----------------------------------------------------------------------------
// Environment
// -----------------------------------------------------------------------------

export function showEnv (env : Core.Term) : string {
    if (Core.isNil(env))       return '';
    if (!Core.isPair(env))     throw new Error(`showEnv(env) env must be a pair not ${env.kind}`);
    if (!Core.isPair(env.fst)) throw new Error(`Expected pair in Env not ${env.fst.kind}`);
    let curr = env.fst;
    return `${Parser.deparse(curr.fst)}:${curr.snd.kind.substr(0, 4).toLowerCase()} ${showEnv( env.snd )}`
}

export function showLocalEnv (env : Core.Term) : string {
    if (Core.isNil(env))       return '';
    if (!Core.isPair(env))     throw new Error(`showEnv(env) env must be a pair not ${env.kind}`);
    if (!Core.isPair(env.fst)) throw new Error(`Expected pair in Env not ${env.fst.kind}`);
    let curr = env.fst;
    if (Core.isNative(curr.snd) || Core.isFExpr(curr.snd)) return '%builtins';
    return `${Parser.deparse(curr.fst)}:${curr.snd.kind.substr(0, 4).toLowerCase()} ${showLocalEnv( env.snd )}`
}

export function lookup (t : Core.Term, env : Core.Term, depth : number = 0) : Core.Term {
    let indent = '  '.repeat(depth);
    if (Core.isNil(env))       throw new Error(`Cannot find ${Parser.deparse(t)} in an empty Env`);
    if (!Core.isPair(env))     throw new Error(`lookup(t, env) env must be a pair not ${env.kind}`);
    if (!Core.isPair(env.fst)) throw new Error(`Expected pair in Env not ${env.fst.kind}`);
    if (!Core.isSym(t))        throw new Error(`lookup(t, env) t must be a symbol not ${t.kind}`);
    console.log(`${indent} /lookup/ ? ${Parser.deparse(t)} IN ${showLocalEnv(env)}`);
    let curr = env.fst;
    console.log(`${indent} /lookup/ ? ${Parser.deparse(t)} EQ ${Parser.deparse(curr.fst)}`);
    if (Core.equalTo(t, curr.fst)) {
        console.log(`${indent} !lookup! @ ${Parser.deparse(t)} => ${Parser.deparse(curr.snd)}`);
        return curr.snd;
    }
    return lookup( t, env.snd, depth + 1 );
}

export function define (sym : Core.Term, t : Core.Term, env : Core.Term) : Core.Term {
    if (!Core.isSym(sym)) throw new Error(`define(sym, t, env) sym must be a symbol not ${t.kind}`);
    return Core.Env( sym, t, env );
}

// -----------------------------------------------------------------------------
