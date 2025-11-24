
import * as Types from './Types'

export type MaybeEnv = Env | undefined;

export class Env {
    public parent   : MaybeEnv;
    public bindings : Map<string, Types.Expr> = new Map<string, Types.Expr>();

    constructor (parent : MaybeEnv = undefined) {
        this.parent = parent;
    }

    exists (name : Types.Sym) : boolean {
        return this.bindings.has(name.ident) || (this.parent?.exists(name) ?? false);
    }

    lookup (name : Types.Sym) : Types.Expr {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (name : Types.Sym, value : Types.Expr) : void {
        this.bindings.set(name.ident, value);
    }

    derive () : Env    { return new Env( this ) }
    depth  () : number { return (this.parent?.depth() ?? -1) + 1 }
}
