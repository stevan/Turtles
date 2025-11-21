
import * as AST from './AST'

export type MaybeEnvironment = Environment | undefined;

export class Environment implements AST.Env {
    public parent   : MaybeEnvironment;
    public bindings : Map<AST.Ident, AST.Value> = new Map<AST.Ident, AST.Value>();

    constructor (parent : MaybeEnvironment = undefined) {
        this.parent = parent;
    }

    lookup (name : AST.Identifier) : AST.Value {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (name : AST.Identifier, value : AST.Value) : void {
        this.bindings.set(name.ident, value);
    }

    derive () : Environment { return new Environment( this ) }
    depth  () : number      { return 1 + (this.parent?.depth() ?? 0) }

    DUMP () : string {
        return `%E[${this.depth()}]{${ [ ...this.bindings.keys() ].map((e) => ("`" + e)).join(', ') }} `
            + (this.parent == undefined ? '' : ` ^(${ this.parent?.DUMP() })`)
    }
}
