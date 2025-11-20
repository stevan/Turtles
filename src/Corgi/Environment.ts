
import * as AST from './AST'

type MaybeEnvironment = Environment | undefined;

export class Environment {
    public parent   : MaybeEnvironment;
    public bindings : Map<AST.Ident, AST.Expr> = new Map<AST.Ident, AST.Expr>();

    constructor (parent : MaybeEnvironment = undefined) {
        this.parent = parent;
    }

    lookup (name : AST.Identifier) : AST.Expr {
        let result = this.bindings.get(name.ident) ?? this.parent?.lookup(name);
        if (result == undefined) throw new Error(`Unable to find ${name.type}(${name.ident}) in E`);
        return result;
    }

    assign (key : AST.Identifier, val : AST.Expr) : void {
        this.bindings.set(key.ident, val);
    }

    derive () : Environment { return new Environment( this ) }
    depth  () : number      { return 1 + (this.parent?.depth() ?? 0) }

    DUMP () : string {
        return `%E[${this.depth()}]{${ [ ...this.bindings.keys() ].map((e) => ("`" + e)).join(', ') }} `
            + (this.parent == undefined ? '' : ` ^(${ this.parent?.DUMP() })`)
    }
}
