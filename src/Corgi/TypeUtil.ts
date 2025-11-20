
import * as AST from './AST'

export function isWord       (v : any) : v is AST.Word       { return v.type == 'WORD' }
export function isVar        (v : any) : v is AST.Var        { return v.type == 'VAR'  }
export function isNum        (v : any) : v is AST.Num        { return v.type == 'NUM'  }
export function isStr        (v : any) : v is AST.Str        { return v.type == 'STR'  }
export function isNil        (v : any) : v is AST.Nil        { return v.type == 'NIL'  }
export function isCons       (v : any) : v is AST.Cons       { return v.type == 'CONS' }
export function isTrue       (v : any) : v is AST.True       { return v.type == '*T*'  }
export function isFalse      (v : any) : v is AST.False      { return v.type == '*F*'  }
export function isFExpr      (v : any) : v is AST.FExpr      { return v.type == 'FEXPR'   }
export function isNative     (v : any) : v is AST.Native     { return v.type == 'NATIVE'  }
export function isLambda     (v : any) : v is AST.Lambda     { return v.type == 'LAMBDA'  }
export function isSpecial    (v : any) : v is AST.Special    { return v.type == 'SPECIAL' }
export function isList       (v : any) : v is AST.List       { return isCons(v)   || isNil(v)   }
export function isBool       (v : any) : v is AST.Bool       { return isTrue(v)   || isFalse(v) }
export function isCallable   (v : any) : v is AST.Callable   { return isNative(v) || isLambda(v)  || isFExpr(v)   }
export function isIdentifier (v : any) : v is AST.Identifier { return isWord(v)   || isVar(v)     || isSpecial(v) }
export function isLiteral    (v : any) : v is AST.Literal    { return isNum(v)    || isStr(v)     || isBool(v)    }
export function isExpr       (v : any) : v is AST.Expr       { return isList(v)   || isLiteral(v) || isCallable(v) || isIdentifier(v) }

export function assertNum        (v : any) : asserts v is AST.Num        { if (!isNum(v))        throw new Error("Not Num")        }
export function assertStr        (v : any) : asserts v is AST.Str        { if (!isStr(v))        throw new Error("Not Str")        }
export function assertTrue       (v : any) : asserts v is AST.True       { if (!isTrue(v))       throw new Error("Not True")       }
export function assertFalse      (v : any) : asserts v is AST.False      { if (!isFalse(v))      throw new Error("Not False")      }
export function assertBool       (v : any) : asserts v is AST.Bool       { if (!isBool(v))       throw new Error("Not Bool")       }
export function assertNil        (v : any) : asserts v is AST.Nil        { if (!isNil(v))        throw new Error("Not Nil")        }
export function assertCons       (v : any) : asserts v is AST.Cons       { if (!isCons(v))       throw new Error("Not Cons")       }
export function assertList       (v : any) : asserts v is AST.List       { if (!isList(v))       throw new Error("Not List")       }
export function assertFExpr      (v : any) : asserts v is AST.FExpr      { if (!isFExpr(v))      throw new Error("Not FExpr")      }
export function assertLambda     (v : any) : asserts v is AST.Lambda     { if (!isLambda(v))     throw new Error("Not Lambda")     }
export function assertNative     (v : any) : asserts v is AST.Native     { if (!isNative(v))     throw new Error("Not Native")     }
export function assertSpecial    (v : any) : asserts v is AST.Special    { if (!isSpecial(v))    throw new Error("Not Special")    }
export function assertVar        (v : any) : asserts v is AST.Var        { if (!isVar(v))        throw new Error("Not Var")        }
export function assertWord       (v : any) : asserts v is AST.Word       { if (!isWord(v))       throw new Error("Not Word")       }
export function assertCallable   (v : any) : asserts v is AST.Callable   { if (!isCallable(v))   throw new Error("Not Callable")   }
export function assertIdentifier (v : any) : asserts v is AST.Identifier { if (!isIdentifier(v)) throw new Error("Not Identifier") }
export function assertLiteral    (v : any) : asserts v is AST.Literal    { if (!isLiteral(v))    throw new Error("Not Literal")    }
export function assertExpr       (v : any) : asserts v is AST.Expr       { if (!isExpr(v))       throw new Error("Not Expr")       }
