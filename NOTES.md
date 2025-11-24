<!----------------------------------------------------------------------------->
# NOTES
<!----------------------------------------------------------------------------->



```lisp
(((lambda (x)
    (lambda (y)
        (lambda (z)
            (+ x (* y z))
        )
    )
) 5) 2)

```
- prints: (lambda (z) (+ x:[5] (* y:[2] z)))







## Context

- create Context type
    - Interpreter/Machine has context stack 
    - holds Env, and ref to Interpreter/Machine
        - perhaps it should also know it's postion on the context stack??
    - holds pointer to Op queue start position 
        - perhaps also end?? 
    - methods ...
        - eval(expr) for runtime evaluating
        - derive() for making a new Env

## FExprs        

- what should they return??
    - in the Interpreter they return evaluated Expr
    - in the Machine they (will) return an Expr to evaluate
    
- how do they we evaluate Exprs during a FExprs application?
    - in the Interpreter it is just another call to evaluate(), no biggie
        - conditionals are handled in the native realm
    - in the Machine we might need to enqueue the EVAL
        - this means we will need a conditional operation??
            - simple ternary operator should work
    
<!----------------------------------------------------------------------------->
## Interpreter
<!----------------------------------------------------------------------------->

- calling `this.evaluate` in FExprs is ugly
    - hide it in the context (see above)

<!----------------------------------------------------------------------------->
## Machine
<!----------------------------------------------------------------------------->

- make Operation types better
    - formalize the patterns
    
- handle eval needs inside of FExprs
    - not entirely sure the best approach here

- check for FExprs in CALL? 
    - if so, bypass the tail EVAL
    

