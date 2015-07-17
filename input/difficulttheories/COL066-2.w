    NAME        COL066-2
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 q: -> ANY
                 w: -> ANY
    ORDERING    LPO
                apply > b > q > w
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(q,X),Y),Z) = apply(Y,apply(X,Z)) 
                  apply(apply(w,X),Y) = apply(apply(X,Y),Y) 
    CONCLUSION