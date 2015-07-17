    NAME        COL032-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 m: -> ANY
                 q: -> ANY
    ORDERING    LPO
                apply > m > q
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(m,X) = apply(X,X) 
                  apply(apply(apply(q,X),Y),Z) = apply(Y,apply(X,Z)) 
    CONCLUSION