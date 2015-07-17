    NAME        COL033-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 l: -> ANY
                 m: -> ANY
    ORDERING    LPO
                apply > b > l > m
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(l,X),Y) = apply(X,apply(Y,Y)) 
                  apply(m,X) = apply(X,X) 
    CONCLUSION