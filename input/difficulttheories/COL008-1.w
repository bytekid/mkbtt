    NAME        COL008-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 m: -> ANY
    ORDERING    LPO
                apply > b > m
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(m,X) = apply(X,X) 
    CONCLUSION