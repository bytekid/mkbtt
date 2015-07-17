    NAME        COL070-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 n1: -> ANY
                 b: -> ANY
    ORDERING    LPO
                apply > n1 > b
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(n1,X),Y),Z) = apply(apply(apply(X,Y),Y),Z) 
                  apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
    CONCLUSION