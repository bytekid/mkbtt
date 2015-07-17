    NAME        COL007-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 l: -> ANY
    ORDERING    LPO
                apply > l
    VARIABLES  X,Y: ANY
    EQUATIONS   apply(apply(l,X),Y) = apply(X,apply(Y,Y)) 
    CONCLUSION