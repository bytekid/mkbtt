    NAME        COL030-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 s: -> ANY
                 l: -> ANY
    ORDERING    LPO
                apply > s > l
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(s,X),Y),Z) = apply(apply(X,Z),apply(Y,Z)) 
                  apply(apply(l,X),Y) = apply(X,apply(Y,Y)) 
    CONCLUSION