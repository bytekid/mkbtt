    NAME        COL010-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 s2: -> ANY
    ORDERING    LPO
                apply > b > s2
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(apply(s2,X),Y),Z) = apply(apply(X,Z),apply(Y,Y)) 
    CONCLUSION