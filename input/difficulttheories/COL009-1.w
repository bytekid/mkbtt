    NAME        COL009-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 l2: -> ANY
    ORDERING    LPO
                apply > b > l2
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(l2,X),Y) = apply(Y,apply(X,X)) 
    CONCLUSION