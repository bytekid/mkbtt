    NAME        COL063-3
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 t: -> ANY
    ORDERING    LPO
                apply > b > t
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(t,X),Y) = apply(Y,X) 
    CONCLUSION