    NAME        COL003-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 w: -> ANY
    ORDERING    LPO
                apply > b > w
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(w,X),Y) = apply(apply(X,Y),Y) 
    CONCLUSION