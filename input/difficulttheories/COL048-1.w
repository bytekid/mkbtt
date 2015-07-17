    NAME        COL048-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 w: -> ANY
                 m: -> ANY
    ORDERING    LPO
                apply > b > w > m
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(w,X),Y) = apply(apply(X,Y),Y) 
                  apply(m,X) = apply(X,X) 
    CONCLUSION