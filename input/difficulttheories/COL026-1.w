    NAME        COL026-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 b: -> ANY
                 w1: -> ANY
    ORDERING    LPO
                apply > b > w1
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(apply(b,X),Y),Z) = apply(X,apply(Y,Z)) 
                  apply(apply(w1,X),Y) = apply(apply(Y,X),X) 
    CONCLUSION