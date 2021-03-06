    NAME        COL035-1
    MODE        COMPLETION
    SORTS       ANY
    SIGNATURE   apply: ANY ANY -> ANY
                 l: -> ANY
                 w: -> ANY
                 q: -> ANY
    ORDERING    LPO
                apply > l > w > q
    VARIABLES  X,Y,Z: ANY
    EQUATIONS   apply(apply(l,X),Y) = apply(X,apply(Y,Y)) 
                  apply(apply(w,X),Y) = apply(apply(X,Y),Y) 
                  apply(apply(apply(q,X),Y),Z) = apply(Y,apply(X,Z)) 
    CONCLUSION